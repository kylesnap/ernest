/* KMeansRexCore.cpp
A fast, easy-to-read implementation of the K-Means clustering algorithm.
allowing customized initialization (random samples or plus plus)
and vectorized execution via the Eigen matrix template library.

Intended to be compiled as a shared library which can then be utilized
from high-level interactive environments, such as Matlab or Python.

Contains
--------
Utility Fcns
* discrete_rand : sampling discrete random variable
* select_without_replacement : sample without replacement

Cluster Location Mu Initialization:
* sampleRowsRandom : sample rows of X at random (w/out replacement)
* sampleRowsPlusPlus : sample rows of X via kmeans++ procedure of Arthur et al.
    see http://en.wikipedia.org/wiki/K-means%2B%2B

K-Means Algorithm (aka Lloyd's Algorithm)
* run_lloyd : executes lloyd for specfied number of iterations

External "C" function interfaces (for calling from Python)
* RunKMeans          : compute cluster centers and assignments via lloyd
* SampleRowsPlusPlus : get just a plusplus initialization

Author: Mike Hughes (www.michaelchughes.com)
Date:   2 April 2013

Altered by K Dewsnap (using R-based RNG)
Date: October 8, 2025
*/

#include "KMeansRexCore.h"

using namespace kmeans_rex;

// ====================================================== Utility Functions
/*
 * Return random integers from `low` (inclusive) to `high` (exclusive).
 */
int randint(int low, int high) {
  double r = R_unif_index(high - low);
  int rint = static_cast<int>(r);
  return rint + low;
}

int discrete_rand(Vec& p) {
  double total = p.sum();
  int K = (int)p.size();

  double r = total * unif_rand();
  double cursum = p(0);
  int newk = 0;
  while (r >= cursum && newk < K - 1) {
    newk++;
    cursum += p[newk];
  }
  if (newk < 0 || newk >= K) {
    cerr << "Badness. Chose illegal discrete value." << endl;
    return -1;
  }
  return newk;
}

void select_without_replacement(int N, int K, Vec& chosenIDs) {
  Vec p = Vec::Ones(N);
  for (int kk = 0; kk < K; kk++) {
    int choice;
    int doKeep = false;
    while (doKeep == false) {
      doKeep = true;
      choice = discrete_rand(p);

      for (int previd = 0; previd < kk; previd++) {
        if (chosenIDs[previd] == choice) {
          doKeep = false;
          break;
        }
      }
    }
    chosenIDs[kk] = choice;
  }
}

// ======================================================= Init Cluster Locs Mu

void sampleRowsRandom(ExtMat& X, ExtMat& Mu) {
  int N = X.rows();
  int K = Mu.rows();
  Vec ChosenIDs = Vec::Zero(K);
  select_without_replacement(N, K, ChosenIDs);
  for (int kk = 0; kk < K; kk++) {
    Mu.row(kk) = X.row(ChosenIDs[kk]);
  }
}

void sampleRowsPlusPlus(ExtMat& X, ExtMat& Mu) {
  int N = X.rows();
  int K = Mu.rows();
  if (K > N) {
    // User requested more clusters than we have available.
    // So, we'll fill only first N rows of Mu
    // and leave all remaining rows of Mu uninitialized.
    K = N;
  }
  int choice = randint(0, N);
  Mu.row(0) = X.row(choice);
  Vec minDist(N);
  Vec curDist(N);
  for (int kk = 1; kk < K; kk++) {
    curDist = (X.rowwise() - Mu.row(kk - 1)).square().rowwise().sum();
    if (kk == 1) {
      minDist = curDist;
    } else {
      minDist = curDist.min(minDist);
    }
    choice = discrete_rand(minDist);
    Mu.row(kk) = X.row(choice);
  }
}

void init_Mu(ExtMat& X, ExtMat& Mu, const char* initname) {
  if (string(initname) == "random") {
    sampleRowsRandom(X, Mu);
  } else if (string(initname) == "plusplus") {
    sampleRowsPlusPlus(X, Mu);
  }
}

// ======================================================= Update Assignments Z
void pairwise_distance(ExtMat& X, ExtMat& Mu, Mat& Dist) {
  int D = X.cols();
  int K = Mu.rows();

  // For small dims D, for loop is noticeably faster than fully vectorized.
  // Odd but true.  So we do fastest thing
  if (D <= 16) {
    for (int kk = 0; kk < K; kk++) {
      Dist.col(kk) = (X.rowwise() - Mu.row(kk)).square().rowwise().sum();
    }
  } else {
    Dist = -2 * (X.matrix() * Mu.transpose().matrix());
    Dist.rowwise() += Mu.square().rowwise().sum().transpose().row(0);
  }
}

double assignClosest(ExtMat& X, ExtMat& Mu, ExtMat& Z, Mat& Dist) {
  double totalDist = 0;
  int minRowID;

  pairwise_distance(X, Mu, Dist);

  for (int nn = 0; nn < X.rows(); nn++) {
    totalDist += Dist.row(nn).minCoeff(&minRowID);
    Z(nn, 0) = minRowID;
  }
  return totalDist;
}

// ======================================================= Update Locations Mu
void calc_Mu(ExtMat& X, ExtMat& Mu, ExtMat& Z) {
  // Mu = Mat::Zero(Mu.rows(), Mu.cols());
  Mu.fill(0);
  Vec NperCluster = Vec::Zero(Mu.rows());
  for (int nn = 0; nn < X.rows(); nn++) {
    Mu.row((int)Z(nn, 0)) += X.row(nn);
    NperCluster[(int)Z(nn, 0)] += 1;
  }
  NperCluster += 1e-100;  // avoid division-by-zero
  for (int k = 0; k < Mu.rows(); k++) {
    Mu.row(k) /= NperCluster(k);
  }
}

// ======================================================= Overall Lloyd Alg.
void run_lloyd(ExtMat& X, ExtMat& Mu, ExtMat& Z, int Niter) {
  double prevDist, totalDist = 0;
  Mat Dist = Mat::Zero(X.rows(), Mu.rows());

  for (int iter = 0; iter < Niter; iter++) {
    totalDist = assignClosest(X, Mu, Z, Dist);
    calc_Mu(X, Mu, Z);
    if (prevDist == totalDist) {
      break;
    }
    prevDist = totalDist;
  }
}

// ===========================================================================
// ===========================================================================
// ===========================  EXTERNALLY CALLABLE FUNCTIONS ================
// ===========================================================================
// ===========================================================================

void kmeans_rex::RunKMeans(const double* X_IN, int N, int D, int K, int Niter,
                           const char* initname, double* Mu_OUT,
                           double* Z_OUT) {
  ern::RandomEngine rand;

  ExtMat X(const_cast<double*>(X_IN), N, D);
  ExtMat Mu(Mu_OUT, K, D);
  ExtMat Z(Z_OUT, N, 1);

  init_Mu(X, Mu, initname);
  run_lloyd(X, Mu, Z, Niter);
}

// Overload: accepts Eigen::Ref<Matrix> and Eigen::Ref<Vector>
void kmeans_rex::RunKMeans(const Eigen::Ref<const Eigen::MatrixXd> X_IN, int K,
                           int Niter, Eigen::Ref<Eigen::MatrixXd> Mu_OUT,
                           Eigen::Ref<Eigen::VectorXd> Z_OUT) {
  int N = X_IN.rows();
  int D = X_IN.cols();
  const char* initname = "plusplus";

  ExtMat X(const_cast<double*>(X_IN.data()), N, D);
  ExtMat Mu(Mu_OUT.data(), K, D);
  ExtMat Z(Z_OUT.data(), N, 1);

  init_Mu(X, Mu, initname);
  run_lloyd(X, Mu, Z, Niter);
}