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

Altered by K Dewsnap (removing unused routines/non-R rng)
Date: October 8, 2025
*/
// nocov start
#include "KMeansRexCore.h"

using namespace kmeans_rex;

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

// Overload: accepts Eigen::Ref<Matrix> and Eigen::Ref<Vector>
void kmeans_rex::RunKMeans(const Eigen::Ref<const Eigen::MatrixXd> X_IN, int K, int Niter,
                           Eigen::Ref<Eigen::MatrixXd> Mu_OUT,
                           Eigen::Ref<Eigen::VectorXd> Z_OUT) {
  int N = X_IN.rows();
  int D = X_IN.cols();
  const char* initname = "plusplus";

  ExtMat X(const_cast<double*>(X_IN.data()), N, D);
  ExtMat Mu(Mu_OUT.data(), K, D);
  ExtMat Z(Z_OUT.data(), N, 1);

  run_lloyd(X, Mu, Z, Niter);
}
// nocov end