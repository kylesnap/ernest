/* KMeansRexCore.cpp
This file includes code modfied under the following license:
BSD 3-clause license for open-source software.

Copyright (c) 2013-2015, Michael C. Hughes
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are
permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of
conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of
conditions and the following disclaimer in the documentation and/or other materials
provided with the distribution.

3. Neither the name of the copyright holder nor the names of its contributors may be used
to endorse or promote products derived from this software without specific prior written
permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. LICENSED UNDER THE BSD
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
  double prevDist = R_PosInf, totalDist = 0;
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

  ExtMat X(const_cast<double*>(X_IN.data()), N, D);
  ExtMat Mu(Mu_OUT.data(), K, D);
  ExtMat Z(Z_OUT.data(), N, 1);

  run_lloyd(X, Mu, Z, Niter);
}
// nocov end
