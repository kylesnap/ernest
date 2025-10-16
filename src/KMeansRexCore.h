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

#include <iostream>

#include "random_generator.h"

namespace kmeans_rex {
using namespace Eigen;
using namespace std;

// DEFINE Custom Type Names to make code more readable.
typedef Map<ArrayXXd> ExtMat;
typedef ArrayXXd Mat;
typedef ArrayXd Vec;

// KMeans Interface.
void RunKMeans(const double *X_IN, int N, int D, int K, int Niter,
               const char *initname, double *Mu_OUT, double *Z_OUT);
void RunKMeans(const Eigen::Ref<const Eigen::MatrixXd> X_IN, int K, int Niter,
               Eigen::Ref<Eigen::MatrixXd> Mu_OUT,
               Eigen::Ref<Eigen::VectorXd> Z_OUT);
}  // namespace kmeans_rex