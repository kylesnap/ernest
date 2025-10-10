#include <cmath>

#include "KMeansRexCore.h"
#include "bounding.h"

/**
 * @brief Find the bounding ellipsoid of the given points.
 * @param X A matrix of points.
 * @returns A list, containing the mean, covariance, transformation matrix,
 * and the log volume of the points, or just NULL if log volume is -Infy.
 */
[[cpp11::register]]
cpp11::list MultiBoundingEllipsoids(cpp11::doubles_matrix<> X,
                                    const double reduction) {
  Eigen::MatrixXd X_eigen = as_Matrix(X);
  std::vector<bounding::Ellipsoid> ellipsoids;
  bounding::FitMultiEllipsoids(X_eigen, ellipsoids, log(reduction));
  return cpp11::list();
}