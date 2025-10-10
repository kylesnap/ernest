test_that("Clustering works", {
  # Set parameters
  set.seed(42)

  # Generate random data for three clusters
  data_1 <- c(0.25, 0.25, 0.25) + uniformly::runif_in_sphere(50, 3, r = 0.1)
  data_2 <- c(0.75, 0.75, 0.75) + uniformly::runif_in_sphere(250, 3, r = 0.1)

  # Stack all data together into a larger 2D array
  data_all <- rbind(data_1, data_2)

  # Display the data
  MultiBoundingEllipsoids(data_all, 1)
})
