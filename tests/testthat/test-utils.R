# # test_that("validate_integer_parameter works correctly", {
# #   expect_equal(round_to_integer(5L, 10), 5L)
# #   expect_equal(round_to_integer(5.5, 10), 55L)
# #
# #   expect_equal(round_to_integer(0.5, 10), 5L)
# #   expect_error(round_to_integer("a", 10), "must be a number")
# # })
# #
# # test_that("check_unique_names detects duplicates", {
# #   expect_error(check_unique_names(c("a", "b", "a")), "duplicated: a")
# #   expect_silent(check_unique_names(c("a", "b", "c")))
# # })
# #
# # test_that("create_live generates live points correctly", {
# #   lrps <- rwcube_lrps$new(
# #     log_lik_fn = gaussian_2$log_lik,
# #     prior_fn = compile(ernest_prior(gaussian_2$prior_transform)),
# #     n_dim = 2L
# #   )
# #   result <- create_live(lrps, 10, 2)
# #   expect_equal(dim(result$unit), c(10, 2))
# #   expect_equal(dim(result$point), c(10, 2))
# #   expect_length(result$log_lik, 10)
# # })
# #

# #
# # test_that("which_minn works correctly", {
# #   x <- c(10, 20, 5, 15)
# #   expect_equal(which_minn(x, 1), 3)
# #   expect_equal(which_minn(x, 2), c(3, 1))
# # })
#
# test_that("compute_integral delivers expected results", {
#   expected <- readRDS(test_path("./compute_integral_test.rds"))
#   observed <- compute_integral(expected$log_lik, expected$log_vol)
#
#   expect_equal(observed$log_weight, expected$log_wt)
#   expect_equal(observed$log_evidence, expected$log_z)
#   expect_equal(observed$log_evidence_var, expected$log_z_var)
#   expect_equal(observed$information, expected$h)
# })
#
#
# test_that("simulate_volume returns correct length and type", {
#   points <- c(3, 2, 1, 13, 13, 12, 23, 22)
#   vol <- simulate_volume(points)
#   expect_type(vol, "double")
#   expect_length(vol, length(points))
# })
#
# test_that("simulate_volume handles all nondecreasing points (Beta contraction)", {
#   set.seed(123)
#   points <- rep(5, 10)
#   vol <- simulate_volume(points)
#   # All contractions should be from Beta(5, 1)
#   expect_true(all(is.finite(vol)))
#   expect_length(vol, 10)
# })
#
# test_that("simulate_volume handles all decreasing points (order statistics contraction)", {
#   set.seed(123)
#   points <- seq(10, 1, by = -1)
#   vol <- simulate_volume(points)
#   expect_true(all(is.finite(vol)))
#   expect_length(vol, 10)
# })
#
# test_that("simulate_volume is reproducible with set.seed", {
#   set.seed(42)
#   points <- c(3, 2, 1, 13, 13, 12, 23, 22)
#   vol1 <- simulate_volume(points)
#   set.seed(42)
#   vol2 <- simulate_volume(points)
#   expect_equal(vol1, vol2)
# })
#
# # Edge case: single point
#
# test_that("simulate_volume handles single point", {
#   points <- 5
#   vol <- simulate_volume(points)
#   expect_length(vol, 1)
#   expect_true(is.finite(vol))
# })
