test_that("ernest_likelihood.function works with valid input", {
  log_likelihood_fn <- function(x) -sum((x - 1)^2)
  wrapped_fn <- ernest_likelihood(log_likelihood_fn)

  expect_s3_class(wrapped_fn, "ernest_likelihood")
  expect_s3_class(wrapped_fn, "function")
  expect_error(wrapped_fn(c(1, 2, 3)), NA) # Should not throw an error
})

test_that("ernest_likelihood.function throws error for invalid input", {
  invalid_fn <- function(x, y) x + y
  expect_error(ernest_likelihood(invalid_fn), "`object` must be a function of exactly one argument.")
})

test_that("ernest_likelihood.glm works with glm objects", {
  data(mtcars)
  glm_model <- glm(mpg ~ wt + hp, data = mtcars, family = gaussian())
  wrapped_fn <- ernest_likelihood(glm_model)

  expect_s3_class(wrapped_fn, "ernest_likelihood")
  expect_s3_class(wrapped_fn, "function")
  expect_equal(attr(wrapped_fn, "call"), glm_model$call)
  expect_equal(attr(wrapped_fn, "family"), gaussian())
  expect_equal(attr(wrapped_fn, "terms"), glm_model$terms)
})

test_that("ernest_likelihood.glm handles unsupported families", {
  data(mtcars)
  glm_model <- glm(mpg ~ wt + hp, data = mtcars, family = inverse.gaussian())
  expect_error(ernest_likelihood(glm_model), "The inverse.gaussian family is not supported.")
})

test_that("ernest_likelihood.glm handles prior weights correctly", {
  data(mtcars)
  mtcars$weights <- rep(2, nrow(mtcars))
  glm_model <- glm(mpg ~ wt + hp, data = mtcars, family = gaussian(), weights = weights)
  wrapped_fn <- ernest_likelihood(glm_model)

  expect_s3_class(wrapped_fn, "ernest_likelihood")
  expect_s3_class(wrapped_fn, "function")
})

test_that("ernest_likelihood with gaussian GLM", {
  data(mtcars)
  mtcars[1:10,]
  glm_model <- glm(mpg ~ wt + hp, data = mtcars[1:10,], family = gaussian())
  wrapped_fn <- ernest_likelihood(glm_model)

  expect_s3_class(wrapped_fn, "ernest_likelihood")
  expect_s3_class(wrapped_fn, "function")

  # Test the log-likelihood function
  # Evaluated in MATLAB
  expect_equal(
    wrapped_fn(c(30.54312065, -1.49894811, -0.04466133, 1.8255)),
    -1.569866787051702e+01
  )
  expect_equal(
    wrapped_fn(c(0, 0, 0, 1)),
    -2.121904385332047e+03
  )

  expect_warning(wrapped_fn(c(0, 0, 0, -1)), "NaNs produced")
})

test_that("ernest_likelihood with binomial GLM", {
  model1 <- glm(
    case ~ spontaneous + induced,
    data = infert,
    family = binomial()
  )

  wrapped_fn <- ernest_likelihood(model1)
  expect_s3_class(wrapped_fn, "ernest_likelihood")
  expect_s3_class(wrapped_fn, "function")

  expect_equal(
    wrapped_fn(coef(model1)),
    -139.8059894168912
  )
  expect_equal(
    wrapped_fn(rep(0, 3)),
    -171.9005007788668
  )
})

test_that("ernest_likelihood with weighted binomial", {
  model1 <- glm(
    cbind(ncases, ncontrols) ~ agegp + tobgp * alcgp,
    data = esoph,
    family = binomial()
  )
  model2 <- glm(
    formula = ncases/(ncases + ncontrols) ~ agegp + tobgp * alcgp,
    data = esoph,
    family = binomial(),
    weights = (ncases + ncontrols)
  )

  wrapped_fn1 <- ernest_likelihood(model1)
  wrapped_fn2 <- ernest_likelihood(model2)

  expect_equal(
    wrapped_fn1(coef(model1)),
    -95.97057946368294
  )
  expect_equal(
    wrapped_fn1(rep(0, 21)),
    -422.5784770088512
  )

  expect_equal(
    wrapped_fn2(coef(model2)),
    -95.97057946368294
  )
  expect_equal(
    wrapped_fn2(rep(0, 21)),
    -422.5784770088512
  )
})
