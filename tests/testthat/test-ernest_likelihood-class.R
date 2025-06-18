test_that("create_likelihood.function works with valid input", {
  log_likelihood_fn <- function(x) -sum((x - 1)^2)
  wrapped_fn <- create_likelihood(log_likelihood_fn)
  expect_s3_class(wrapped_fn, c("ernest_likelihood", "function"))
  expect_true(is.function(attr(wrapped_fn, "body")))

  expect_equal(wrapped_fn(c(1, 2, 3)), -5)

  expect_s3_class(create_likelihood(wrapped_fn), "ernest_likelihood")
  expect_equal(
    create_likelihood(wrapped_fn),
    wrapped_fn
  )
})

test_that("create_likelihood.function throws errors when problematic", {
  log_likelihood_fn <- function(x) sum((x - 1)^2)
  wrapped_fn <- create_likelihood(log_likelihood_fn)

  expect_warning(
    {
      na <- wrapped_fn(c(1, 2, NA))
    },
    "`NA` to `-Inf`"
  )
  expect_warning(
    {
      inf <- wrapped_fn(c(1, 2, Inf))
    },
    "`Inf` to `-Inf`"
  )
  expect_warning(
    {
      nan <- wrapped_fn(c(1, 2, NaN))
    },
    "`NaN` to `-Inf`"
  )
  expect_equal(c(na, inf, nan), rep(-Inf, 3))
})

test_that("create_likelihood.glm works with glm objects", {
  data(mtcars)
  glm_model <- glm(mpg ~ wt + hp, data = mtcars, family = gaussian())
  wrapped_fn <- create_likelihood(glm_model)
  expect_s3_class(wrapped_fn, c("ernest_likelihood", "function"))
  expect_true(is.function(attr(wrapped_fn, "body")))

  expect_equal(
    optim(c(0, 0, 0, 1), \(x) -wrapped_fn(x))$value,
    -as.double(logLik(glm_model)),
    tolerance = 0.1
  )
  design_matrix <- model.matrix(glm_model)
  expect_equal(
    wrapped_fn(c(0, 0, 0, 1)),
    sum(
      dnorm(
        model.response(glm_model$model),
        mean = model.matrix(glm_model) %*% c(0, 0, 0),
        sd = 1,
        log = TRUE
      )
    )
  )

  expect_equal(
    create_likelihood(wrapped_fn),
    wrapped_fn
  )
})

test_that("create_likelihood.glm works with binomial", {
  data(infert)
  glm_model <- glm(
    case ~ spontaneous + induced,
    data = infert,
    family = binomial()
  )
  wrapped_fn <- create_likelihood(glm_model)

  expect_s3_class(wrapped_fn, c("ernest_likelihood", "function"))
  expect_true(is.function(attr(wrapped_fn, "body")))

  expect_equal(
    optim(coef(glm_model), \(x) -wrapped_fn(x))$value,
    -as.double(logLik(glm_model)),
    tolerance = 0.1
  )

  expect_equal(
    wrapped_fn(c(0, 0, 0)),
    sum(
      dbinom(
        model.response(glm_model$model),
        size = rep(1, nobs(glm_model)),
        prob = binomial()$linkinv(model.matrix(glm_model) %*% c(0, 0, 0)),
        log = TRUE
      )
    )
  )

  glm_model <- glm(
    case ~ spontaneous + induced,
    data = infert,
    family = binomial(link = "probit")
  )
  wrapped_fn <- create_likelihood(glm_model)
  expect_equal(
    optim(coef(glm_model), \(x) -wrapped_fn(x))$value,
    -as.double(logLik(glm_model)),
    tolerance = 0.1
  )
})

model1 <- glm(
  cbind(ncases, ncontrols) ~ agegp + tobgp * alcgp,
  data = esoph,
  family = binomial()
)
model2 <- glm(
  formula = ncases / (ncases + ncontrols) ~ agegp + tobgp * alcgp,
  data = esoph,
  family = binomial(),
  weights = (ncases + ncontrols)
)

test_that("Binomial responses parse identically", {
  y_1 <- model.response(model1$model)
  y_2 <- model.response(model2$model)
  w_1 <- model.weights(model1$model)
  w_2 <- model.weights(model2$model)
  mod1 <- parse_binomial_response(y_1, w_1)
  mod2 <- parse_binomial_response(y_2, w_2)
  expect_equal(mod1$y, mod2$y)
  expect_equal(mod1$size, mod2$size)
})

test_that("create_likelihood with weighted binomial", {
  model1 <- glm(
    cbind(ncases, ncontrols) ~ agegp + tobgp * alcgp,
    data = esoph,
    family = binomial()
  )
  model2 <- glm(
    formula = ncases / (ncases + ncontrols) ~ agegp + tobgp * alcgp,
    data = esoph,
    family = binomial(),
    weights = (ncases + ncontrols)
  )
  wrapped_fn1 <- create_likelihood(model1)
  wrapped_fn2 <- create_likelihood(model2)
  expect_equal(
    wrapped_fn1(coef(model1)),
    -95.97057946368294
  )
  expect_equal(
    wrapped_fn1(rep(0, 21)),
    -422.5784770088512
  )
  expect_equal(
    wrapped_fn2(coef(model1)),
    -95.97057946368294
  )
  expect_equal(
    wrapped_fn2(rep(0, 21)),
    -422.5784770088512
  )
})

test_that("create_likelihood.glm handles unsupported families", {
  data(mtcars)
  glm_model <- glm(mpg ~ wt + hp, data = mtcars, family = inverse.gaussian())
  expect_error(
    create_likelihood(glm_model),
    "The inverse.gaussian family is not supported."
  )
})
