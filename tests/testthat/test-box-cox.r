context("Box-Cox Transformation")

test_that("Box-Cox transformation works with a numeric vector", {
  set.seed(42)
  x <- rexp(1000, 5)

  bc_out <- boxcox_transform(x, family="bcPower")

  # Manually employ Box-Cox
  df <- data.frame(x=x)

  bc_manual <- boxCox(x ~ 1, data=df, family="bcPower", plotit=FALSE, interp=TRUE)
  lambda <- with(bc_manual, x[which.max(y)])
  x_transformed <- bcPower(x, lambda=lambda)

  expect_equal(bc_out$x, x_transformed)
  expect_equal(bc_out$lambda, lambda)
})

test_that("Yeo-Johnson transformation works with a negative numeric vector", {
  set.seed(42)
  x <- rexp(1000, 5)
  x <- x - 1

  yj_out <- boxcox_transform(x, family="yjPower")

  # Manually employ Yeo-Johnson
  df <- data.frame(x=x)
  yj_manual <- boxCox(x ~ 1, data=df, family="yjPower", plotit=FALSE, interp=TRUE)
  lambda <- with(yj_manual, x[which.max(y)])
  x_transformed <- yjPower(x, lambda=lambda)

  expect_equal(yj_out$x, x_transformed)
  expect_equal(yj_out$lambda, lambda)
})

test_that("Inverse Box-Cox transformation works", {
  set.seed(42)
  x <- rexp(1000, 5)
  bc_out <- boxcox_transform(x, family="bcPower")

  inv_transform <- with(bc_out, inv_boxcox(x, lambda))
  expect_equal(inv_transform, x)

  # The Box-Cox transform when lambda=0 is equivalent to log(x).
  # Hence, the inverse transform should equal exp(x).
  expect_equal(inv_boxcox(x, lambda=0), exp(x))
})
