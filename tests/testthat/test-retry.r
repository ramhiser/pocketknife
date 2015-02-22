context("Exponential backoff with retry")

test_that("retry generates error with simple example designed to fail", {
  expect_error(retry(log("a"), silent=TRUE, max_attempts=3))
})
