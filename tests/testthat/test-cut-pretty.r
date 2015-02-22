context("Pretty cuts with cut_pretty")

test_that("cut_pretty consumes an iterator when n < length(iterator)", {
  set.seed(42)
  x <- runif(n=50, 0, 50)
  pretty_cuts <- cut_pretty(x, breaks=pretty(x))

  # Cuts `x` and then manipulates levels to match expected behavior
  manual_cuts <- cut(x, breaks=pretty(x))
  levels_cut <- levels(manual_cuts)
  levels_cut <- gsub("(", "", levels_cut, fixed=TRUE)
  levels_cut <- gsub("]", "", levels_cut, fixed=TRUE)
  levels_cut <- gsub(",", " to ", levels_cut, fixed=TRUE)
  levels(manual_cuts) <- levels_cut

  expect_equal(pretty_cuts, manual_cuts)
})
