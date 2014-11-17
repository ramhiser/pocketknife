context("Contingency Tables")

test_that("table_na works as expected", {  
  # No missing data
  expect_equal(table_na(iris$Species), table(iris$Species))
  
  # Now with missing data
  iris$Species[1:5] <- NA
  expect_equal(table_na(iris$Species), table(iris$Species, useNA="ifany"))
})
