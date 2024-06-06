library(testthat)
library(MVET)

test_that("boxMtest works correctly with wine dataset", {
  data(wine)
  class <- wine$class
  winedata <- subset(wine, select = -class)

  result <- boxMtest(winedata, class)

  # Check if the structure of result is correct
  expect_named(result, c("M.stat", "df", "p.value"))

  # Check if the values are correct
  expect_equal(result$M.stat, round(684.20309, 6))
  expect_equal(result$df, 182)
  expect_equal(result$p.value, signif(2.891851e-59, 10))
})
