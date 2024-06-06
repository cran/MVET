library(testthat)
library(MVET)

test_that("outlier function works correctly with wine dataset", {
  data(wine)
  class2.wine <- subset(wine, class == 2)[, -1]

  result <- outlier(class2.wine, lim = 0, level = 0.05, option = "all")

  # Check if the structure of result is correct
  expect_named(result, c("modified.data", "modified.mvn", "outlier.num", "outlier.cnt"))

  # Check if modified.data is as expected (checking a few rows for simplicity)
  expect_equal(as.numeric(result$modified.data[1,]),
               c(12.37, 0.94, 1.36, 10.6, 88, 1.98, 0.57, 0.28, 0.42, 1.95, 1.05, 1.82, 520), tolerance = 1e-5)
  expect_equal(as.numeric(result$modified.data[2,]),
               c(12.33, 1.10, 2.28, 16.0, 101, 2.05, 1.09, 0.63, 0.41, 3.27, 1.25, 1.67, 680), tolerance = 1e-5)

  # Check if modified.mvn is as expected
  expected_mvn <- data.frame(
    Test = c("Skewness", "Kurtosis", "MVN Test"),
    Statistics = c(round(496.573547, 6), round(-1.384847, 6), NA),
    P.Value = c(signif(0.08688040889, 10), signif(0.1660993537, 10), NA),
    Test.result = c("Accept", "Accept", "Accept"),
    stringsAsFactors = FALSE
  )
  expect_equal(result$modified.mvn, expected_mvn)

  # Check if outlier.num is as expected
  expect_equal(result$outlier.num, c(63, 15, 37, 11, 52, 20, 38))

  # Check if outlier.cnt is as expected
  expect_equal(result$outlier.cnt, 7)
})


