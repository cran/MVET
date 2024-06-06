library(testthat)
library(MVET)

test_that("HT2test function works correctly for one-sample test with wine dataset", {
  data(wine)
  class1.wine <- subset(wine, class == 1)[, -1]

  ## One sample
  value <- 0
  p <- ncol(class1.wine)
  mu0 <- matrix(rep(value, p), nrow = p, ncol = 1)
  result <- HT2test(data1 = class1.wine, mu0 = mu0, sample = "one")

  # Check if the result has the expected structure
  expect_named(result, "One.HT2")

  # Verify that the One.HT2 result matches the expected values
  expected_one_ht2 <- data.frame(
    Df1 = round(13, 2),
    Df2 = round(46, 2),
    T2.stat = round(85601.502, 6),
    F.stat = round(5222.3728, 6),
    P.value = signif(2.787469e-68, 10),
    stringsAsFactors = FALSE
  )

  # Compare each column individually with appropriate tolerance
  expect_equal(result$One.HT2$Df1, round(expected_one_ht2$Df1, 2))
  expect_equal(result$One.HT2$Df2, round(expected_one_ht2$Df2, 2))
  expect_equal(result$One.HT2$T2.stat, round(expected_one_ht2$T2.stat, 6))
  expect_equal(result$One.HT2$F.stat, round(expected_one_ht2$F.stat, 6))
  expect_equal(result$One.HT2$P.value, signif(expected_one_ht2$P.value, 10))
})




test_that("HT2test function works correctly for two-sample test with wine dataset", {
  data(wine)
  class1.wine <- subset(wine, class == 1)[, -1]
  class2.wine <- subset(wine, class == 2)[, -1]
  modified.class2.wine <- outlier(class2.wine, lim = 0, level = 0.05, option = "all")$modified.data

  # Two sample test
  result_two <- HT2test(data1 = class1.wine, data2 = modified.class2.wine, sample = "two", plot.scale = TRUE)

  # Check if the result has the expected structure
  expect_named(result_two, c("Mean.val.plot", "Two.HT2"))

  # Verify that the Two.HT2 result matches the expected values
  expected_two_ht2 <- data.frame(
    Df1 = round(13, 2),
    Df2 = round(109, 2),
    T2.stat = round(852.85755, 6),
    F.stat = round(59.098203, 6),
    P.value = signif(3.60141e-43, 10),
    stringsAsFactors = FALSE
  )

  # Compare each column individually with appropriate tolerance
  expect_equal(result_two$Two.HT2$Df1, round(expected_two_ht2$Df1, 2))
  expect_equal(result_two$Two.HT2$Df2, round(expected_two_ht2$Df2, 2))
  expect_equal(result_two$Two.HT2$T2.stat, round(expected_two_ht2$T2.stat, 6))
  expect_equal(result_two$Two.HT2$F.stat, round(expected_two_ht2$F.stat, 6))
  expect_equal(result_two$Two.HT2$P.value, signif(expected_two_ht2$P.value, 10))

  # Check if Mean.val.plot exists and is a ggplot object
  if (!is.null(result_two$Mean.val.plot)) {
    expect_true("ggplot" %in% class(result_two$Mean.val.plot))
  } else {
    expect_true(FALSE, info = "Mean.val.plot is NULL")
  }
})
