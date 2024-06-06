library(testthat)
library(MVET)

test_that("PPCCtest function works correctly with wine dataset", {
  data(wine)
  class1.wine <- subset(wine, class == 1)[, -1]

  # Call the PPCCtest function
  result <- PPCCtest(class1.wine, level = 0.05)

  # Check if the result has the expected structure
  expect_named(result, c("data.cnt", "PPCC.value", "critical.value", "test.res", "QQPlot"))

  # Verify that the result values match the expected values
  expect_equal(result$data.cnt, 59)
  expect_equal(result$PPCC.value, round(0.9922, 4))
  expect_equal(result$critical.value, 0.97938)
  expect_equal(result$test.res, "0.9922 > 0.9794 : Accept Normality at level = 0.05")

  # Check if QQPlot exists and is a ggplot object
  if (!is.null(result$QQPlot)) {
    expect_true("ggplot" %in% class(result$QQPlot))
  } else {
    expect_true(FALSE, info = "QQPlot is NULL")
  }
})
