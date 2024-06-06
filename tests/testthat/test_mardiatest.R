library(testthat)
library(MVET)

# Test1
test_that("First mardiatest function works correctly with wine dataset", {
  data(wine)
  class2.wine <- subset(wine, class == 2)[, -1]

  # Call the mardiatest function
  result <- mardiatest(class2.wine, level = 0.05, showplot = TRUE)

  # Check the structure of the result
  print(result)

  # Verify the names of the result
  expect_named(result, c("mult.nomality", "QQplot"))

  # Verify that the mult.nomality result matches the expected values
  expected_nomality <- data.frame(
    Test = c("Skewness", "Kurtosis", "MVN Test"),
    Statistics = c(round(682.515366, 6), round(2.181523, 6), NA),
    P.Value = c(signif(2.349902e-11, 10), signif(2.914472e-02, 10), NA),
    Test.result = c("Reject", "Reject", "Reject"),
    stringsAsFactors = FALSE
  )

  # Rename P-Value column to P.Value
  names(result$mult.nomality)[names(result$mult.nomality) == "P-Value"] <- "P.Value"

  expect_equal(result$mult.nomality, expected_nomality, tolerance = 1e-6)

  # Check if QQplot exists
  if (!is.null(result$QQplot)) {
    expect_true("ggplot" %in% class(result$QQplot))
  } else {
    expect_true(FALSE, info = "QQplot is NULL")
  }
})





# Test2
test_that("Second mardiatest function works correctly with wine dataset", {
  data(wine)
  class2.wine <- subset(wine, class == 2)[, -1]

  # Call the mardiatest function
  result <- mardiatest(class2.wine, level = 0.05, showplot = TRUE, showoutlier = TRUE, outlieropt = "all", shownewdata = TRUE)

  # Check the names of the result list
  expected_names <- c("modified.data", "mult.nomality", "modified.mvn", "outlier.num", "outlier.cnt")
  if (!is.null(result$QQplot)) {
    expected_names <- c(expected_names, "QQplot")
  }
  expect_named(result, expected_names)

  # Check specific rows of modified.data for accuracy
  expect_equal(as.numeric(result$modified.data[1,]), c(12.37, 0.94, 1.36, 10.6, 88, 1.98, 0.57, 0.28, 0.42, 1.95, 1.05, 1.82, 520), tolerance = 1e-5)
  expect_equal(as.numeric(result$modified.data[2,]), c(12.33, 1.10, 2.28, 16.0, 101, 2.05, 1.09, 0.63, 0.41, 3.27, 1.25, 1.67, 680), tolerance = 1e-5)

  # Check the mult.nomality result for accuracy
  expected_mult_nomality <- data.frame(
    Test = c("Skewness", "Kurtosis", "MVN Test"),
    Statistics = c(682.515366, 2.181523, NA),
    P.Value = c(2.349902e-11, 2.914472e-02, NA),
    Test.result = c("Reject", "Reject", "Reject"),
    stringsAsFactors = FALSE
  )
  names(result$mult.nomality)[names(result$mult.nomality) == "P-Value"] <- "P.Value"
  expect_equal(result$mult.nomality, expected_mult_nomality, tolerance = 1e-6)

  # Check the modified.mvn result for accuracy
  expected_modified_mvn <- data.frame(
    Test = c("Skewness", "Kurtosis", "MVN Test"),
    Statistics = c(496.573547, -1.384847, NA),
    P.Value = c(0.08688041, 0.16609935, NA),
    Test.result = c("Accept", "Accept", "Accept"),
    stringsAsFactors = FALSE
  )
  names(result$modified.mvn)[names(result$modified.mvn) == "P-Value"] <- "P.Value"
  expect_equal(result$modified.mvn, expected_modified_mvn, tolerance = 1e-6)

  # Check if outlier.num is as expected
  expect_equal(result$outlier.num, c(63, 15, 37, 11, 52, 20, 38))

  # Check if outlier.cnt is as expected
  expect_equal(result$outlier.cnt, 7)

  # Check if QQplot exists and is a ggplot object
  if (!is.null(result$QQplot)) {
    expect_true("ggplot" %in% class(result$QQplot))
  } else {
    # Warn if QQplot is NULL
    warning("QQplot is NULL")
  }
})

