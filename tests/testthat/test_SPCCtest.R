library(testthat)
library(MVET)

test_that("SPCCtest function works correctly with wine dataset", {
  data(wine)
  class1.wine <- subset(wine, class == 1)[, -1]

  # Call the SPCCtest function
  result <- SPCCtest(class1.wine, k = 5, level = 0.05)

  # Check if the result has the expected structure
  expect_named(result, c("Srivastava.QQplot", "data.cnt", "explain.ratio", "critical.value", "result"))

  # Verify that the data.cnt matches the expected value
  expect_equal(result$data.cnt, 59)

  # Verify that the explain.ratio matches the expected values
  expected_explain_ratio <- round(c(28.0272, 18.2361, 14.3088, 7.2643, 6.6721, 6.5108, 5.2466, 4.0029, 3.2172, 2.3865, 1.8824, 1.4057, 0.8393), 4)
  expect_equal(result$explain.ratio, expected_explain_ratio, tolerance = 1e-4)

  # Verify that the critical.value matches the expected value
  expect_equal(result$critical.value, 0.97938)

  # Convert the result columns from list to appropriate types
  result$result$PC <- as.character(result$result$PC)
  result$result$Corr.coef <- as.numeric(result$result$Corr.coef)
  result$result$Crit.value <- as.numeric(result$result$Crit.value)
  result$result$Sig.level <- as.numeric(result$result$Sig.level)
  result$result$Test.res <- as.character(result$result$Test.res)

  # Verify that the result table matches the expected values
  expected_result <- data.frame(
    PC = c("PC1", "PC2", "PC3", "PC4", "PC5", "ALL"),
    Corr.coef = c(0.9928, 0.9879, 0.9954, 0.9866, 0.9916, NA),
    Crit.value = c(0.9794, 0.9794, 0.9794, 0.9794, 0.9794, NA),
    Sig.level = c(0.05, 0.05, 0.05, 0.05, 0.05, NA),
    Test.res = c("Accept", "Accept", "Accept", "Accept", "Accept", "Accept"),
    stringsAsFactors = FALSE
  )
  expect_equal(result$result, expected_result)

  # Check if Srivastava.QQplot exists and is a gtable object
  if (!is.null(result$Srivastava.QQplot)) {
    expect_true("gtable" %in% class(result$Srivastava.QQplot))
  } else {
    expect_true(FALSE, info = "Srivastava.QQplot is NULL")
  }
})
