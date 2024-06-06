library(testthat)
library(MVET)

# one way
test_that("VManova function works correctly for one-way MANOVA with wine dataset", {
  data(wine)

  # One-way MANOVA test
  result <- VManova(wine, grp1.name = "class", way = "one", method = "all", plot.scale = TRUE)

  # Check if the result has the expected structure
  expect_named(result, c("Mean.val.plot", "One.all"))

  # Convert the result columns from character to numeric
  result$One.all$Statistic <- as.numeric(result$One.all$Statistic)
  result$One.all$F.df1 <- as.numeric(result$One.all$F.df1)
  result$One.all$F.df2 <- as.numeric(result$One.all$F.df2)
  result$One.all$F.stat <- as.numeric(result$One.all$F.stat)
  result$One.all$P.value <- as.numeric(result$One.all$P.value)

  # Verify that the One.all result matches the expected values
  expected_one_all <- data.frame(
    DF = round(c(2, 2, 2, 2, 175), 2),
    Statistic = round(c(0.019341, 13.210208, 1.705821, 9.081739, NA), 6),
    F.df1 = round(c(26, 26, 26, 13, NA), 2),
    F.df2 = round(c(326, 324, 328, 164, NA), 2),
    F.stat = round(c(77.619868, 82.309761, 73.151282, 114.569636, NA), 6),
    P.value = signif(c(4.37763650999189e-123, 3.70584297243896e-126, 5.74176392839254e-120, 4.19711775487251e-75, NA), 10),
    stringsAsFactors = FALSE
  )

  # Compare each column individually with appropriate tolerance
  expect_equal(result$One.all$DF, round(expected_one_all$DF, 2))
  expect_equal(result$One.all$Statistic, round(expected_one_all$Statistic, 6))
  expect_equal(result$One.all$F.df1, round(expected_one_all$F.df1, 2))
  expect_equal(result$One.all$F.df2, round(expected_one_all$F.df2, 2))
  expect_equal(result$One.all$F.stat, round(expected_one_all$F.stat, 6))
  expect_equal(result$One.all$P.value, signif(expected_one_all$P.value, 10))

  # Check if Mean.val.plot exists and is a ggplot object
  if (!is.null(result$Mean.val.plot)) {
    expect_true("ggplot" %in% class(result$Mean.val.plot))
  } else {
    expect_true(FALSE, info = "Mean.val.plot is NULL")
  }
})


########################

# two way
test_that("VManova function works correctly for two-way MANOVA with wine dataset", {
  data(wine)
  newwine <- wine
  # (1: low, 2: medium, 3: high)
  newwine$v4 <- ifelse(wine$v4 <= 17, 1, ifelse(wine$v4 <= 22, 2, 3))

  # Two-way MANOVA test
  result <- VManova(newwine, grp1.name = "class", grp2.name = "v4", way = "two", method = "all", plot.scale = TRUE)

  # Check if the result has the expected structure
  expect_named(result, c("Mean.val.plot", "Two.Wilks", "Two.LH", "Two.Pillai", "Two.Roy"))

  # Convert the result columns from character to numeric
  result$Two.Wilks$Wilks.stat <- as.numeric(result$Two.Wilks$Wilks.stat)
  result$Two.Wilks$F.df1 <- as.numeric(result$Two.Wilks$F.df1)
  result$Two.Wilks$F.df2 <- as.numeric(result$Two.Wilks$F.df2)
  result$Two.Wilks$F.stat <- as.numeric(result$Two.Wilks$F.stat)
  result$Two.Wilks$P.value <- as.numeric(result$Two.Wilks$P.value)

  result$Two.LH$LH.stat <- as.numeric(result$Two.LH$LH.stat)
  result$Two.LH$F.df1 <- as.numeric(result$Two.LH$F.df1)
  result$Two.LH$F.df2 <- as.numeric(result$Two.LH$F.df2)
  result$Two.LH$F.stat <- as.numeric(result$Two.LH$F.stat)
  result$Two.LH$P.value <- as.numeric(result$Two.LH$P.value)

  result$Two.Pillai$Pillai.stat <- as.numeric(result$Two.Pillai$Pillai.stat)
  result$Two.Pillai$F.df1 <- as.numeric(result$Two.Pillai$F.df1)
  result$Two.Pillai$F.df2 <- as.numeric(result$Two.Pillai$F.df2)
  result$Two.Pillai$F.stat <- as.numeric(result$Two.Pillai$F.stat)
  result$Two.Pillai$P.value <- as.numeric(result$Two.Pillai$P.value)

  result$Two.Roy$Roy.stat <- as.numeric(result$Two.Roy$Roy.stat)
  result$Two.Roy$F.df1 <- as.numeric(result$Two.Roy$F.df1)
  result$Two.Roy$F.df2 <- as.numeric(result$Two.Roy$F.df2)
  result$Two.Roy$F.stat <- as.numeric(result$Two.Roy$F.stat)
  result$Two.Roy$P.value <- as.numeric(result$Two.Roy$P.value)

  # Verify that the Two.Wilks result matches the expected values
  expected_two_wilks <- data.frame(
    df = round(c(2, 2, 4, 169), 2),
    Wilks.stat = round(c(0.02075, 0.527929, 0.697283, NA), 6),
    F.df1 = round(c(24, 24, 48, NA), 2),
    F.df2 = round(c(316, 316, 610.67, NA), 2),
    F.stat = round(c(78.238568, 4.954574, 1.248345, NA), 6),
    P.value = signif(c(1.166864586e-117, 5.444356075e-12, 0.1274016333, NA), 10),
    stringsAsFactors = FALSE
  )
  expect_equal(result$Two.Wilks$df, round(expected_two_wilks$df, 2))
  expect_equal(result$Two.Wilks$Wilks.stat, round(expected_two_wilks$Wilks.stat, 6))
  expect_equal(result$Two.Wilks$F.df1, round(expected_two_wilks$F.df1, 2))
  expect_equal(result$Two.Wilks$F.df2, round(expected_two_wilks$F.df2, 2))
  expect_equal(result$Two.Wilks$F.stat, round(expected_two_wilks$F.stat, 6))
  expect_equal(result$Two.Wilks$P.value, signif(expected_two_wilks$P.value, 10))

  # Verify that the Two.LH result matches the expected values
  expected_two_lh <- data.frame(
    df = round(c(2, 2, 4, 169), 2),
    LH.stat = round(c(12.830995, 0.801424, 0.396674, NA), 6),
    F.df1 = round(c(24, 24, 48, NA), 2),
    F.df2 = round(c(314, 314, 626, NA), 2),
    F.stat = round(c(83.936092, 5.242651, 1.293323, NA), 6),
    P.value = signif(c(2.708365667e-121, 7.205596462e-13, 0.09362813781, NA), 10),
    stringsAsFactors = FALSE
  )
  expect_equal(result$Two.LH$df, round(expected_two_lh$df, 2))
  expect_equal(result$Two.LH$LH.stat, round(expected_two_lh$LH.stat, 6))
  expect_equal(result$Two.LH$F.df1, round(expected_two_lh$F.df1, 2))
  expect_equal(result$Two.LH$F.df2, round(expected_two_lh$F.df2, 2))
  expect_equal(result$Two.LH$F.stat, round(expected_two_lh$F.stat, 6))
  expect_equal(result$Two.LH$P.value, signif(expected_two_lh$P.value, 10))

  # Verify that the Two.Pillai result matches the expected values
  expected_two_pillai <- data.frame(
    df = round(c(2, 2, 4, 169), 2),
    Pillai.stat = round(c(1.692263, 0.521046, 0.329397, NA), 6),
    F.df1 = round(c(24, 24, 48, NA), 2),
    F.df2 = round(c(318, 318, 644, NA), 2),
    F.stat = round(c(72.86247, 4.668066, 1.204001, NA), 6),
    P.value = signif(c(5.680505187e-114, 4.126553735e-11, 0.1683874135, NA), 10),
    stringsAsFactors = FALSE
  )
  expect_equal(result$Two.Pillai$df, round(expected_two_pillai$df, 2))
  expect_equal(result$Two.Pillai$Pillai.stat, round(expected_two_pillai$Pillai.stat, 6))
  expect_equal(result$Two.Pillai$F.df1, round(expected_two_pillai$F.df1, 2))
  expect_equal(result$Two.Pillai$F.df2, round(expected_two_pillai$F.df2, 2))
  expect_equal(result$Two.Pillai$F.stat, round(expected_two_pillai$F.stat, 6))
  expect_equal(result$Two.Pillai$P.value, signif(expected_two_pillai$P.value, 10))

  # Verify that the Two.Roy result matches the expected values
  expected_two_roy <- data.frame(
    df = round(c(2, 2, 4, 169), 2),
    Roy.stat = round(c(9.022389, 0.6611, 0.273126, NA), 6),
    F.df1 = round(c(12, 12, 12, NA), 2),
    F.df2 = round(c(159, 159, 161, NA), 2),
    F.stat = round(c(119.546649, 8.759578, 3.664443, NA), 6),
    P.value = signif(c(5.01537082302137e-73, 1.05334966685597e-12, 7.04893414113869e-05, NA), 10),
    stringsAsFactors = FALSE
  )
  expect_equal(result$Two.Roy$df, round(expected_two_roy$df, 2))
  expect_equal(result$Two.Roy$Roy.stat, round(expected_two_roy$Roy.stat, 6))
  expect_equal(result$Two.Roy$F.df1, round(expected_two_roy$F.df1, 2))
  expect_equal(result$Two.Roy$F.df2, round(expected_two_roy$F.df2, 2))
  expect_equal(result$Two.Roy$F.stat, round(expected_two_roy$F.stat, 6))
  expect_equal(result$Two.Roy$P.value, signif(expected_two_roy$P.value, 10))

  # Check if Mean.val.plot exists and is a ggplot object
  if (!is.null(result$Mean.val.plot)) {
    expect_true("gtable" %in% class(result$Mean.val.plot))
  } else {
    expect_true(FALSE, info = "Mean.val.plot is NULL")
  }
})
