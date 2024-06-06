#' Mardia Test for Multivariate Normality Test
#'
#' Performs a multivariate normality test by conducting a mardia test using skewness and kurtosis. If both skewness and kurtosis are satisfied, multivariate normality is satisfied.
#'
#' @param data A numeric matrix or data frame.
#' @param level The significance level of the skewness and kurtosis statistics. (default = \code{0.05})
#' @param showplot If \code{TRUE}, show a chi-square Q-Q plot using \code{ggplot2}. If '\code{showoutlier}' is \code{TRUE}, outliers are also displayed. (default = \code{FALSE})
#' @param showoutlier If \code{TRUE}, show the outliers number and count. (default = \code{FALSE})
#' @param outlieropt An \code{"option"} in the \code{outlier} function. (default = \code{"all"})
#' @param shownewdata If \code{TRUE} Shows the new data with outliers removed. (default = \code{FALSE})
#'
#' @return
#'    \item{mult.nomality}{Calculate statistics and p-values for skewness and kurtosis to ultimately determine whether multivariate normality is satisfied.}
#'    \item{QQPlot}{Shows Chi-Square Q-Q plot.}
#'    \item{...}{Same as the result of \code{outlier}}
#'
#'
#' @usage
#' mardiatest(data,
#'            level = 0.05,
#'            showplot = FALSE,
#'            showoutlier = FALSE,
#'            outlieropt = "all",
#'            shownewdata = FALSE)
#'
#'
#' @references
#' Mardia, K. V. (1970), Measures of multivariate skewness and kurtosis with applications. Biometrika, 57(3), 519-530.
#'
#' Mardia, K. V. (1974), Applications of Some Measures of Multivariate Skewness and Kurtosis in Testing Normality and Robustness Studies. Sankhya, 36, 115-128.
#'
#'
#' @seealso \code{\link{outlier}}
#' @keywords MVN
#' @examples
#' ## Simple Mardia Test
#' data(wine)
#' class2.wine <- subset(wine, class == 2)[, -1]
#' mardiatest(class2.wine, level = 0.05, showplot = TRUE)
#'
#' ## Mardia Test and Outlier Detection
#' data(wine)
#' class2.wine <- subset(wine, class == 2)[, -1]
#' mardiatest(class2.wine, level = 0.05, showplot = TRUE,
#'            showoutlier = TRUE, outlieropt = "all", shownewdata = TRUE)
#'
#'
#' @import stats
#' @import ggplot2
#'
#' @export

mardiatest <- function(data, level = 0.05, showplot = FALSE, showoutlier = FALSE, outlieropt = "all", shownewdata = FALSE){

  if (any(is.na(data))) {
    stop("NA values found in the data.", call. = FALSE)
  }

  scale_data <- scale(data, scale = FALSE)
  n <- nrow(scale_data)
  p <- ncol(scale_data)
  xbar <- colMeans(scale_data)
  S <- cov(scale_data)

  maha.temp <- matrix(0, n, n)
  for(i in 1:n){
    for(j in 1:n){
      maha.temp[i,j] <- t(scale_data[i, ] - xbar) %*% solve(S) %*% (scale_data[j,] - xbar)
    }
  }

  b1p <- sum(maha.temp^3) / n^2
  b2p <- sum(diag(maha.temp^2)) / n
  df <- p * (p + 1) * (p + 2) / 6


  if(n < 20){
    k <- (p + 1) * (n + 1) * (n + 3) / ((n * (n + 1) * (p + 1)) - 6)
    skew <- n * k * b1p / 6
  } else {
    skew <-  n * b1p / 6
  }

  kurt <- (b2p - p * (p + 2)) * sqrt(n / (8 * p * (p + 2)))
  pv.skew <-  pchisq(skew, df, lower.tail = FALSE)
  pv.kurt <- 2 * (1 - pnorm(abs(kurt)))


  skew_res <- ifelse(pv.skew > level, "Accept", "Reject")


  kurt_res <-ifelse(pv.kurt > level, "Accept", "Reject")


  mvn_res <- ifelse(pv.skew > level && pv.kurt > level, "Accept", "Reject")


  test_name <- rbind("Skewness", "Kurtosis", "MVN Test")
  res_stat <- rbind(round(skew, 6), round(kurt, 6), NA)
  res_pv <- rbind(signif(pv.skew, 10), signif(pv.kurt, 10), NA)
  res <- rbind(skew_res, kurt_res, mvn_res)


  if (showoutlier) {
    out.res <- outlier(data, level = level, option = outlieropt)
  }


  # Q-Q plot (exclude outlier)
  if (showplot && !showoutlier) {
    m <- mahalanobis(scale_data, xbar, S)
    m <- sort(m)
    id <- seq(1, n)
    pt <- (id - 0.5) / n
    q <- qchisq(pt, p)
    plot.data <- data.frame(q, m)

    # Chi-Square Q-Q plot
    QQPlot <- ggplot(plot.data, aes(x = q, y = m)) +
      geom_point() +
      geom_abline(intercept=0, slope=1, colour = "blue", linewidth = 0.7) +
      labs(title = "Chi-Square Q-Q plot",
           x = "Chi-Square Quantile", y = "Squared Mahalanobis Distance")
  }


  # Q-Q plot (include outlier)
  if (showplot && showoutlier) {
    m <- mahalanobis(scale_data, xbar, S)
    names(m) <- c(1:n)
    m <- sort(m)
    id <- seq(1, n)
    pt <- (id - 0.5) / n
    q <- qchisq(pt, p)
    plot.data1 <- data.frame(num = names(m), q, m)

    out.name <- paste0("Outliers(n=", out.res$outlier.cnt, ")")
    non.name <- paste0("Non-outliers(n=", n - out.res$outlier.cnt, ")")
    legend.out <- ifelse(1:n %in% out.res$outlier.num, out.name, non.name)
    plot.data2 <- data.frame(num = c(1:n), legend.out)
    plot.data <- merge(plot.data1, plot.data2, by = "num")
    outlier.data <- subset(plot.data, legend.out == out.name)

    # Chi-Square Q-Q plot
    QQPlot <- ggplot(plot.data, aes(x = q, y = m, color = legend.out)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, colour = "blue", linewidth = 0.7) +
      geom_text(data = outlier.data, aes(label = num), vjust = -1, show.legend = FALSE) +
      theme(legend.title = element_blank(), legend.position = "bottom") +
      scale_color_manual(values = c("#FF0033", "black"), breaks = c(out.name, non.name)) +
      labs(title = "Chi-Square Q-Q plot",
           x = "Chi-Square Quantile", y = "Squared Mahalanobis Distance")
  }


  res <- cbind.data.frame(test_name, res_stat, res_pv, res)
  names(res) <- c("Test", "Statistics", "P.Value", "Test.result")
  rownames(res) <- NULL
  result <- list(mult.nomality = res)

  if (showoutlier) {
    outlier.result <- out.res[c(2, 3, 4)]
    result <- c(result, outlier.result)
    if (shownewdata) {
      new.data <- out.res[1]
      result <- c(new.data, result)
    }
  }

  if (showplot) {
    result <- c(result, list(QQplot = QQPlot))
  }

  return(result)

}

utils::globalVariables("num") # Because of using plot data frame
