#' Probability Plot Correlation Coefficient(PPCC) Test for Multivariate Normality Test
#'
#' The correlation coefficient of the quantiles and mahalanobis square are tested by using the critical value table by Filliben (1975) for the multivariate normality test.
#'
#' @param data A numeric matrix or data frame.
#' @param level At the \code{0.01} or \code{0.05} significance level, the critical value. (default = \code{0.05})
#'
#'
#' @return
#'    \item{data.cnt}{Observation \code{n} data count.}
#'    \item{PPCC.value}{Correlation coefficient value.}
#'    \item{critical.value}{Critical value proposed by Filliben (1975), corresponding to \code{data.cnt} and \code{PPCC.value}.}
#'    \item{test.res}{Final result of multivariate normality.}
#'    \item{QQPlot}{Shows Chi-Square Q-Q plot.}
#'
#' @usage
#' PPCCtest(data,
#'          level = 0.05)
#'
#' @references
#' Filliben, J. J. (1975), The Probability Plot Correlation Coefficient Test for Normality, \emph{Technometrics} 17, 111-117.
#'
#'
#' @keywords MVN
#' @examples
#' data(wine)
#' class1.wine <- subset(wine, class == 1)[, -1]
#' PPCCtest(class1.wine, level = 0.05)
#'
#' @import stats
#' @import ggplot2
#'
#' @export

# Chi-squre Plot & Probability Plot Correlation Coefficient Test for Checking MVN
PPCCtest <- function(data, level = 0.05){

  if (any(is.na(data))) {
    stop("NA values found in the data.", call. = FALSE)
  }

  # significance level error
  if (!(level %in% c(0.01, 0.05))) {
    stop("Only significance level 0.01 and 0.05 are available.", call. = FALSE)
  }


  # correlation coefficient value
  n <- nrow(data)
  p <- ncol(data)
  S <- cov(data)
  xbar <- colMeans(data)
  m <- mahalanobis(data, xbar, S)
  m <- sort(m)
  id <- seq(1, n)
  pt <- (id - 0.5) / n
  q <- qchisq(pt, p)
  rq <- cor(cbind(q, m))[1,2]
  rq <- round(rq, 4)


  # Critical Value Table
  if (n %in% CVtable$N){
    crit <- subset(CVtable, subset = (N == n), select = as.character(level))
  } else {
    num_lower <- utils::tail(subset(CVtable, subset = (N < n)), 1)
    num_upper <- utils::head(subset(CVtable, subset = (N > n)), 1)
    pos_ratio <- (n - num_lower$N) / (num_upper$N - num_lower$N)
    crit <- num_lower[, as.character(level)] + (num_upper[, as.character(level)] - num_lower[, as.character(level)]) * pos_ratio
  }



  # Probability Plot Correlation Coefficient Test for Normality
  test_res <- ifelse(rq > crit,
                     paste(rq, ">", round(crit, 4), ": Accept Normality", "at level =", level),
                     paste(rq, "<", round(crit, 4), ": Reject Normality", "at level =", level))
  test_res <- c(test_res)


  # Chi-Square Q-Q plot
  QQPlot <- ggplot(data.frame(q, m), aes(x = q, y = m)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, colour = "blue", linewidth = 0.7) +
    labs(title = "Chi-Square Q-Q plot",
         x = "Chi-Square Quantile", y = "Squared Mahalanobis Distance")


  result <- list(data.cnt = n, PPCC.value = rq, critical.value = crit,
                 test.res = test_res, QQPlot = QQPlot)

  return(result)

}


utils::globalVariables("N") # Because of using critical value table
