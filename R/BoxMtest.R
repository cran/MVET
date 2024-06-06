#' Box's M-test
#'
#' Performs Box's M-test for homogeneity of covariance matrices derived from multivariate normality data according to a single classification factor. This test is based on the chi-square approximation.
#'
#' @param data A numeric matrix or data frame.
#' @param group In either vector or factor form, the length of the group must correspond to the number of observations \code{n} in the data.
#'
#'
#' @return
#'    \item{M.stat}{Box's M-test statistic approximates the chi-square distribution.}
#'    \item{df}{The degree of freedom is related to the test statistic.}
#'    \item{p.value}{The p-value of the test statistic.}
#'
#'
#' @usage
#' boxMtest(data,
#'          group)
#'
#'
#' @seealso \code{mardiatest}
#' @keywords BoxM
#' @examples
#' data(wine)
#' class <- wine$class
#' winedata <- subset(wine, select = -class)
#' boxMtest(winedata, class)
#'
#' @import stats
#'
#' @export

boxMtest <- function(data, group){

  if (any(is.na(data))) {
    stop("NA values found in the data.", call. = FALSE)
  }


  if (nrow(data) != length(group))
    stop("The number of data and the group do not match!", call. = FALSE)

  n <- nrow(data)
  p <- ncol(data)
  group <- as.numeric(group)

  if(min(group) == 0){
    group <- group + 1
  }

  nk <- tabulate(group)
  df <- p * (p + 1) * (length(nk) - 1) / 2
  Sp <- 0
  group.sum1 <- 0
  group.sum2 <- 0
  M <- 0

  for(i in 1:length(nk)){
    group.sum1 <- group.sum1 + (nk[i] - 1)
  }

  for(i in 1:length(nk)){
    Sp <- Sp + (nk[i] - 1) * cov(data[group == i, ]) / group.sum1
    group.sum2 <- group.sum2 + (1 / (nk[i] - 1))
  }
  group.temp <- group.sum2 - 1 / group.sum1

  gamma <- 1 - group.temp * (2 * p^2 + 3 * p - 1) / (6 * (p + 1) * (length(nk) - 1))

  for(i in 1:length(nk)){
    M <- M + gamma * (nk[i] - 1) * log(det(solve(cov(data[group == i, ])) %*% Sp))
  }

  pvalue <- pchisq(M, df, lower.tail = FALSE)

  result <- list(M.stat = round(M, 6), df = round(df, 2), p.value = signif(pvalue, 10))
  return(result)

}
