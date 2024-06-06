#' Hotelling T Square Test
#'
#' The mean vector test (Hotelling T square test) to compare one sample or two samples that satisfy the multivariate normality test and the homogeneity of covariance matrices test.
#'
#' @param data1 The data frame or matrix must consist of only numbers, and the data must consist of only a single group or class. It should not contain columns that separate groups or classes.
#' @param data2 The data frame or matrix must consist of only numbers, and the data must consist of only a single group or class. It should not contain columns that separate groups or classes. The \code{data2} is for comparison with \code{data1} and is not used to compare one sample to another.
#' @param mu0 The mu0 is used to test the mean vector hypothesis of \code{data1}. It is only used to compare one-sample.
#' @param sample The options for specifying the number of groups for group comparisons are \code{one} and \code{two}, where \code{one} is used to compare one-sample and \code{two} is used to compare two-samples. (default sample = \code{two})
#' @param plot.scale If \code{TRUE}, the data will be scaled before calculating mean values and used in the plot. It has no direct effect on the data. It only applies to two samples. (default plot.scale = \code{FALSE})
#'
#'
#' @return
#'    \item{One.HT2}{The Hotelling T square test in one-sample, showing the degrees of freedom required for the F test, the Hotelling t square statistic, the F test statistic, and the probability of significance.}
#'    \item{Mean.val.plot}{Plot the mean value parallel coordinates, representing the two samples using the mean values for each variable.}
#'    \item{Two.HT2}{The Hotelling T square test in two-sample, showing the degrees of freedom required for the F test, the Hotelling t square statistic, the F test statistic, and the probability of significance.}
#'
#'
#' @usage
#' HT2test(data1,
#'         data2,
#'         mu0 = NULL,
#'         sample = "two",
#'         plot.scale = FALSE)
#'
#'
#' @references
#' Johnson, R. A., & Wichern, D. W. (2007). Applied Multivariate Statistical Analysis (6th ed.). Pearson Prentice Hall.
#'
#'
#' @seealso \code{\link{mardiatest} for multivariate normality (Includes outlier remove)}
#' @seealso \code{\link{PPCCtest} for multivariate normality}
#' @seealso \code{\link{SPCCtest} for multivariate normality}
#' @seealso \code{\link{boxMtest} for homogeneity of covariance matrices}
#' @keywords Hotelling.T^2
#'
#' @examples
#' data(wine)
#' class1.wine <- subset(wine, class == 1)[, -1]
#' class2.wine <- subset(wine, class == 2)[, -1]
#' modified.class2.wine <- outlier(class2.wine, lim = 0, level = 0.05, option = "all")$modified.data
#'
#' ## one sample
#' value <- 0
#' p <- ncol(class1.wine)
#' mu0 <- matrix(rep(value, p), nrow = p, ncol = 1)
#' HT2test(data1 = class1.wine, mu0 = mu0, sample = "one")
#'
#' ## two sample
#' HT2test(data1 = class1.wine, data2 = modified.class2.wine, sample = "two", plot.scale = TRUE)
#'
#'
#' @import stats
#' @import grDevices
#'
#'
#' @export

HT2test <- function(data1, data2, mu0 = NULL, sample = "two", plot.scale = FALSE){

  if (any(is.na(data1))) {
    stop("NA values found in the 'data1'.", call. = FALSE)
  }

  sample_type <- c("one", "two")
  if (!sample %in% sample_type) {
    stop("Please choose from 'one', 'two' for the 'sample' parameter.", call. = FALSE)
  }




  # one sample
  if (sample == "one"){

    if (ncol(data1) != nrow(mu0)) {
      stop("The number of variables in 'data1' does not match the number of rows in 'mu0'.", call. = FALSE)
    }

    if (!missing(data2) && !is.null(data2)) {
      warning("'data2' is ignored in One Sample.")
    }

    if (plot.scale){
      warning("'plot.scale' is ignored in One Sample.")
      }



    n <- nrow(data1)
    p <- ncol(data1)
    mean_vec <- colMeans(data1)
    S <- cov(data1)

    T2_stat <- n * t(mean_vec - mu0) %*% solve(S) %*% (mean_vec - mu0)
    F_stat <- (n - p) / (p * (n - 1)) * T2_stat
    df1 <- p
    df2 <- n - p
    pvalue <- pf(F_stat, df1, df2, lower.tail = FALSE)

    res <- cbind.data.frame(round(df1, 2), round(df2, 2),
                            round(T2_stat, 6), round(F_stat, 6), signif(pvalue, 10))
    names(res) <- c("Df1", "Df2", "T2.stat", "F.stat", "P.value")
    rownames(res) <- NULL
    result <- list(One.HT2 = res)

  }



  # two sample
  if (sample == "two"){

    if (any(is.na(data2))) {
      stop("NA values found in the 'data2'.", call. = FALSE)
    }

    if (ncol(data1) != ncol(data2)) {
      stop("The number of variables in 'data1' does not match the number of variables in 'data2'.", call. = FALSE)
    }

    if (!missing(mu0) && !is.null(mu0)) {
      warning("'mu0' is ignored in Two Sample.")
    }


    n1 <- nrow(data1)
    n2 <- nrow(data2)
    p <- ncol(data1)
    mean1 <- colMeans(data1)
    mean2 <- colMeans(data2)
    S1 <- cov(data1)
    S2 <- cov(data2)
    Sp <- ((n1 - 1) * S1 + (n2 - 1) * S2) / (n1 + n2 - 2)
    diff_mean <- mean1 - mean2

    T2_stat <- (n1 * n2) / (n1 + n2) * t(diff_mean) %*% solve(Sp) %*% diff_mean
    F_stat <- ((n1 + n2 - p - 1) / (p * (n1 + n2 - 2))) * T2_stat
    df1 <- p
    df2 <- n1 + n2 - p - 1
    pvalue <- pf(F_stat, df1, df2, lower.tail = FALSE)


    # Show Mean Vector Parallel Coordinates Plot
    data_grp <- data.frame(
      grp = c(rep(1, nrow(data1)), rep(2, nrow(data2))),
      rbind(data1, data2)
    )
    res_plot <- .mean_parallel_plot(data = data_grp, grp.name = 'grp', scale = plot.scale)

    res <- cbind.data.frame(round(df1, 2), round(df2, 2),
                            round(T2_stat, 6), round(F_stat, 6), signif(pvalue, 10))
    names(res) <- c("Df1", "Df2", "T2.stat", "F.stat", "P.value")
    rownames(res) <- NULL
    result <- list(Mean.val.plot = res_plot ,Two.HT2 = res)

  }

  return(result)

}
