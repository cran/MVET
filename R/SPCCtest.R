#' Srivastava Plot Correlation Coefficient(SPCC) Test for Multivariate Normality Test
#'
#' Using principal component analysis, the number of eigenvalues is selected such that the ratio of eigenvalues exceeds 70\%. The principal component score vectors corresponding to these selected eigenvalues are used, and testing is conducted using the threshold defined by Filliben (1975). Users have the option to select the number of eigenvalues for the analysis based on their requirements.
#'
#' @param data A numeric matrix or data frame.
#' @param k The number of principal components can be manually selected. If 0 is entered, it automatically finds k components such that the explained variance ratio is at least 70\%. (default = \code{0})
#' @param level At the \code{0.01} or \code{0.05} significance level, the critical value. (default = \code{0.05})
#'
#'
#' @return
#'    \item{Srivastava.QQplot}{Shows a chi-Square Q-Q plot for each PCs using ggplot2.}
#'    \item{data.cnt}{Observation \code{n} data count.}
#'    \item{explain.ratio}{Displays all explained variance ratios.}
#'    \item{critical.value}{Critical value proposed by Filliben (1975), corresponding to \code{data.cnt} and \code{PPCC.value}.}
#'    \item{result}{Final result of multivariate normality.}
#'
#'
#' @usage
#' SPCCtest(data,
#'          k = 0,
#'          level = 0.05)
#'
#'
#' @references
#' Srivastava, M. S. (1984), A measure of skewness and kurtosis and a graphical method for assessing multivariate normality. Statistics & Probability Letters, 2(5), 263-267.
#'
#' Filliben, J. J. (1975), The Probability Plot Correlation Coefficient Test for Normality, \emph{Technometrics} 17, 111-117.
#'
#'
#' @keywords MVN
#' @examples
#' data(wine)
#' class1.wine <- subset(wine, class == 1)[, -1]
#' SPCCtest(class1.wine, k = 5, level = 0.05)
#'
#' @import stats
#' @import ggplot2
#' @import gridExtra
#'
#' @export

SPCCtest <- function(data, k = 0, level = 0.05){

  if (any(is.na(data))) {
    stop("NA values found in the data.", call. = FALSE)
  }

  # significance levels error
  if (!(level %in% c(0.01, 0.05))) {
    stop("Only significance level 0.01 and 0.05 are available.", call. = FALSE)
  }

  # k must be integer
  if (!is.numeric(k) || k != floor(k)) {
    stop("'k' must be an integer value.", call. = FALSE)
  }


  # correlation coefficient value of each PC scores & quantiles
  n <- nrow(data)
  p <- ncol(data)
  R <- cor(data)
  eig_vec <- eigen(R)$vectors
  z <- scale(data, scale=T) %*% eig_vec
  z.sort <- apply(z, 2, sort)
  qi <- qnorm((1:n) / (n + 1))


  # If k is zero(default), select principal components that account for 70% or more of the eigenvalues. However, k should select at least two eigenvalues.
  eig_val <- eigen(R)$values
  explain <- eig_val / sum(eig_val) * 100
  if (k == 0){
    k <- which(cumsum(explain) > 70)[1]
  }


  # Correlation coefficients between quantiles and data
  pc_rq <- NULL
  for (i in 1:k){
    pc_rq[i] <- cor(qi, z.sort[,i])
  }
  pc_rq <- as.data.frame(pc_rq)
  colnames(pc_rq) <- "Correlation Coeficient Value"
  rownames(pc_rq) <- c(paste0("PC", 1:k))
  pc_rq <- round(pc_rq, 4)


  # Critical Value Table
  if (n %in% CVtable$N){
    crit <- subset(CVtable, subset = (N == n), select = as.character(level))
  } else {
    num_lower <- utils::tail(subset(CVtable, subset = (N < n)), 1)
    num_upper <- utils::head(subset(CVtable, subset = (N > n)), 1)
    pos_ratio <- (n - num_lower$N) / (num_upper$N - num_lower$N)
    crit <- num_lower[, as.character(level)] + (num_upper[, as.character(level)] - num_lower[, as.character(level)]) * pos_ratio
  }


  # Probability Plot Correlation Coefficient Test for multivariate normality
  test_res <- NULL
  res <- NULL
  for (i in 1:k){
    test_res[[i]] <- ifelse(pc_rq[i, ] > crit, "Accept", "Reject")
    res[[i]] <- c(PC = paste0("PC", i), Corr.coef = round(pc_rq[i, ], 4), Crit.value = round(crit, 4), Sig.level = level, Test.res = test_res[i])
  }
  final.res <- ifelse(!any(grepl("Reject", test_res)), "Accept", "Reject")
  res[[i + 1]] <- c("ALL", NA, NA, NA, final.res)
  res_df <- do.call(rbind, res)
  res_df <- data.frame(res_df)


  # Chi-Square Q-Q plot(PC scores)
  PC_plot <- list()
  for (i in 1:k) {
    df <- data.frame(qi, PC = z.sort[, i])

    PC_plot[[i]] <- ggplot(df, aes(x = qi, y = PC)) +
      geom_point() +
      geom_smooth(formula = 'y ~ x', method = "lm", se = FALSE, colour = "blue", linewidth = 0.7) +
      labs(title = paste0(i, "st PC : ", round(pc_rq[i, ], 4)), x = "Quantile", y = paste0("PC", i))
  }

  grobs <- lapply(PC_plot, ggplotGrob)

  Srivastava_plot <- grid.arrange(grobs = grobs, nrow = ceiling(k / 2), ncol = 2,
                                  top = "Srivastava Plot Correlation Coefficient for Multivariate Normality")


  result <- list(Srivastava.QQplot = Srivastava_plot, data.cnt = n,
                 explain.ratio = round(explain, 4), critical.value = crit, result = res_df)
  return(result)

}


utils::globalVariables(c("N", "PC")) # Because of using critical value table & ggplot2
