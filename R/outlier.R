#' Outliers Detection
#'
#' Using the mardia test, outliers are detected based on skewness and kurtosis. However, outliers don't detect more than half of the total observation data.(Can be modified with the \code{lim} option.)
#'
#' @param data A numeric matrix or data frame.
#' @param lim The number of outliers detected can be limited. If 0 is entered, detection is possible up to half of the data. (default = \code{0})
#' @param level The significance level of the skewness and kurtosis statistics of the "\code{madiatest}" function. (default = \code{0.05})
#' @param option \code{"skew"} refers to skewness, \code{"kurt"} refers to kurtosis, \code{"all"} refers to skewness and kurtosis. Outliers are detected until the corresponding option in the \code{mardiatest} is “Accept”. (default = \code{"all"})
#'
#'
#' @return
#'    \item{modified.data}{The modified data without outliers.}
#'    \item{modified.mvn}{The modified Mardia test result without outliers.}
#'    \item{outlier.num}{The position of outliers.}
#'    \item{outlier.cnt}{Total number of outliers.}
#'
#'
#' @usage
#' outlier(data,
#'         lim = 0,
#'         level = 0.05,
#'         option = "all")
#'
#'
#' @references
#' Jobson, J. D.(1992). Applied Multivariate Data Analysis, Springer-Verlag, New York.
#'
#'
#' @seealso \code{\link{mardiatest}}
#' @keywords Outliers
#' @examples
#' data(wine)
#' class2.wine <- subset(wine, class == 2)[, -1]
#' outlier(class2.wine, lim = 0, level = 0.05, option = "all")
#'
#'
#' @export

outlier <- function(data, lim = 0, level = 0.05, option = "all"){

  if (any(is.na(data))) {
    stop("NA values found in the data.", call. = FALSE)
  }

  # lim must be integer
  if (!is.numeric(lim) || lim != floor(lim)) {
    stop("'lim' must be an integer value.", call. = FALSE)
  }


  data <- as.data.frame(data)
  re.data <- data
  rownames(re.data) <- NULL
  rm.data.num <- NULL

  if (option == "skew"){
    res_num <- 1
  } else if (option == "kurt"){
    res_num <- 2
  } else if (option == "all"){
    res_num <- 3
  } else {
    stop("Use \"option\" as \"skew\", \"kurt\" and \"all\".", call. = FALSE)
  }

  # First MVN result
  is.mvn <- mardiatest(data, level = level, showplot = FALSE, showoutlier = FALSE)$mult.nomality$Test.result[res_num]

  # Options related to 'lim'
  half.n <- nrow(data) / 2
  cnt <- 0

  while (is.mvn == "Reject"){

    n <- nrow(re.data)
    p <- ncol(re.data)
    z <- as.matrix(re.data)

    if (lim == cnt && lim != 0){
      break
    } else if (lim == 0){
        if (n < half.n) {
          warning("Outliers that exceed half of the data size are detected.")
          break
        }

    }

    # covariance S
    I = diag(n)
    J = matrix(1, n, n)
    H = I - (1 / n) * J
    Y = H %*% z
    S = t(Y) %*% Y / (n - 1)

    # Mean Vector and Covariance Matrix Excluding the i-th Observation
    I_1 = diag(n - 1)
    J_1 = matrix(1, n - 1, n - 1)
    H_1 = I_1 - (1 / (n - 1)) * J_1

    # Calculate r_i^2
    ri = sapply(1:n, function(i) {
      Yi = H_1 %*% z[-i, ]
      Si = t(Yi) %*% Yi / (n - 2)
      det(Si) / det(S)
    })


    out_num <- order(ri)[1]
    final.out.num <- as.numeric(rownames(re.data[out_num, ]))
    rm.data.num <- append(rm.data.num, final.out.num)
    re.data <- re.data[-out_num, ]

    is.mvn <- mardiatest(re.data, level = level, showplot = FALSE, showoutlier = FALSE)$mult.nomality$Test.result[res_num]

    cnt <- cnt + 1

  }

  final.mvn <- mardiatest(re.data, level = level, showplot = FALSE, showoutlier = FALSE)$mult.nomality

  result <- list(modified.data = re.data, modified.mvn =  final.mvn,
                 outlier.num = rm.data.num, outlier.cnt = length(rm.data.num))
  return(result)

}
