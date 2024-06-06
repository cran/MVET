#' Various Multivariate Anova(VManova)
#'
#' Perform various types of multivariate analysis of variance (MANOVA) that satisfy tests of multivariate normality and homogeneity of covariance matrices.
#'
#' @param data A numeric matrix or data frame. If data frames, group(class) column can be a factor or a string.
#' @param grp1.name The name of the first group (or class) column in the input data, specified as a \code{string}.
#' @param grp2.name The name of the second group (or class) column in the input data, specified as a \code{string}. Used to represent the second group(class) in a two-way MANOVA.
#' @param way The type of MANOVA to perform ("\code{one}" for one-way or "\code{two}" for two-way). (default = "\code{one}")
#' @param method The method for MANOVA analysis. "\code{Wilks}" represents Wilks' lambda, "\code{LH}" represents Lawley-Hotelling trace, "\code{Pillai}" represents Pillai-Bartlett trace, "\code{Roy}" represents Roy's largest root, and "\code{all}" represents all methods. (default is "\code{all}")
#' @param plot.scale If \code{TRUE}, the data will be scaled before calculating mean values and used in the plot. It has no direct effect on the MANOVA analysis itself. (default plot.scale = \code{FALSE})
#'
#'
#' @return
#'    \item{Mean.val.plot}{Plot the mean value parallel coordinates, representing the two samples using the mean values for each variable.}
#'    \item{One.all}{Outputs the results of a one-way MANOVA test. It displays the degrees of freedom (Df1, Df2) of the F-distribution, statistics for Wilks, Lawley-Hotelling, Pillai, and Roy, the F-distribution test statistic, and the significance level in that order.}
#'    \item{Two.all}{Outputs the results of a two-way MANOVA test. It displays the degrees of freedom (Df1, Df2) of the F-distribution, statistics for Wilks, Lawley-Hotelling, Pillai, and Roy, the F-distribution test statistic, and the significance level in that order.}
#'
#' @usage
#' VManova(data,
#'         grp1.name,
#'         grp2.name,
#'         way = "one",
#'         method = "all",
#'         plot.scale = FALSE)
#'
#'
#' @references
#' Rencher, A. C., & Christensen, W. F. (2002). Methods of Multivariate Analysis. John Wiley & Sons, Inc., New York.
#'
#'
#' @seealso \code{\link{mardiatest} for multivariate normality (Includes outlier remove)}
#' @seealso \code{\link{PPCCtest} for multivariate normality}
#' @seealso \code{\link{SPCCtest} for multivariate normality}
#' @seealso \code{\link{boxMtest} for homogeneity of covariance matrices}
#' @keywords manova
#'
#'
#' @examples
#' data(wine)
#'
#' ## one way
#' VManova(wine, grp1.name = "class", way = "one", method = "all", plot.scale = TRUE)
#'
#' ## two way
#' newwine <- wine
#' # (1: low, 2: medium, 3: high)
#' newwine$v4 <- ifelse(wine$v4 <= 17, 1,
#'                      ifelse(wine$v4 <= 22, 2, 3))
#' VManova(newwine, grp1.name = "class", grp2.name = "v4",
#'         way = "two", method = "all", plot.scale = TRUE)
#'
#'
#' @import stats
#' @import ggplot2
#' @import grDevices
#' @import gridExtra
#'
#' @export

VManova <- function(data, grp1.name, grp2.name, way = "one", method = "all", plot.scale = FALSE){

  if (any(is.na(data))) {
    stop("NA values found in the data.", call. = FALSE)
  }

  if (is.null(data[[grp1.name]])){
    stop("Input data does not match the group(or class) name 'grp1.name'.", call. = FALSE)
  }

  way_type = c("one", "two")
  if (!way %in% way_type) {
    stop("Please choose from 'one', 'two' for the 'way' parameter.", call. = FALSE)
  }

  method_type = c("Wilks", "LH", "Pillai", "Roy", "all")
  if (!method %in% method_type) {
    stop("Please choose from 'Wilks', 'LH', 'Pillai', 'Roy', 'all' for the 'method' parameter.", call. = FALSE)
  }


  # Wilks statistic
  Wilks <- function(SST, SSE, p, H_df, E_df) {
    wilks_stat <- det(SSE) / det(SSE + SST)
    t <- sqrt((p^2 * H_df^2 - 4) / (p^2 + H_df^2 - 5))
    df1 <- p * H_df
    df2 <- (E_df + H_df - 0.5 * (p + H_df + 1)) * t - 0.5 * (p * H_df - 2)
    f_stat <- (1 - wilks_stat^(1 / t)) / wilks_stat^(1 / t) * df2 / df1
    p_value <- pf(f_stat, df1, df2, lower.tail = FALSE)

    return(c(round(wilks_stat, 6), round(df1, 2),
             round(df2, 2), round(f_stat, 6), signif(p_value, 10)))
  }


  # LH(Lawley-Hotelling) statistic
  LH <- function(SST, SSE, p, H_df, E_df) {
    LH_stat <- sum(diag(solve(SSE) %*% SST))
    s <- min(H_df, p)
    m <- .5 * (abs(H_df - p) - 1)
    nn <- .5 * (E_df - p - 1)
    df1 <- s * (2 * m + s + 1)
    df2 <- 2 * (s * nn + 1)
    f_stat <- (df2 * LH_stat) / (s * df1)
    p_value <- pf(f_stat, df1, df2, lower.tail = FALSE)

    return(c(round(LH_stat, 6), round(df1, 2),
             round(df2, 2), round(f_stat, 6), signif(p_value, 10)))
  }


  # Pillai statistic
  Pillai <- function(SST, SSE, p, H_df, E_df) {
    pillai_stat <- sum(diag(SST %*% solve(SST + SSE)))
    s <- min(H_df, p)
    d <- max(H_df, p)
    df1 <- s * d
    df2 <- s * (E_df - p + s)
    f_stat <- (df2 * pillai_stat) / (df1 * (s - pillai_stat))
    p_value <- pf(f_stat, df1, df2, lower.tail = FALSE)

    return(c(round(pillai_stat, 6), round(df1, 2),
             round(df2, 2), round(f_stat, 6), signif(p_value, 10)))
  }


  # Roy statistic
  Roy <- function(SST, SSE, p, H_df, E_df) {
    roy_stat <- max(Re(eigen(solve(SSE) %*% SST)$values))
    d <- max(H_df, p)
    df1 <- d
    df2 <- E_df - d + H_df
    f_stat <- (df2 * roy_stat) / df1
    p_value <- pf(f_stat, df1, df2, lower.tail = FALSE)

    return(c(round(roy_stat, 6), round(df1, 2),
             round(df2, 2), round(f_stat, 6), signif(p_value, 10)))
  }



  # one way
  if (way == "one"){

    if (!missing(grp2.name) && !is.null(grp2.name)){
      warning("'grp2.name' is ignored in One way MANOVA.", call. = FALSE)
    }

    dep_data <- as.matrix(data[, !colnames(data) %in% grp1.name])

    total_mean <- colMeans(dep_data)

    grp_means <- aggregate(dep_data, by = list(data[[grp1.name]]), FUN = mean)

    N <- nrow(dep_data)
    G <- nrow(grp_means)
    P <- ncol(dep_data)
    H_df <- G - 1
    E_df <- N - G
    df <- c(H_df, E_df)


    # Show Mean Vector Parallel Coordinates Plot
    res_plot <- .mean_parallel_plot(data = data, grp.name = grp1.name, scale = plot.scale)


    # SST
    A <- matrix(0, ncol = P, nrow = P)
    for (g in 1:G) {
      n <- nrow(data[data[[grp1.name]] == grp_means[g, 1], ])
      mean_diff <- as.matrix(grp_means[g, -1] - total_mean)
      A <- A + n * (t(mean_diff) %*% mean_diff)
    }


    # SSE
    W <- matrix(0, ncol = P, nrow = P)
    for (g in 1:G) {
      n <- nrow(data[data[[grp1.name]] == grp_means[g, 1], ])
      deviations <- cov(dep_data[data[[grp1.name]] == grp_means[g, 1], ])
      W <- W + (n - 1) * deviations
    }



    # Wilks lamda
    if (method == "Wilks" || method == "all"){

      wilks <- Wilks(SST = A, SSE = W, p = P, H_df = H_df, E_df = E_df)
      wilks_df <- as.data.frame(t(wilks))
      colnames(wilks_df) <- c("Wilks.stat", "F.df1", "F.df2", "F.stat", "P.value")
      wilks_df1 <- rbind.data.frame(wilks_df, error = c("", "", "", "", ""))
      res1 <- cbind(df, wilks_df1)
      rownames(res1) <- c(grp1.name, "error")
      result <- list(Mean.val.plot = res_plot, One.Wilks = res1)

    }


    # LH(Lawley-Hotelling)
    if (method == "LH" || method == "all"){

      lh <- LH(SST = A, SSE = W, p = P, H_df = H_df, E_df = E_df)
      lh_df <- as.data.frame(t(lh))
      colnames(lh_df) <- c("LH.stat", "F.df1", "F.df2", "F.stat", "P.value")
      lh_df1 <- rbind.data.frame(lh_df, error = c("", "", "", "", ""))
      res2 <- cbind(df, lh_df1)
      rownames(res2) <- c(grp1.name, "error")
      result <- list(Mean.val.plot = res_plot, One.LH = res2)

    }


    # Pillai
    if (method == "Pillai" || method == "all"){

      pillai <- Pillai(SST = A, SSE = W, p = P, H_df = H_df, E_df = E_df)
      pillai_df <- as.data.frame(t(pillai))
      colnames(pillai_df) <- c("Pillai.stat", "F.df1", "F.df2", "F.stat", "P.value")
      pillai_df1 <- rbind.data.frame(pillai_df, error = c("", "", "", "", ""))
      res3 <- cbind(df, pillai_df1)
      rownames(res3) <- c(grp1.name, "error")
      result <- list(Mean.val.plot = res_plot, One.Pillai = res3)

    }


    # Roy
    if (method == "Roy" || method == "all"){

      roy <- Roy(SST = A, SSE = W, p = P, H_df = H_df, E_df = E_df)
      roy_df <- as.data.frame(t(roy))
      colnames(roy_df) <- c("Roy.stat", "F.df1", "F.df2", "F.stat", "P.value")
      roy_df1 <- rbind.data.frame(roy_df, error = c("", "", "", "", ""))
      res4 <- cbind(df, roy_df1)
      rownames(res4) <- c(grp1.name, "error")
      result <- list(Mean.val.plot = res_plot, One.Roy = res4)

    }

    if (method == "all"){

      names(wilks_df) <- c("Statistic", "F.df1", "F.df2", "F.stat", "P.value")
      names(lh_df) <- c("Statistic", "F.df1", "F.df2", "F.stat", "P.value")
      names(pillai_df) <- c("Statistic", "F.df1", "F.df2", "F.stat", "P.value")
      names(roy_df) <- c("Statistic", "F.df1", "F.df2", "F.stat", "P.value")
      fin.res1 <- rbind(wilks_df, lh_df, pillai_df, roy_df, error = c("", "", "", "", ""))
      fin.res2 <- cbind(DF = c(df[1], df[1], df[1], df[1], df[2]), fin.res1)
      rownames(fin.res2) <- c("Wilks", "Lawley-Hotelling", "Pillai", "Roy", "error")
      result <- list(Mean.val.plot = res_plot, One.all = fin.res2)

    }

  }


  # two way
  if (way == "two"){

    if (is.null(data[[grp2.name]])){
      stop("Input data does not match the group(or class) name 'grp2.name'.", call. = FALSE)
    }

    grp.names <- c(grp1.name, grp2.name)
    dep_data <- as.matrix(data[, !colnames(data) %in% grp.names])
    grp1 <- factor(data[[grp1.name]])
    grp2 <- factor(data[[grp2.name]])

    G1 <- length(levels(grp1))
    G2 <- length(levels(grp2))
    N <- nrow(dep_data)
    I_n <- G1 * G2
    n <- N / I_n
    P <- ncol(dep_data)

    HA_df <- G1 - 1
    HB_df <- G2 - 1
    HI_df <- HA_df * HB_df
    E_df <- G1 * G2 * (n - 1)
    df <- c(HA_df, HB_df, HI_df, E_df)


    # Show Mean Vector Parallel Coordinates Plot
    adj_data1 <- data[, !colnames(data) %in% grp2.name]
    adj_data2 <- data[, !colnames(data) %in% grp1.name]
    res_grp1_plot <- .mean_parallel_plot(data = adj_data1, grp.name = grp1.name, scale = plot.scale)
    res_grp2_plot <- .mean_parallel_plot(data = adj_data2, grp.name = grp2.name, scale = plot.scale)
    res_plot <- grid.arrange(res_grp1_plot, res_grp2_plot, nrow = 1, ncol = 2,
                             top = "Two-Way Mean Vector Parallel Coordinates Plot")


    # fitting
    mod <- lm(dep_data ~ grp1 * grp2)

    rownames_vector <- rownames(mod$effects)
    filtered_vector <- rownames_vector[nchar(rownames_vector) > 0]
    effect <- mod$effects[filtered_vector,]
    final_effect <- subset(effect, rownames(effect) != "(Intercept)")

    # SSA
    A <- matrix(0, ncol = P, nrow = P)
    for (i in 1:HA_df){
      cross_prod1 <- final_effect[i, ] %*% t(final_effect[i, ])
      A <- A + cross_prod1
    }

    # SSB
    B <- matrix(0, ncol = P, nrow = P)
    for (j in (HA_df + 1):(HA_df + HB_df)){
      cross_prod2 <- final_effect[j, ] %*% t(final_effect[j, ])
      B <- B + cross_prod2
    }

    # SSI
    I <- matrix(0, ncol = P, nrow = P)
    for (k in (HA_df + HB_df + 1):nrow(final_effect)){
      cross_prod3 <- final_effect[k, ] %*% t(final_effect[k, ])
      I <- I + cross_prod3
    }

    # SSE
    W <- t(mod$residuals) %*% mod$residuals


    # Wilks lamda
    if (method == "Wilks" || method == "all"){

      A_wilks <- Wilks(SST = A, SSE = W, p = P, H_df = HA_df, E_df = E_df)
      B_wilks <- Wilks(SST = B, SSE = W, p = P, H_df = HB_df, E_df = E_df)
      I_wilks <- Wilks(SST = I, SSE = W, p = P, H_df = HI_df, E_df = E_df)
      wilks <- rbind(A_wilks, B_wilks, I_wilks)
      rownames(wilks) <- c(grp1.name, grp2.name, paste0(grp1.name, ":", grp2.name))
      colnames(wilks) <- c("Wilks.stat", "F.df1", "F.df2", "F.stat", "P.value")

      wilks1 <- rbind.data.frame(wilks, error = c("", "", "", "", ""))
      res1 <- cbind(df, wilks1)
      result <- list(Mean.val.plot = res_plot, Two.Wilks = res1)

    }


    # LH(Lawley-Hotelling)
    if (method == "LH" || method == "all"){

      A_lh <- LH(SST = A, SSE = W, p = P, H_df = HA_df, E_df = E_df)
      B_lh <- LH(SST = B, SSE = W, p = P, H_df = HB_df, E_df = E_df)
      I_lh <- LH(SST = I, SSE = W, p = P, H_df = HI_df, E_df = E_df)
      lh <- rbind(A_lh, B_lh, I_lh)
      rownames(lh) <- c(grp1.name, grp2.name, paste0(grp1.name, ":", grp2.name))
      colnames(lh) <- c("LH.stat", "F.df1", "F.df2", "F.stat", "P.value")

      lh1 <- rbind.data.frame(lh, error = c("", "", "", "", ""))
      res2 <- cbind(df, lh1)
      result <- list(Mean.val.plot = res_plot, Two.LH = res2)

    }


    # Pillai
    if (method == "Pillai" || method == "all"){

      A_pillai <- Pillai(SST = A, SSE = W, p = P, H_df = HA_df, E_df = E_df)
      B_pillai <- Pillai(SST = B, SSE = W, p = P, H_df = HB_df, E_df = E_df)
      I_pillai <- Pillai(SST = I, SSE = W, p = P, H_df = HI_df, E_df = E_df)
      pillai <- rbind(A_pillai, B_pillai, I_pillai)
      rownames(pillai) <- c(grp1.name, grp2.name, paste0(grp1.name, ":", grp2.name))
      colnames(pillai) <- c("Pillai.stat", "F.df1", "F.df2", "F.stat", "P.value")

      pillai1 <- rbind.data.frame(pillai, error = c("", "", "", "", ""))
      res3 <- cbind(df, pillai1)
      result <- list(Mean.val.plot = res_plot, Two.Pillai = res3)

    }


    # Roy
    if (method == "Roy" || method == "all"){

      A_roy <- Roy(SST = A, SSE = W, p = P, H_df = HA_df, E_df = E_df)
      B_roy <- Roy(SST = B, SSE = W, p = P, H_df = HB_df, E_df = E_df)
      I_roy <- Roy(SST = I, SSE = W, p = P, H_df = HI_df, E_df = E_df)
      roy <- rbind(A_roy, B_roy, I_roy)
      rownames(roy) <- c(grp1.name, grp2.name, paste0(grp1.name, ":", grp2.name))
      colnames(roy) <- c("Roy.stat", "F.df1", "F.df2", "F.stat", "P.value")

      roy1 <- rbind.data.frame(roy, error = c("", "", "", "", ""))
      res4 <- cbind(df, roy1)
      result <- list(Mean.val.plot = res_plot, Two.Roy = res4)

    }


    if (method == "all"){

      result <- list(Mean.val.plot = res_plot, Two.Wilks = res1, Two.LH = res2,
                     Two.Pillai = res3, Two.Roy = res4)

    }

  }

  return(result)

}

