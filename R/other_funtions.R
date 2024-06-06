#' Mean Value Parallel Coordinates Plot (Use to HT2test & VManova)
#'
#' @param data A numeric matrix or data frame. If data frame, group(class) column can be a factor or a string.
#' @param grp.name The name of a column of \code{string} representing groups(classes) in the input data.
#' @param scale If \code{TRUE}, the data will be scaled before calculating mean values and used in the plot. (default scale = \code{FALSE})
#'
#' @return Mean Value Parallel Coordinates Plot
#'
#'
#'
.mean_parallel_plot <- function(data, grp.name, scale = FALSE) {

  if (!(grp.name %in% colnames(data))) {
    stop("Input data does not match the group(or class) name.")
  }

  if (!is.matrix(data)) {
    data <- as.data.frame(data)
  }

  data_vars <- data[, !colnames(data) %in% grp.name]

  if (scale) {
    data_vars <- scale(data_vars)
  }

  data_colmean <- aggregate(data_vars, by = list(data[[grp.name]]), FUN = mean)
  data_nogrp <- data_colmean[, -1]
  grp_name <- factor(data_colmean[, 1])


  if (length(levels(grp_name)) > 25) {
    stop("The number of groups (classes) in your data is limited to a maximum of 25.")
  }


  # plot data
  data_long <- data.frame(
    Group = rep(grp_name, each = ncol(data_nogrp)),
    Variable = rep(colnames(data_nogrp), times = nrow(data_nogrp)),
    Value = as.vector(t(data_nogrp))
  )

  data_long$Variable <- factor(data_long$Variable, levels = colnames(data_vars))

  group_levels <- levels(data_long$Group)
  num_groups <- length(group_levels)

  colors <- rainbow(num_groups)
  shapes <- seq(0, num_groups - 1)


  # ggplot
  result <- ggplot(data_long, aes(x = Variable, y = Value, group = Group, color = Group, shape = Group)) +
    geom_line(size = 1.1, alpha = 0.8) +
    geom_point(size = 3) +
    scale_color_manual(values = colors) +
    scale_shape_manual(values = shapes) +
    theme(legend.position = "bottom") +
    labs(title = paste("Mean Value Parallel Coordinates Plot", if (scale) "(Scaled)" else ""),
         x = "Variables", y = "Mean Values",
         color = grp.name, shape = grp.name)

  return(result)

}

utils::globalVariables(c("Variable", "Value", "Group")) # Because of using ggplot2
