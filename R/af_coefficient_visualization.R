#' Extract Coefficient and Confidence Interval
#'
#' @description
#' Extracts the coefficient estimate and confidence interval for a specified covariate
#' from a fitted model. Returns a list containing the model name, covariate name,
#' coefficient value, and the lower and upper confidence interval bounds.
#'
#' @param m (model object) A fitted model object with coef() and confint() methods
#' @param model_name (character) The name identifier for the model
#' @param cov_name (character) The name of the covariate to extract
#'
#' @return (list) A named list with elements: model_name, cov_name, coef, conf1 (lower CI), and conf2 (upper CI)
#'
#' @export
af_get_coef_and_ci <- function(m, model_name, cov_name) {
  # Extract the coefficient and confidence interval for the specified covariate
  coef_val <- coef(m)[cov_name]
  conf_interval <- confint(m)[cov_name, ]
  conf1 <- conf_interval[1]
  conf2 <- conf_interval[2]

  # Create a list with the required information
  result_list <- list(
    model_name = model_name,
    cov_name = cov_name,
    coef = coef_val,
    conf1 = conf1,
    conf2 = conf2
  )

  return(result_list)
}

#' Create Coefficient and Confidence Interval Table
#'
#' @description
#' Extracts coefficients and confidence intervals for a specified covariate across
#' multiple fitted models. Iterates through a list of models and compiles results into
#' a data frame with one row per model, including model name, covariate name, coefficient,
#' and confidence interval bounds.
#'
#' @param models (list) A named list of fitted model objects
#' @param cov_name (character) The name of the covariate to extract from all models
#'
#' @return (data.frame) A data frame with columns: model_name, cov_name, coef, conf1, and conf2
#'
#' @export
af_coef_and_ci_table <- function(models, cov_name) {
  coef_ci_table <- NULL

  for (i in 1:length(models)) {
    m <- models[[i]]
    model_name <- names(models[i])
    coef_ci_line <- unlist(af_get_coef_and_ci(m, model_name, cov_name))
    names(coef_ci_line) <- NULL
    coef_ci_table <- rbind(coef_ci_table, coef_ci_line)
  }

  rownames(coef_ci_table) <- NULL
  colnames(coef_ci_table) <- c(
    "model_name",
    "cov_name",
    "coef",
    "conf1",
    "conf2"
  )
  return(as.data.frame(coef_ci_table))
}

#' Plot Coefficient and Confidence Interval
#'
#' @description
#' Creates a forest plot style visualization displaying coefficients and confidence
#' intervals from multiple models. The plot shows point estimates as squares with
#' horizontal lines representing confidence intervals, a vertical reference line at zero,
#' and numeric labels for coefficient values.
#'
#' @param coef_ci_table (data.frame) A data frame with columns: model_name, coef, conf1, and conf2
#'
#' @return (ggplot) A ggplot object displaying the coefficient estimates and confidence intervals
#'
#' @import ggplot2
#' @import scales
#'
#' @export
af_coef_and_ci_plot <- function(coef_ci_table) {
  # Create a data frame with the values you want to plot
  plot_data <- data.frame(
    model_name = coef_ci_table$model_name,
    coef = coef_ci_table$coef,
    conf1 = coef_ci_table$conf1,
    conf2 = coef_ci_table$conf2
  )

  # Convert 'value' to numeric
  plot_data$coef <- as.numeric(plot_data$coef)
  plot_data$conf1 <- as.numeric(plot_data$conf1)
  plot_data$conf2 <- as.numeric(plot_data$conf2)

  # Create the plot
  p <- ggplot(plot_data, aes(x = coef, y = model_name)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    geom_segment(aes(xend = conf1, yend = model_name), color = "black") +
    geom_segment(aes(xend = conf2, yend = model_name), color = "black") +
    geom_point(aes(x = coef), shape = 15, size = 3, color = "black") +
    geom_text(aes(label = sprintf("%.2f", coef)), nudge_y = 0.4) + # Add this line for text labels
    labs(x = "", y = "") +
    scale_x_continuous(labels = scales::number_format(digits = 2))

  return(p)
}
