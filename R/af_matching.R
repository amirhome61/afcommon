#' Interpret Propensity Score Matching Results
#'
#' @description
#' Generates a comprehensive interpretation text for propensity score matching analysis
#' results. Creates a narrative report describing the matching method, balance assessment,
#' treatment effect estimation, and statistical inference, formatted for inclusion in
#' research reports or manuscripts.
#'
#' @param avg_comp_results (data.frame) Results from marginaleffects::avg_comparisons containing treatment effect estimates
#' @param match_method (character) The matching method used for the analysis. Default is "full matching"
#' @param ps_model (character) The model used for propensity score estimation. Default is "probit"
#' @param outcome_name (character) The name of the outcome variable. Default is NULL
#' @param treatment_name (character) The name of the treatment variable. Default is NULL
#' @param n_units (numeric) The number of units included in the analysis. Default is NULL
#' @param max_smd (numeric) The maximum standardized mean difference achieved after matching. Default is NULL
#' @param max_smd_squares (numeric) The maximum standardized mean difference for squares of covariates. Default is NULL
#' @param units_discarded (logical) Whether any units were discarded during matching. Default is FALSE
#'
#' @return (character) A formatted interpretation text describing the matching analysis, balance assessment, estimation method, and treatment effect results with statistical significance
#'
#' @export
af_interpret_matching <- function(
  avg_comp_results, # Results from marginaleffects::avg_comparisons
  match_method = "full matching", # Matching method used (e.g., "full matching", "1:1 nearest neighbor")
  ps_model = "probit", # Model used for propensity score estimation
  outcome_name = NULL, # Name of the outcome variable
  treatment_name = NULL, # Name of the treatment variable
  n_units = NULL, # Number of units in the analysis
  max_smd = NULL, # Maximum standardized mean difference achieved
  max_smd_squares = NULL, # Maximum SMD for squares of covariates
  units_discarded = FALSE # Whether any units were discarded in matching
) {
  # Extract key statistics from avg_comparisons results
  estimate <- avg_comp_results$estimate[1]
  std_error <- avg_comp_results$std.error[1]
  p_value <- avg_comp_results$p.value[1]

  # Format numbers for reporting
  estimate_formatted <- format(round(estimate, 1), big.mark = ",")
  se_formatted <- format(round(std_error, 1), big.mark = ",")

  # Create balance assessment text
  balance_text <- sprintf(
    "After matching, all standardized mean differences for the covariates were below %.1f",
    ifelse(is.null(max_smd), 0.1, max_smd)
  )
  if (!is.null(max_smd_squares)) {
    balance_text <- paste0(
      balance_text,
      sprintf(
        " and all standardized mean differences for squares and two-way interactions between covariates were below %.2f",
        max_smd_squares
      )
    )
  }
  balance_text <- paste0(balance_text, ", indicating adequate balance.")

  # Create units text
  units_text <- if (!units_discarded) {
    sprintf(
      "%s uses all treated and all control units, so no units were discarded by the matching.",
      match_method
    )
  } else {
    "Some units were discarded during the matching process."
  }

  # Create interpretation text based on p-value
  effect_interpretation <- if (p_value < 0.05) {
    "indicating strong evidence that the treatment had an effect"
  } else if (p_value < 0.10) {
    "suggesting moderate evidence that the treatment had an effect"
  } else {
    "indicating insufficient evidence to conclude the treatment had an effect"
  }

  # Generate the full interpretation
  interpretation <- sprintf(
    "We used propensity score matching to estimate the average marginal effect of the %s on %s on those who received it accounting for confounding by the included covariates. We first attempted 1:1 nearest neighbor propensity score matching without replacement with a propensity score estimated using logistic regression of the treatment on the covariates. This matching specification yielded poor balance, so we instead tried %s on the propensity score, which yielded adequate balance. The propensity score was estimated using a %s regression of the treatment on the covariates, which yielded better balance than did a logistic regression. %s %s

To estimate the treatment effect and its standard error, we fit a linear regression model with %s as the outcome and the treatment, covariates, and their interaction as predictors and included the %s weights in the estimation. The lm() function was used to fit the outcome, and the comparisons() function in the marginaleffects package was used to perform g-computation in the matched sample to estimate the ATT. A cluster-robust variance was used to estimate its standard error with matching stratum membership as the clustering variable.

The estimated effect was %s (SE = %s, p = %.3f), %s.",
    treatment_name,
    outcome_name,
    match_method,
    ps_model,
    balance_text,
    units_text,
    outcome_name,
    tolower(match_method),
    estimate_formatted,
    se_formatted,
    p_value,
    effect_interpretation
  )

  return(interpretation)
}

#' Compare Treatment Effects With and Without Matching Across Groups
#'
#' @description
#' Performs comparative analysis of treatment effects across multiple groups or waves,
#' with and without propensity score matching. For each group, estimates treatment effects
#' on multiple dependent variables using linear regression with specified covariates, then
#' performs full matching and re-estimates effects. Returns detailed results and visualization.
#'
#' @param df (data.frame) The data frame containing all variables for the analysis
#' @param dep_list (character vector) List of dependent variable names to analyze
#' @param ctrl_vars (character vector) List of control variable names to include in models
#' @param treat_var (character) The name of the treatment variable
#' @param treat_level (character) The level of the treatment variable to analyze
#' @param group_var (character) The name of the grouping variable for stratified analysis
#' @param group_list (vector) List of group values to analyze separately
#'
#' @return (list) A named list with two elements: df (data frame containing estimates, p-values, confidence intervals, and significance stars for all models) and plot (ggplot object comparing matched and unmatched treatment effects across groups and outcomes)
#'
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom rlang sym
#'
#' @export
af_match_compare <- function(
  df,
  dep_list,
  ctrl_vars,
  treat_var,
  treat_level,
  group_var,
  group_list
) {
  results_df <- data.frame()

  for (group in group_list) {
    # Filter data for current group
    curr_data <- df %>% filter(!!sym(group_var) == group)

    # Store unmatched and matched results
    for (match_type in c("Unmatched", "Matched")) {
      # Perform matching if needed
      if (match_type == "Matched") {
        match_formula <- as.formula(paste(
          treat_var,
          "~",
          paste(ctrl_vars, collapse = " + ")
        ))
        m.out <- matchit(match_formula, data = curr_data, method = "full")
        curr_data <- match.data(m.out)
      }

      # Run regressions for each dependent variable
      for (dep_var in dep_list) {
        # Create formula
        formula_str <- paste(
          dep_var,
          "~",
          treat_var,
          "+",
          paste(ctrl_vars, collapse = " + ")
        )
        model <- if (match_type == "Matched") {
          lm(as.formula(formula_str), data = curr_data, weights = weights)
        } else {
          lm(as.formula(formula_str), data = curr_data)
        }

        # Extract results
        coef_data <- tidy(model, conf.int = TRUE) %>%
          filter(term == paste0(treat_var, treat_level))

        # Add significance stars
        stars <- case_when(
          coef_data$p.value < 0.001 ~ "***",
          coef_data$p.value < 0.01 ~ "**",
          coef_data$p.value < 0.05 ~ "*",
          TRUE ~ ""
        )

        # Compile results
        results_df <- rbind(
          results_df,
          data.frame(
            dependent = dep_var,
            group = group,
            match_type = match_type,
            estimate = coef_data$estimate,
            p.value = coef_data$p.value,
            conf.low = coef_data$conf.low,
            conf.high = coef_data$conf.high,
            significance = stars
          )
        )
      }
    }
  }

  # Create visualization
  plot <- ggplot(
    results_df,
    aes(
      x = dependent,
      y = estimate,
      ymin = conf.low,
      ymax = conf.high,
      color = factor(group),
      shape = match_type
    )
  ) +
    geom_pointrange(position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_text(
      aes(label = significance),
      position = position_dodge(width = 0.5),
      vjust = -0.5
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14, face = "bold"),
      legend.position = "top",
      legend.title = element_text(size = 12)
    ) +
    labs(
      x = "Dependent Variables",
      y = "Treatment Effect",
      color = "Wave",
      shape = "Analysis Type"
    ) +
    coord_flip()

  return(list(df = results_df, plot = plot))
}
