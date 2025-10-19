af_mediation_analysis <- function(df, outcome_var_name, mediator_var_name, treatment_var_name) {
  
  # Step 1: Fit the mediator model
  mediator_reg_formula <- af_build_formula(response = mediator_var_name,
                                           predictors = c(treatment_var_name), 
                                           as_string = TRUE)
  mediator_model <- lm(as.formula(mediator_reg_formula), data = df)
  mediator_model$call<-str2lang(paste0("lm(", mediator_reg_formula, ",data=df)"))
  
  # Step 2: Fit the outcome model
  outcome_reg_formula <- af_build_formula(response = outcome_var_name, 
                                          predictors = c(treatment_var_name, 
                                                         mediator_var_name), 
                                          as_string = TRUE)
  outcome_model <- lm(as.formula(outcome_reg_formula), data = df)
  outcome_model$call<-str2lang(paste0("lm(", outcome_reg_formula, ",data=df)"))
  
  # Step 3: Conduct mediation analysis
  med_analysis <- mediation::mediate(mediator_model, outcome_model,
                                     treat = treatment_var_name, 
                                     mediator = mediator_var_name,
                                     boot = TRUE, 
                                     sims = 1000)
  
  return(med_analysis)
}

af_interpret_mediate_results <- function(med_analysis, outcome_var_name, mediator_var_name, treatment_var_name) {
  # Extract summary of the mediation analysis
  med_summary <- summary(med_analysis)
  
  PropMediated <- med_summary$n0
  PropMediated_CI <- c(med_summary$n0.ci[1], med_summary$n0.ci[2])
  PropMediated_p <- med_summary$n0.p
  
  # Print interpretation of results
  if (PropMediated_p < 0.05) {
    result_line <- 
      paste("The results of the causal mediation analysis shows", round(PropMediated*100, 1), 
        "% of", treatment_var_name,"total effect on", outcome_var_name, "is mediated through", mediator_var_name, ".\n")
  } else {
    result_line <- 
      "The proportion of the total effect mediated through the mediator is not statistically significant.\n"
  }
  
  return(result_line)
}

af_plot_mediation_effects <- function(med_results) {
  # Extract mediation results
  summ <- summary(med_results)
  
  # Create data frame for plotting
  effects_df <- data.frame(
    effect = c("ACME", "ADE", "Total\nEffect"),
    estimate = c(summ$d0, summ$z0, summ$tau.coef),
    ci_lower = c(summ$d0.ci[1], summ$z0.ci[1], summ$tau.ci[1]),
    ci_upper = c(summ$d0.ci[2], summ$z0.ci[2], summ$tau.ci[2])
  )
  
  # Create plot
  ggplot(effects_df, aes(y = effect, x = estimate)) +
    geom_point() +
    geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +
    labs(x = "Effect Size", y = "") +
    theme_minimal()
}

af_plot_mediation_coefficients <- function(med_results) {
  # Extract coefficients
  a <- coef(med_results$model.m)["treatment"]  # treatment -> mediator
  b <- coef(med_results$model.y)["mediator"]   # mediator -> outcome
  c_prime <- coef(med_results$model.y)["treatment"] # direct effect
  
  # Create layout data
  nodes <- data.frame(
    x = c(0, 1, 2),
    y = c(0, 1, 0),
    label = c("Treatment", "Mediator", "Outcome")
  )
  
  # Create edge labels
  edge_labels <- data.frame(
    x = c(0.5, 1.5, 0.8),
    y = c(0.6, 0.6, 0),
    label = c(
      sprintf("a = %.2f", a),
      sprintf("b = %.2f", b),
      sprintf("c' = %.2f", c_prime)
    )
  )
  
  # Create plot
  library(ggplot2)
  ggplot() +
    # Add edges (arrows)
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
                 arrow = arrow(length = unit(0.3, "cm"))) +
    geom_segment(aes(x = 1, y = 1, xend = 2, yend = 0), 
                 arrow = arrow(length = unit(0.3, "cm"))) +
    geom_segment(aes(x = 0, y = 0, xend = 2, yend = 0), 
                 arrow = arrow(length = unit(0.3, "cm"))) +
    # Add nodes
    geom_point(data = nodes, aes(x = x, y = y), size = 20, shape = 21, 
               fill = "white", color = "black") +
    # Add node labels
    geom_text(data = nodes, aes(x = x, y = y, label = label)) +
    # Add edge labels
    geom_text(data = edge_labels, aes(x = x, y = y, label = label), 
              vjust = -0.5) +
    # Customize theme
    theme_void() +
    theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
    coord_cartesian(xlim = c(-0.5, 2.5), ylim = c(-0.5, 1.5))
}

af_create_mediation_table <- function(med_results) {

    summ <- summary(med_results)
  
  data.frame(
    Effect = c("ACME", "ADE", "Total Effect", "Prop. Mediated"),
    Estimate = c(summ$d0, summ$z0, summ$tau.coef, summ$n0),
    CI_Lower = c(summ$d0.ci[1], summ$z0.ci[1], summ$tau.ci[1], summ$n0.ci[1]),
    CI_Upper = c(summ$d0.ci[2], summ$z0.ci[2], summ$tau.ci[2], summ$n0.ci[2]),
    P_Value = c(summ$d0.p, summ$z0.p, summ$tau.p, summ$n0.p)
  ) %>%
    gt() %>%
    fmt_number(columns = c("Estimate", "CI_Lower", "CI_Upper"), decimals = 4) %>%
    fmt_scientific(columns = "P_Value", decimals = 2) %>%
    cols_label(
      CI_Lower = "95% CI Lower",
      CI_Upper = "95% CI Upper",
      P_Value = "p-value"
    ) %>%
    tab_header(
      title = "Causal Mediation Analysis Results"
    ) %>%
    tab_footnote(
      footnote = "ACME = Average Causal Mediation Effect; ADE = Average Direct Effect",
      locations = cells_column_labels(columns = Effect)
    )
}
