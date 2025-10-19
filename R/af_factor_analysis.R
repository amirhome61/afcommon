af_interpret_kmo <- function(msa) {
  
  # Kaiser-Meyer-Olkin (KMO) factor adequacy
  #
  # 0.00 to 0.49 unacceptable
  # 0.50 to 0.59 miserable
  # 0.60 to 0.69 mediocre
  # 0.70 to 0.79 middling
  # 0.80 to 0.89 meritorious
  # 0.90 to 1.00 marvelous
  
  # Define the categories and labels
  breaks <- c(-Inf, 0.50, 0.60, 0.70, 0.80, 0.90, Inf)
  labels <- c("Unacceptable", "Miserable", "Mediocre", "Middling", "Meritorious", "Marvelous")
  
  # Find the label corresponding to the msa value
  interpretation <- sprintf("%s (%.2f)", labels[findInterval(msa, breaks)], msa)
  
  return(interpretation)
}

af_interpret_bartlett <- function(bartlett_result) {
  if (bartlett_result$p.value < 0.05) {
    interpretation <- "The test is significant, indicating that the variables are sufficiently correlated for factor analysis."
  } else {
    interpretation <- "The test is not significant, indicating that the variables are not sufficiently correlated for factor analysis."
  }
  return(interpretation)
}

af_interpret_cronbach <- function(alpha_result) {
  
  raw_alpha <- alpha_result$total$raw_alpha
  
  # Define the breaks and labels
  breaks <- c(-Inf, 0.5, 0.6, 0.7, 0.8, 0.9, Inf)
  labels <- c("Unacceptable reliability", "Poor reliability", "Questionable reliability", 
              "Acceptable reliability", "Good reliability", "Excellent reliability")
  
  # Find the appropriate label
  interpretation <- sprintf("%s (%.2f)", labels[findInterval(raw_alpha, breaks)], raw_alpha)
  
  return(interpretation)
}

af_cfa <- function(df, var_list, latent_var_name, 
                   group = NULL, model = NULL, cfa_type = NULL,
                   group_id = "all", factors_list = NULL, clean_model = TRUE) {
  
  # model enables to define a multi-line SEM model rather than create a simple one 
  # based on the var list and the latent var name 
  
  # cfa_type is either: configural / metric / scalar / strict 
  
  # Factor list identify which variables (manifests) should be treated as ordinal. 
  # This enables the flexibility to relate to survey questions as either numeric or ordinal 
  # without changing the data. for example: a scale of 1..7 can be either numeric or ordinal.
  
  var_list <- af_remove_empty_vars(df, var_list) # make sure all variables have data
  
  # If no custom model provided, create standard model
  if (is.null(model)) {
    model <- paste(latent_var_name, "=~", paste(var_list, collapse = "+"))
  } else {
    if (clean_model) {
      result <- af_clean_cfa_model(df, model)
      model <- result$model
    }
    
  }
  
  # Set up group.equal parameter based on cfa_type
  group.equal <- NULL
  if (!is.null(cfa_type)) {
    if (cfa_type == "configural") {
      group.equal <- NULL  # No equality constraints
    } else if (cfa_type == "metric") {
      group.equal <- "loadings"  # Equal factor loadings
    } else if (cfa_type == "scalar") {
      group.equal <- c("loadings", "intercepts")  # Equal loadings and intercepts
    } else if (cfa_type == "strict") {
      group.equal <- c("loadings", "intercepts", "residuals")  # Equal loadings, intercepts, and residuals
    }
  }
  
  # Fit the CFA model
  fit <- lavaan::cfa(model, 
                     data = df,
                     ordered = factors_list,
                     group = group,
                     group.equal = group.equal)
  
  # Get fit measures
  fit_measures <- fitMeasures(fit)
  fit_predict <- lavaan::lavPredict(fit, 
                                    newdata = df,          # data that was use to fit the model
                                    method = "regression",
                                    type = "lv",            # requests latent variable scores
                                    append.data = TRUE,
                                    transform = TRUE)       # predictions incorporate the model-implied mean structure
  
  # Reliability analysis (Cronbach's alpha)
  alpha_result <- psych::alpha(df[, var_list])
  alpha_interpretation <- af_interpret_cronbach(alpha_result)
  
  cfa_tbl <- data.frame("Latent var" = latent_var_name,
                        "Group" = group_id,
                        "CFI" = fit_measures["cfi"],
                        "TLI" = fit_measures["tli"],
                        "RMSEA" = fit_measures["rmsea"],
                        "SRMR"= fit_measures["srmr"],
                        "Cronbach" = alpha_interpretation,
                        "Alpha" = alpha_result$total$raw_alpha
  )
  
  if (is.null(group)) {
    latent_scores <- fit_predict[, latent_var_name]
  } else {
    latent_scores <- numeric(nrow(df))
    
    # Get unique group levels from df
    group_levels <- unique(df[[group]])
    
    # Fill predictions in correct order
    for(g in group_levels) {
      group_indices <- which(df[[group]] == g)
      scores <- as.data.frame(fit_predict[[g]])[[latent_var_name]]
      latent_scores[group_indices] <- scores
    }
    
    check_result <- af_check_order(df, fit_predict, group, 
                                   latent_var_name, var_list, latent_scores)
    if (!check_result$correct) stop(check_result$errors)
  }
  
  # Calculate the predicted latent variable using min-max approach
  # 1. The lowest latent score maps to 1
  # 2. The highest latent score maps to 7
  # 3. All other scores are linearly distributed in between
  
  latent_min <- min(latent_scores)
  latent_max <- max(latent_scores)
  target_min <- 1  # Minimum value on your desired scale
  target_max <- 7  # Maximum value on your desired scale
  
  # Min-max scaling to transform to 1-7 range
  latent_predict <- target_min + (latent_scores - latent_min) * 
    (target_max - target_min) / (latent_max - latent_min)
  
  # Create a new dataframe with the latent variable added
  new_df <- df
  new_df[[latent_var_name]] <- latent_predict
  
  result <- list(fit = fit, cfa_tbl = cfa_tbl, 
                 latent_predict = latent_predict, new_df = new_df)
  
  return(result)
}


# Sanity check function
af_check_order <- function(df, fit_predict, group, 
                           latent_var_name, var_list, latent_scores) {
  all_correct <- TRUE
  all_errors = NULL
  
  for(g in names(fit_predict)) {
    # Get indices for this group
    group_indices <- which(df[[group]] == g)
    
    # Compare manifest variables
    for(var in var_list) {
      if(!all(df[group_indices, var] == fit_predict[[g]][, var])) {
        all_errors <- c(all_errors, paste("Mismatch in", var, "for group", g, "\n"))
        all_correct <- FALSE
      }
    }
    
    # Compare latent scores
    if(!all(latent_scores[group_indices] == fit_predict[[g]][, latent_var_name])) {
      all_errors <- c(all_errors, paste("Mismatch in latent scores for group", g, "\n"))
      all_correct <- FALSE
    }
  }
  
  if(all_correct) {
    all_errors <- c(all_errors, paste("All checks passed - ordering is correct!\n"))
  }
  
  return(list (correct = all_correct, errors = all_errors))
}

af_cfa_between_group <- function(df, group_var, latent_var, cfa_vars, model = NULL) {
  # Convert group_var from string to symbol for dplyr operations
  group_var_sym <- rlang::sym(group_var)
  
  # Split dataframe into list of groups
  grouped_df <- split(df, df[[group_var]])
  
  # Initialize result dataframe and lists for storing various results
  result_df <- df
  fit_results <- list()
  cfa_table <- NULL
  dist_plots <- list()
  corr_plots <- list()
  
  # Process each group
  for (group_id in names(grouped_df)) {
    # Get current group's data
    dfg <- grouped_df[[group_id]]
    
    # Run af_cfa for current group
    result <- af_cfa(
      dfg,
      var_list = cfa_vars,
      latent_var_name = latent_var,
      group_id = group_id, 
      model = model
    )
    
    # Update the result dataframe with predicted values for current group
    group_indices <- which(df[[group_var]] == group_id)
    result_df[group_indices, latent_var] <- result$latent_predict
    
    # Store fit and cfa_table results for this group
    fit_results[[group_id]] <- result$fit
    
    # Add group identifier to cfa_table
    cfa_table <- rbind(cfa_table, result$cfa_tbl)
    
    # Create a dataframe for the distribution plot
    dist_df <- data.frame(temp = result$latent_predict)
    names(dist_df) <- latent_var
    
    # Create and store distribution plot
    dist_plots[[group_id]] <- 
      af_plot_distribution(dist_df, latent_var, 
                           title = paste(group_var, group_id))
    
    corr_plots[[group_id]] <- 
      af_plot_correlation(dfg, cfa_vars, title = paste(group_var, group_id))
  }
  
  # Return all results as a list
  return(list(
    updated_df = result_df,
    fit_by_group = fit_results,
    cfa_table = cfa_table,
    dist_plots = dist_plots, 
    corr_plots = corr_plots
  ))
}

af_create_sem_plot <- function(fit, exclude_nodes = NULL, layout = "tree", nrotation = 1, edge_labels = TRUE) {
  
  if (!is.null(exclude_nodes))  
    fit <- semptools::drop_nodes(object = semPlot::semPlotModel(fit), nodes = exclude_nodes)
  
  if (edge_labels) {  # Do not inclue the edgeLabels parameter
    p <- semPlot::semPaths(
      fit,                     # model to plot 
      exoVar = FALSE, 
      exoCov = FALSE,
      what = "paths",          # "paths", "std", "est"
      layout = layout,         # "tree","circle","spring","tree2","circle2"
      rotation = nrotation,    # 1=top, 2=left side, 3=bottom 4=right side
      style = "OpenMx",        # "OpenMx", "ram"
      whatLabels = "est",      # "est", "std"
      thresholds = FALSE,      # Do not include thresholds (inside manifest rectangles)
      intercepts = FALSE,       # False to prevent intercept triangles
      residuals = FALSE,       # Do not include residuals and variances in plot
      nCharNodes = 0,          # Display full variable names
      edge.label.cex = 0.5,    # Font size of edge labels
      borders = TRUE,          # Ensure borders are shown
      sizeMan = 10,            # Width of manifest rectangles
      sizeMan2 = 8,            # Height of manifest rectangles
      sizeLat = 10,            # Width of latent variables circles
      curvePivot = TRUE,       # Straight+"kincks" edges instead of round
      combineGroups = TRUE,    # If TRUE all groups are combined in the same path diagram
      panelGroups = TRUE,       # Create panel of group plots
      ask = FALSE              # Do not ask for next plot in case of multiple groups
    )
  } else {
    p <- semPlot::semPaths(
      fit,                     # model to plot 
      exoVar = FALSE, 
      exoCov = FALSE,
      what = "paths",          # "paths", "std", "est"
      layout = layout,         # "tree","circle","spring","tree2","circle2"
      rotation = nrotation,    # 1=top, 2=left side, 3=bottom 4=right side
      style = "OpenMx",        # "OpenMx", "ram"
      whatLabels = "est",      # "est", "std"
      edgeLabels = NULL,         # Use "" to not display the loading values
      thresholds = FALSE,      # Do not include thresholds (inside manifest rectangles)
      intercepts = FALSE,       # False to prevent intercept triangles
      residuals = FALSE,       # Do not include residuals and variances in plot
      nCharNodes = 0,          # Display full variable names
      edge.label.cex = 0.5,    # Font size of edge labels
      borders = TRUE,          # Ensure borders are shown
      sizeMan = 10,            # Width of manifest rectangles
      sizeMan2 = 8,            # Height of manifest rectangles
      sizeLat = 10,            # Width of latent variables circles
      curvePivot = TRUE,       # Straight+"kincks" edges instead of round
      combineGroups = TRUE,    # If TRUE all groups are combined in the same path diagram
      panelGroups = TRUE,       # Create panel of group plots
      ask = FALSE              # Do not ask for next plot in case of multiple groups
    )
  }
  
  
  
  
  invisible(p)
}

af_gt_cfa_results_tbl <- function(cfa_tbl) {
  gt_table <- gt(cfa_tbl) %>% 
    gt::tab_source_note(source_note = "CFI (Comparative Fit Index) values above 0.90 indicate good fit.") %>%  
    gt::tab_source_note(source_note = "TLI (Tucker-Lewis Index) values above 0.90 indicate good fit.") %>%  
    gt::tab_source_note(source_note = "RMSEA (Root Mean Square Error of Approximation) values below 0.08 indicate reasonable fit, below 0.05 indicate good fit.") %>% 
    gt::tab_source_note(source_note = "SRMR (Standardized Root Mean Residual) values below 0.08 indicate good fit.") 
  
  return(gt_table)
}

af_clean_cfa_model <- function(df, model) {
  
  # Example usage:
  # result <- clean_cfa_model(df, model)
  # print(result$model)  # Print the cleaned model
  # print(result$removed_indicators)  # See which indicators were removed and why
  
  # Split model into lines
  model_lines <- strsplit(model, "\n")[[1]]
  
  # First, collect all latent variables (they should never be removed)
  latent_vars <- sapply(model_lines, function(line) {
    parts <- strsplit(line, "=~")[[1]]
    if (length(parts) != 2) return(NULL)
    trimws(parts[1])
  })
  latent_vars <- unique(latent_vars[!sapply(latent_vars, is.null)])
  
  # Function to check if a variable is usable (exists and has non-NA/non-NULL values)
  is_usable_variable <- function(var_name) {
    if (!var_name %in% names(df)) return(FALSE)
    var_data <- df[[var_name]]
    # Check if the variable has any non-NA and non-NULL values
    !all(is.na(var_data)) && !all(is.null(var_data))
  }
  
  # Function to clean a single model line
  clean_line <- function(line) {
    # Skip empty lines or comments
    if (grepl("^\\s*$", line) || grepl("^\\s*#", line)) return(line)
    
    # Split the line into latent variable and indicators
    parts <- strsplit(line, "=~")[[1]]
    if (length(parts) != 2) return(line)  # Return unchanged if not a measurement model line
    
    # Keep latent variable part unchanged
    latent_var <- parts[1]
    
    # Process indicators
    indicators <- strsplit(parts[2], "\\+")[[1]]
    indicators <- trimws(indicators)
    
    # An indicator should be kept if it's either:
    # 1. A latent variable from another part of the model, OR
    # 2. Exists in the dataframe AND has some non-NA/non-NULL values
    valid_indicators <- indicators[sapply(indicators, function(ind) {
      ind %in% latent_vars || is_usable_variable(ind)
    })]
    
    # If no valid indicators remain, return NULL
    if (length(valid_indicators) == 0) return(NULL)
    
    # Reconstruct the line, keeping the original latent variable part exactly as is
    paste0(latent_var, "=~ ", paste(valid_indicators, collapse = " + "))
  }
  
  # Clean each line and remove NULL results
  cleaned_lines <- lapply(model_lines, clean_line)
  cleaned_lines <- cleaned_lines[!sapply(cleaned_lines, is.null)]
  
  # Combine lines back into a model string
  cleaned_model <- paste(unlist(cleaned_lines), collapse = "\n")
  
  # Get list of all indicators (excluding latent variables)
  all_indicators <- unique(unlist(lapply(model_lines, function(line) {
    parts <- strsplit(line, "=~")[[1]]
    if (length(parts) != 2) return(character(0))
    indicators <- strsplit(parts[2], "\\+")[[1]]
    indicators <- trimws(indicators)
    # Only return indicators that are not latent variables
    indicators[!indicators %in% latent_vars]
  })))
  
  # Find which indicators were removed and why
  removed_indicators <- data.frame(
    indicator = all_indicators,
    reason = sapply(all_indicators, function(ind) {
      if (!ind %in% names(df)) {
        "not in dataframe"
      } else if (all(is.na(df[[ind]]))) {
        "all NA"
      } else if (all(is.null(df[[ind]]))) {
        "all NULL"
      } else {
        NA_character_
      }
    })
  )
  removed_indicators <- removed_indicators[!is.na(removed_indicators$reason), ]
  
  list(
    model = cleaned_model,
    removed_indicators = removed_indicators
  )
}

af_measurement_invariance <- function(model, df, group, var_list, latent_var_name) {
  # Run different types of CFA
  configural_fit <- af_cfa(df = df, var_list = var_list, latent_var_name = latent_var_name, 
                           group = group, model = model, cfa_type = "configural")
  
  metric_fit <- af_cfa(df = df, var_list = var_list, latent_var_name = latent_var_name, 
                       group = group, model = model, cfa_type = "metric")
  
  scalar_fit <- af_cfa(df = df, var_list = var_list, latent_var_name = latent_var_name, 
                       group = group, model = model, cfa_type = "scalar")
  
  strict_fit <- af_cfa(df = df, var_list = var_list, latent_var_name = latent_var_name,
                       group = group, model = model, cfa_type = "strict")
  
  # Compare model fits
  measEqOut <- semTools::compareFit(configural_fit$fit, metric_fit$fit, 
                                    scalar_fit$fit, strict_fit$fit)
  
  # Generate interpretation text
  result_text <- af_interpret_measurement_invariance(measEqOut)
  
  # Create comparison plot of latent predictions
  latent_comparison_plot <- ggplot() +
    geom_smooth(aes(x = 1:length(configural_fit$latent_predict), 
                    y = configural_fit$latent_predict, 
                    color = "Configural"), se = FALSE) +
    geom_smooth(aes(x = 1:length(metric_fit$latent_predict), 
                    y = metric_fit$latent_predict, 
                    color = "Metric"), se = FALSE) +
    geom_smooth(aes(x = 1:length(scalar_fit$latent_predict), 
                    y = scalar_fit$latent_predict, 
                    color = "Scalar"), se = FALSE) +
    labs(title = "Comparison of Latent Predictions",
         x = "Index",
         y = "Latent Score",
         color = "Model Type") +
    theme_minimal()
  
  # Calculate differences and correlations
  metric_config_diff <- metric_fit$latent_predict - configural_fit$latent_predict
  scalar_metric_diff <- scalar_fit$latent_predict - metric_fit$latent_predict
  
  metric_config_cor <- cor(metric_fit$latent_predict, configural_fit$latent_predict)
  scalar_metric_cor <- cor(scalar_fit$latent_predict, metric_fit$latent_predict)
  
  # Create difference plots
  metric_config_plot <- ggplot() +
    geom_line(aes(x = 1:length(metric_config_diff), y = metric_config_diff)) +
    geom_hline(yintercept = mean(metric_config_diff), linetype = "dashed", color = "red") +
    geom_hline(yintercept = min(metric_config_diff), linetype = "dotted", color = "blue") +
    geom_hline(yintercept = max(metric_config_diff), linetype = "dotted", color = "blue") +
    labs(title = "Metric - Configural Differences",
         subtitle = sprintf("Correlation: %.3f", metric_config_cor),
         x = "Index",
         y = "Difference") +
    annotate("text", x = 10, y = mean(metric_config_diff), 
             label = sprintf("Mean: %.3f", mean(metric_config_diff))) +
    annotate("text", x = 10, y = min(metric_config_diff), 
             label = sprintf("Min: %.3f", min(metric_config_diff))) +
    annotate("text", x = 10, y = max(metric_config_diff), 
             label = sprintf("Max: %.3f", max(metric_config_diff))) +
    theme_minimal()
  
  scalar_metric_plot <- ggplot() +
    geom_line(aes(x = 1:length(scalar_metric_diff), y = scalar_metric_diff)) +
    geom_hline(yintercept = mean(scalar_metric_diff), linetype = "dashed", color = "red") +
    geom_hline(yintercept = min(scalar_metric_diff), linetype = "dotted", color = "blue") +
    geom_hline(yintercept = max(scalar_metric_diff), linetype = "dotted", color = "blue") +
    labs(title = "Scalar - Metric Differences",
         subtitle = sprintf("Correlation: %.3f", scalar_metric_cor),
         x = "Index",
         y = "Difference") +
    annotate("text", x = 10, y = mean(scalar_metric_diff), 
             label = sprintf("Mean: %.3f", mean(scalar_metric_diff))) +
    annotate("text", x = 10, y = min(scalar_metric_diff), 
             label = sprintf("Min: %.3f", min(scalar_metric_diff))) +
    annotate("text", x = 10, y = max(scalar_metric_diff), 
             label = sprintf("Max: %.3f", max(scalar_metric_diff))) +
    theme_minimal()
  
  # Create and return results list
  results <- list(
    fits = list(
      configural = configural_fit,
      metric = metric_fit,
      scalar = scalar_fit,
      strict = strict_fit
    ),
    measEqOut = measEqOut,
    result_text = result_text,
    plots = list(
      latent_comparison = latent_comparison_plot,
      metric_configural_diff = metric_config_plot,
      scalar_metric_diff = scalar_metric_plot
    ),
    differences = list(
      metric_configural = list(
        diff = metric_config_diff,
        cor = metric_config_cor,
        mean = mean(metric_config_diff),
        min = min(metric_config_diff),
        max = max(metric_config_diff)
      ),
      scalar_metric = list(
        diff = scalar_metric_diff,
        cor = scalar_metric_cor,
        mean = mean(scalar_metric_diff),
        min = min(scalar_metric_diff),
        max = max(scalar_metric_diff)
      )
    )
  )
  
  return(results)
}

# ------------------------------------------------------------------------------

af_interpret_measurement_invariance <- function(measEqOut, 
                                                cutoff_cfi = 0.01, 
                                                cutoff_rmsea = 0.015, 
                                                cutoff_p = 0.05,
                                                borderline_cfi = 0.015,  
                                                borderline_rmsea = 0.06) {
  
  # Extract nested model comparisons and fit indices
  chi_tests <- measEqOut@nested
  fit_indices <- measEqOut@fit
  
  rownames(chi_tests) <- c("fit.config", "fit.metric", "fit.scalar", "fit.strict")
  rownames(fit_indices) <- c("fit.config", "fit.metric", "fit.scalar", "fit.strict")
  
  # Evaluate each level of invariance with both strict and borderline criteria
  metric_evaluation <- list(
    chi_p = chi_tests["fit.metric", "Pr(>Chisq)"] > cutoff_p,
    cfi = abs(fit_indices["fit.metric", "cfi"] - fit_indices["fit.config", "cfi"]) < cutoff_cfi,
    rmsea = abs(fit_indices["fit.metric", "rmsea"] - fit_indices["fit.config", "rmsea"]) < cutoff_rmsea,
    absolute_fit = fit_indices["fit.metric", "cfi"] > 0.95
  )
  
  scalar_evaluation <- list(
    chi_p = chi_tests["fit.scalar", "Pr(>Chisq)"] > cutoff_p,
    cfi = abs(fit_indices["fit.scalar", "cfi"] - fit_indices["fit.metric", "cfi"]) < cutoff_cfi,
    rmsea = abs(fit_indices["fit.scalar", "rmsea"] - fit_indices["fit.metric", "rmsea"]) < cutoff_rmsea,
    absolute_fit = fit_indices["fit.scalar", "cfi"] > 0.95,
    borderline_cfi = abs(fit_indices["fit.scalar", "cfi"] - fit_indices["fit.metric", "cfi"]) < borderline_cfi,
    acceptable_fit = fit_indices["fit.scalar", "cfi"] > 0.95 && fit_indices["fit.scalar", "rmsea"] < borderline_rmsea
  )
  
  strict_evaluation <- list(
    chi_p = chi_tests["fit.strict", "Pr(>Chisq)"] > cutoff_p,
    cfi = abs(fit_indices["fit.strict", "cfi"] - fit_indices["fit.scalar", "cfi"]) < cutoff_cfi,
    rmsea = abs(fit_indices["fit.strict", "rmsea"] - fit_indices["fit.scalar", "rmsea"]) < cutoff_rmsea,
    absolute_fit = fit_indices["fit.strict", "cfi"] > 0.95
  )
  
  # Generate interpretation text
  interpretation <- paste0(
    "Assessment of Measurment Invariance\n",
    "\nConfigural Model Fit:\n\n",
    sprintf("+ CFI = %.3f, RMSEA = %.3f\n", 
            fit_indices["fit.config", "cfi"],
            fit_indices["fit.config", "rmsea"]),
    if(fit_indices["fit.config", "cfi"] > 0.95) 
      "+ Good configural fit established\n" 
    else 
      "+ Potential issues with configural fit\n",
    
    "\nMetric (Loading) Invariance:\n\n",
    sprintf("+ Chi-square difference p = %.3f\n", chi_tests["fit.metric", "Pr(>Chisq)"]),
    sprintf("+ ΔCFI = %.3f, ΔRMSEA = %.3f\n",
            fit_indices["fit.metric", "cfi"] - fit_indices["fit.config", "cfi"],
            fit_indices["fit.metric", "rmsea"] - fit_indices["fit.config", "rmsea"]),
    if(all(unlist(metric_evaluation))) 
      "+ ✓ Metric invariance supported\n" 
    else 
      "+ ⚠ Metric invariance not fully supported\n",
    
    "\nScalar (Intercept) Invariance:\n\n",
    sprintf("+ Chi-square difference p = %.3f\n", chi_tests["fit.scalar", "Pr(>Chisq)"]),
    sprintf("+ ΔCFI = %.3f, ΔRMSEA = %.3f\n",
            fit_indices["fit.scalar", "cfi"] - fit_indices["fit.metric", "cfi"],
            fit_indices["fit.scalar", "rmsea"] - fit_indices["fit.metric", "rmsea"]),
    if(all(unlist(scalar_evaluation[c("chi_p", "cfi", "rmsea")]))) 
      "+ ✓ Scalar invariance supported\n"
    else if(scalar_evaluation$chi_p && (scalar_evaluation$borderline_cfi || scalar_evaluation$acceptable_fit))
      "+ ○ Scalar invariance borderline but acceptable\n"
    else 
      "+ ⚠ Scalar invariance not supported\n",
    
    "\nStrict Invariance:\n\n",
    sprintf("+ Chi-square difference p = %.3f\n", chi_tests["fit.strict", "Pr(>Chisq)"]),
    sprintf("+ ΔCFI = %.3f, ΔRMSEA = %.3f\n",
            fit_indices["fit.strict", "cfi"] - fit_indices["fit.scalar", "cfi"],
            fit_indices["fit.strict", "rmsea"] - fit_indices["fit.scalar", "rmsea"]),
    if(all(unlist(strict_evaluation))) 
      "+ ✓ Strict invariance supported\n"
    else if(strict_evaluation$chi_p && strict_evaluation$absolute_fit)
      "+ ○ Strict invariance acceptable given good absolute fit\n"
    else 
      "+ ⚠ Strict invariance not supported\n",
    
    "\nOVERALL CONCLUSION:\n",
    if(all(unlist(metric_evaluation))) {
      if(all(unlist(scalar_evaluation[c("chi_p", "absolute_fit")])) && 
         (scalar_evaluation$borderline_cfi || scalar_evaluation$acceptable_fit)) {
        if(strict_evaluation$chi_p && strict_evaluation$absolute_fit) {
          paste0(
            "Full measurement invariance supported:\n\n",
            "+ Metric invariance achieved confidently\n",
            "+ Scalar invariance acceptable (borderline but supported by chi-square and absolute fit)\n",
            "+ Strict invariance supported\n\n",
            "You can meaningfully compare means, relationships, and variances across groups, ",
            "though some caution is warranted for mean comparisons."
          )
        } else {
          paste0(
            "Partial measurement invariance supported:\n\n",
            "+ Metric invariance achieved confidently\n",
            "+ Scalar invariance acceptable (borderline but supported by chi-square and absolute fit)\n",
            "+ Strict invariance not fully supported\n\n",
            "You can meaningfully compare means and relationships across groups, ",
            "though some caution is warranted for mean comparisons."
          )
        }
      } else {
        paste0(
          "Partial measurement invariance supported:\n\n",
          "+ Metric invariance achieved confidently\n",
          "+ Other levels not fully supported\n\n",
          "You can compare relationships between variables across groups, ",
          "but should be cautious about mean comparisons."
        )
      }
    } else {
      "Caution: Even metric invariance is not fully supported. Group comparisons may not be meaningful."
    }
  )
  
  return(interpretation)
}