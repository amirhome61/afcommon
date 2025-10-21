af_plot_coef <- function(model, covs = NULL, palette = "Color") {
  colr = ifelse(palette == "Color", "Set1", "bw")
  p <- plot_model(
    model,
    type = "std",
    colors = colr,
    vline.color = "grey70",
    title = "",
    show.values = TRUE,
    value.size = 2.5,
    value.offset = 0.4,
    show.p = TRUE,
    show.intercept = FALSE,
    show.legend = FALSE,
    digits = 3,
    sort.est = TRUE
  ) +
    theme(legend.position = "none")

  if (!is.null(covs)) {
    p <- p + scale_x_discrete(labels = names(covs)[order(p$data$estimate)])
  }

  return(p)
}

af_multi_plot_coef <- function(models, covs = NULL, palette = "Pastel1") {
  p <- sjPlot::plot_models(
    models,
    std.est = "std",
    colors = palette,
    vline.color = "grey70",
    title = "",
    show.p = FALSE,
    show.legend = TRUE
  ) +
    theme(legend.position = "bottom")

  if (!is.null(covs)) {
    p <- p + scale_x_discrete(labels = names(covs)[order(p$data$estimate)])
  }

  return(p)
}

af_plot_interaction <- function(
  model,
  dep_label,
  x_label,
  y_label,
  title = ""
) {
  sjPlot::plot_model(
    model,
    type = "int",
    title = title,
    axis.title = c(x_label, y_label),
    legend.title = dep_label
  ) +
    theme(legend.position = "bottom")
}

# The af_build_formula function is a versatile R tool designed to construct complex regression formulas by accepting various parameter lists that specify different components of the model. At its core, it requires a response variable (or multiple response variables) and can optionally include predictor variables, while offering extensive customization through additional parameters. The function supports a wide range of statistical modeling approaches including linear models, generalized linear models, mixed-effects models, and generalized additive models. It can handle multiple response variables, polynomial terms, variable transformations, two-way interactions, nested terms, random effects, and offset variables. Through its flexible parameter structure, users can create formulas for simple linear regression up to complex hierarchical models with crossed random effects and smooth terms. The function performs input validation, builds the formula component by component, and returns a valid R formula object that can be directly used in various modeling functions such as lm(), glm(), lmer(), or gam(). For convenience, it automatically handles special cases such as multiple response variables using cbind(), polynomial specifications using poly(), smooth terms using s(), and random effects specifications using the appropriate syntax for mixed models.

af_build_formula <- function(
  # Core variables
  as_string = FALSE, # Logical: return formula as string instead of formula object
  response, # Character: response variable(s)
  predictors = NULL, # Character vector: main predictor variables

  # Special terms
  random = NULL, # List: random effects specifications
  nested = NULL, # List: nested term specifications
  smooth = NULL, # List: smooth term specifications (for GAMs)
  offset = NULL, # Character: offset variable(s)

  # Variable transformations
  poly = NULL, # List: polynomial specifications
  resp_transform = NULL, # Character: response transformation function
  var_transform = NULL, # List: predictor transformation specifications

  # Interaction specifications
  interactions = NULL, # List: interaction specifications with type
  cross = NULL, # List: full factorial crossing specifications
  interaction_type = ":", # Character: type of interaction operator (":" or "*")

  # Model specifications
  weights = NULL, # Character: weight variable
  intercept = TRUE, # Logical: include intercept?

  # Additional specifications
  grouping = NULL, # Character vector: grouping variables for mixed models
  constraints = NULL # List: constraint specifications
) {
  # Input validation
  if (is.null(response) || length(response) == 0) {
    stop("Response variable(s) must be specified")
  }

  # Initialize formula components
  lhs <- NULL # Left-hand side
  rhs <- NULL # Right-hand side

  # Build left-hand side (response)
  if (length(response) > 1) {
    lhs <- paste("cbind(", paste(response, collapse = ", "), ")")
  } else {
    lhs <- response
  }

  # Apply response transformation if specified
  if (!is.null(resp_transform)) {
    lhs <- paste0(resp_transform, "(", lhs, ")")
  }

  # Build right-hand side components
  terms <- list()

  # 1. Handle intercept
  if (!intercept) {
    terms <- c(terms, "-1")
  }

  # 2. Add main predictors with transformations
  if (!is.null(predictors)) {
    pred_terms <- predictors
    if (!is.null(var_transform)) {
      pred_terms <- mapply(
        function(x) {
          if (x %in% names(var_transform)) {
            paste0(var_transform[[x]], "(", x, ")")
          } else {
            x
          }
        },
        predictors,
        USE.NAMES = FALSE
      )
    }
    terms <- c(terms, pred_terms)
  }

  # 3. Add polynomial terms
  if (!is.null(poly)) {
    poly_terms <- mapply(
      function(var, degree) {
        paste0("poly(", var, ", ", degree, ")")
      },
      names(poly),
      poly,
      USE.NAMES = FALSE
    )
    terms <- c(terms, poly_terms)
  }

  # 4. Add smooth terms (for GAMs)
  if (!is.null(smooth)) {
    smooth_terms <- mapply(
      function(var, spec) {
        if (is.list(spec)) {
          paste0(
            "s(",
            var,
            ", k=",
            spec$k,
            if (!is.null(spec$by)) paste0(", by=", spec$by),
            if (!is.null(spec$bs)) paste0(", bs='", spec$bs, "'"),
            ")"
          )
        } else {
          paste0("s(", var, ")")
        }
      },
      names(smooth),
      smooth,
      USE.NAMES = FALSE
    )
    terms <- c(terms, smooth_terms)
  }

  # 5. Add interactions
  if (!is.null(interactions)) {
    int_terms <- sapply(interactions, function(x) {
      if (is.list(x) && !is.null(x$type)) {
        paste(x$vars, collapse = x$type)
      } else {
        paste(x, collapse = interaction_type)
      }
    })
    terms <- c(terms, int_terms)
  }

  # 6. Add crossed terms (full factorial)
  if (!is.null(cross)) {
    cross_terms <- sapply(cross, function(x) {
      paste0("(", paste(x, collapse = "*"), ")")
    })
    terms <- c(terms, cross_terms)
  }

  # 7. Add nested terms
  if (!is.null(nested)) {
    nested_terms <- sapply(nested, function(x) {
      paste(x, collapse = "/")
    })
    terms <- c(terms, nested_terms)
  }

  # 8. Add random effects
  if (!is.null(random)) {
    random_terms <- sapply(random, function(x) {
      if (is.list(x)) {
        paste0("(", paste(x$terms, collapse = " + "), " | ", x$group, ")")
      } else {
        paste0("(1 | ", x, ")")
      }
    })
    terms <- c(terms, random_terms)
  }

  # 9. Add offset terms
  if (!is.null(offset)) {
    offset_terms <- paste0("offset(", offset, ")")
    terms <- c(terms, offset_terms)
  }

  # 10. Add constraints
  if (!is.null(constraints)) {
    constraint_terms <- sapply(constraints, function(x) {
      paste0("(", x, ")")
    })
    terms <- c(terms, constraint_terms)
  }

  # Combine all terms
  rhs <- paste(unlist(terms), collapse = " + ")

  # Build the complete formula
  formula_str <- paste(lhs, "~", rhs)

  # Return results based on what's specified
  result <- list(
    formula = if (as_string) formula_str else as.formula(formula_str),
    weights = weights
  )

  # If no weights specified and as_string is TRUE, just return the formula string
  if (as_string && is.null(weights)) {
    return(formula_str)
  }

  return(result)
}


af_modelsummary <- function(
  models,
  tag = "reg-out",
  coef_map = NULL,
  coef_exp = FALSE,
  exp_ctrl = NULL,
  output_file = NULL,
  gof_map = c("nobs", "r.squared", "adj.r.squared")
) {
  # Initialize variables
  model_list <- models
  notes_text <- NULL

  # Handle coefficient exponentiation
  if (!is.null(exp_ctrl)) {
    for (i in seq_along(models)) {
      if (exp_ctrl[i]) {
        # Create a copy of the model and exponentially transform coefficients
        model_copy <- models[[i]]
        coef(model_copy) <- exp(coef(models[[i]]))
        model_list[[i]] <- model_copy
      }
    }
    exp_ctrl_idxs <- which(exp_ctrl)
    notes_text <- paste(
      "Estimates of models",
      toString(exp_ctrl_idxs),
      "are exponentiated."
    )
  }

  # Handle full coefficient exponentiation if coef_exp is TRUE
  if (coef_exp) {
    for (i in seq_along(models)) {
      model_copy <- models[[i]]
      coef(model_copy) <- exp(coef(models[[i]]))
      model_list[[i]] <- model_copy
    }
    notes_text <- "All estimates are exponentiated."
  }

  if (!is.null(output_file)) {
    ms_table <- modelsummary(
      model_list,
      stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
      coef_map = coef_map,
      gof_map = gof_map,
      output = output_file,
      title = paste0("Table: ", tag),
      notes = notes_text
    )
  }

  gt_ms_table <- modelsummary(
    model_list,
    stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
    coef_map = coef_map,
    gof_map = gof_map,
    output = "gt",
    title = paste0("Table: ", tag),
    notes = notes_text
  )

  # Return gt table
  return(gt_ms_table)
}

# The create_regression_notes function generates formatted notes for regression tables by combining information about statistical significance levels, reference categories for categorical variables, sample sizes, and model specifications. It accepts a dataframe and one or multiple regression models, along with optional display names for variables. The function automatically detects categorical variables and their reference levels, handles both single and multiple model cases, and can indicate the use of robust standard errors and exponentiated coefficients. It produces a professionally formatted note suitable for academic papers or technical reports, with proper punctuation and grammar that adapts based on the number of models and specified parameters. The function supports customization through parameters such as significance thresholds, display names for variables, and model-specific settings for exponentiated coefficients.

af_create_regression_notes <- function(
  df,
  models,
  display_names = NULL,
  is_robust = FALSE,
  is_bootstrap = FALSE,
  bootstrap_iterations = 1000,
  is_exp = FALSE,
  show_significance = FALSE,
  show_n = FALSE,
  significance_levels = c(0.05, 0.01, 0.001)
) {
  # Initialize note components
  note_parts <- list()

  # Add significance levels with optional Rmarkdown escaping
  if (show_significance) {
    note_parts$significance <- paste0(
      "Note: * p < ",
      format(max(significance_levels), nsmall = 3),
      "; ",
      "** p < ",
      format(median(significance_levels), nsmall = 3),
      "; ",
      "*** p < ",
      format(min(significance_levels), nsmall = 3)
    )
  }

  # Function to get reference levels for categorical variables
  get_reference_levels <- function(model) {
    # Get all variables from the model formula
    all_vars <- all.vars(formula(model))
    # Filter for only factor variables that are in the model
    factor_vars <- all_vars[
      all_vars %in% names(df) & sapply(df[all_vars], is.factor)
    ]

    ref_levels <- character()
    for (var in factor_vars) {
      ref_level <- levels(df[[var]])[1] # assuming first level is reference
      display_var <- ifelse(
        !is.null(display_names[var]),
        display_names[var],
        var
      )
      ref_levels <- c(
        ref_levels,
        sprintf("The reference category for %s is '%s'", display_var, ref_level)
      )
    }
    return(ref_levels)
  }

  # Get reference levels from all models
  if (inherits(models, "list")) {
    all_ref_levels <- unique(unlist(lapply(models, get_reference_levels)))
  } else {
    all_ref_levels <- unique(get_reference_levels(models))
  }

  if (length(all_ref_levels) > 0) {
    note_parts$ref_levels <- paste0(all_ref_levels, collapse = ". ")
    note_parts$ref_levels <- paste0(note_parts$ref_levels, ".")
  }

  # Get sample sizes and handle both single and multiple models
  if (show_n) {
    if (inherits(models, "list")) {
      n_models <- length(models)
      sample_sizes <- sapply(models, function(m) nrow(model.frame(m)))

      if (n_models > 1) {
        if (n_models == 2) {
          note_parts$n <- sprintf(
            "N = %d and %d for models 1–2, respectively",
            sample_sizes[1],
            sample_sizes[2]
          )
        } else {
          note_parts$n <- sprintf(
            "N = %s, and %d for models 1–%d, respectively",
            paste(sample_sizes[-n_models], collapse = ", "),
            sample_sizes[n_models],
            n_models
          )
        }
      }
    } else {
      # Single model case
      sample_sizes <- nrow(model.frame(models))
      note_parts$n <- sprintf("N = %d", sample_sizes)
    }
  }

  # Add robust standard errors note if applicable
  if (any(is_robust)) {
    if (inherits(models, "list")) {
      if (length(is_robust) == 1) {
        is_robust <- rep(is_robust, length(models))
      }
      robust_models <- which(is_robust)
      if (length(robust_models) > 1) {
        model_nums <- af_comma_and_list(robust_models)
        note_parts$robust <- sprintf(
          "Models %s have robust standard errors in parenthesis",
          model_nums
        )
      } else {
        model_nums <- robust_models
        note_parts$robust <- sprintf(
          "Model %s has robust standard errors in parenthesis",
          model_nums
        )
      }
    } else {
      note_parts$robust <- "Robust standard errors in parentheses"
    }
  } else {
    if (!is_bootstrap) note_parts$robust <- "Standard errors in parentheses"
  }

  # Add bootstrapped standard errors note if applicable
  if (any(is_bootstrap)) {
    if (inherits(models, "list")) {
      if (length(is_bootstrap) == 1) {
        is_bootstrap <- rep(is_bootstrap, length(models))
      }
      bootstrap_models <- which(is_bootstrap)
      if (length(bootstrap_models) > 1) {
        model_nums <- af_comma_and_list(bootstrap_models)
        note_parts$bootstrap <- sprintf(
          "Models %s have bootstrapped standard errors in parentheses (%d iterations)",
          model_nums,
          bootstrap_iterations
        )
      } else {
        model_nums <- robust_models
        note_parts$bootstrap <- sprintf(
          "Model %s has bootstrapped standard errors in parenthesis (%d iterations)",
          model_nums,
          bootstrap_iterations
        )
      }
    } else {
      note_parts$bootstrap <- sprintf(
        "Bootstrapped standard errors in parenthesis (%d iterations).",
        bootstrap_iterations
      )
    }
  }

  # Add exponentiated coefficients note if applicable
  if (any(is_exp)) {
    if (inherits(models, "list")) {
      if (length(is_exp) == 1) {
        is_exp <- rep(is_exp, length(models))
      }
      exp_models <- which(is_exp)
      if (length(exp_models) > 1) {
        model_nums <- paste("models", paste(exp_models, collapse = " and "))
        note_parts$exp <- sprintf(
          "The coefficients of %s are exponentiated",
          model_nums
        )
      } else {
        model_nums <- paste("model", exp_models)
        note_parts$exp <- sprintf(
          "The coefficients of %s are exponentiated",
          model_nums
        )
      }
    } else {
      note_parts$exp <- "The coefficients are exponentiated"
    }
  }

  # Combine all parts with proper spacing and punctuation
  note_parts <- lapply(note_parts, function(x) {
    if (!grepl("[.!?]$", x)) {
      paste0(x, ".")
    } else {
      x
    }
  })

  note <- paste(unlist(note_parts), collapse = " ")

  return(note)
}

# The function af_vif calculates Variance Inflation Factors (VIF) for regression models to assess multicollinearity between predictor variables. It returns both a summary text indicating whether multicollinearity is a concern (using a threshold of VIF > 10) and a formatted table showing VIF values for each predictor variable.

af_vif_test <- function(model) {
  # Calculate VIF values
  vif_results <- performance::multicollinearity(model)
  vif_values <- vif_results$VIF

  # Create result message
  if (any(vif_values > 10)) {
    result_line <- "We use VIF to test for multicolinearity. The result indicates VIF values > 10 detected. \n Multicollinearity is high.\n"
  } else {
    result_line <- "We use VIF to test for multicolinearity. The result indicates that all VIF values <= 10. \n Multicollinearity is not a concern.\n"
  }

  # Return both results as a list

  gt_tbl = vif_results %>%
    gt() %>%
    fmt_number(
      columns = where(is.numeric),
      decimals = 1
    ) %>%
    tab_header(
      title = "VIF Table"
    )

  return(list(
    result_line = result_line,
    gt_tbl = gt_tbl
  ))
}


af_hetero_test <- function(model) {
  het_test <- performance::check_heteroscedasticity(model)
  pval <- as.numeric(het_test)

  if (pval < 0.05) {
    result_line <- paste0(
      "We use Breusch-Pagan test for homoscedasticity (equal variance) assumption. The result of P-value ",
      round(pval, 3),
      " < 0.05 indicates a concern for heteroscedasticity.\n"
    )
  } else {
    result_line <- paste0(
      "We use Breusch-Pagan test for homoscedasticity (equal variance) assumption. The result of P-value ",
      round(pval, 3),
      " >= 0.05, indicates homoscedasticity (no concern for heteroscedasticity).\n"
    )
  }

  return(list(
    result_line = result_line,
    pval = pval
  ))
}

af_expand_x_list <- function(df, x_list) {
  # The function expands categorical variables into their individual levels (excluding the reference level)
  # while keeping numeric variables as-is. For categorical variables, it creates separate named entries for
  # each non-reference level, using the original variable name as the key. For numeric variables, it creates
  # a single unnamed entry.
  result <- c()
  for (var in x_list) {
    type <- af_col_type(df, var)
    if (type %in% c("nominal", "ordinal", "factor")) {
      levels <- levels(df[[var]])
      result <- c(result, setNames(levels[-1], rep(var, length(levels) - 1)))
    } else {
      result <- c(result, setNames("", var))
    }
  }
  return(result)
}

af_prepare_regression <- function(df, dependent_var, independent_vars) {
  notes <- character()
  valid_vars <- c()

  # Check if dependent variable exists
  if (!(dependent_var %in% names(df))) {
    notes <- c(
      notes,
      sprintf(
        "Dependent variable '%s' does not exist in the data",
        dependent_var
      )
    )
    dependent_var <- NULL
  }

  for (var in independent_vars) {
    if (!(var %in% names(df))) {
      notes <- c(
        notes,
        sprintf("Independent variable '%s' does not exist in the data", var)
      )
    } else if (is.factor(df[[var]])) {
      unique_values <- unique(df[[var]])
      if (length(unique_values) == 1) {
        notes <- c(
          notes,
          sprintf(
            "Variable '%s' had a single value of '%s'",
            var,
            as.character(unique_values)
          )
        )
      } else {
        valid_vars <- c(valid_vars, var)
      }
    } else {
      valid_vars <- c(valid_vars, var)
    }
  }

  if (is.null(dependent_var) || length(valid_vars) == 0) {
    formula <- NULL
  } else {
    formula <- as.formula(paste(
      dependent_var,
      "~",
      paste(valid_vars, collapse = " + ")
    ))
  }

  # notes <- if (length(notes) > 0) paste(notes, collapse = "\n") else ""

  list(formula = formula, notes = notes)
}

#' Variance Inflation Factor Test with GT Table Output
#'
#' This function performs a VIF test on a regression model and returns both
#' a formatted GT table and a text interpretation of the results.
#'
#' @param model A fitted regression model (lm, glm, etc.)
#' @param interactions Logical. If TRUE, uses type = 'predictor' for models with interactions.
#'                    If FALSE, uses standard VIF calculation. Default is TRUE.
#' @param threshold Numeric. The threshold value for determining problematic multicollinearity.
#'                 Default is 2.0 for adjusted GVIF when interactions = TRUE, 5.0 otherwise.
#'
#' @return A list containing:
#'   \item{table}{A gt table with VIF results}
#'   \item{interpretation}{A character string with interpretation of results}
#'   \item{max_vif}{The maximum VIF value found}
#'   \item{problematic_predictors}{Vector of predictor names exceeding threshold}
#'
#' @examples
#' # Basic usage
#' model <- lm(y ~ x1 + x2 + x3, data = mydata)
#' result <- af_vif_test(model, interactions = FALSE)
#' print(result$interpretation)
#' result$table
#'
#' # With interactions
#' model_int <- lm(y ~ x1 * x2 + x3, data = mydata)
#' result <- af_vif_test(model_int, interactions = TRUE)
#'
#' @import car
#' @import gt
#' @import dplyr
#' @export
af_vif_test <- function(model, interactions = TRUE, threshold = NULL) {
  # Input validation
  if (!inherits(model, c("lm", "glm", "mlm"))) {
    stop("Model must be a fitted regression model (lm, glm, etc.)")
  }

  if (!is.logical(interactions)) {
    stop("interactions parameter must be TRUE or FALSE")
  }

  # Set default threshold based on interaction setting
  if (is.null(threshold)) {
    threshold <- ifelse(interactions, 2.0, 5.0)
  }

  if (!is.numeric(threshold) || threshold <= 0) {
    stop("threshold must be a positive numeric value")
  }

  # Load required libraries
  if (!requireNamespace("car", quietly = TRUE)) {
    stop("Package 'car' is required but not installed")
  }
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("Package 'gt' is required but not installed")
  }

  # Calculate VIF
  tryCatch(
    {
      if (interactions) {
        vif_results <- car::vif(model, type = 'predictor')
        vif_column <- "GVIF^(1/(2*Df))"
        vif_label <- "Adjusted GVIF"
        table_title <- "Multicollinearity Assessment (with Interactions)"
      } else {
        vif_results <- car::vif(model)
        # Handle case where vif returns a vector vs matrix
        if (is.vector(vif_results)) {
          vif_results <- data.frame(
            VIF = vif_results,
            row.names = names(vif_results)
          )
          vif_column <- "VIF"
        } else {
          vif_column <- "GVIF^(1/(2*Df))"
        }
        vif_label <- "VIF"
        table_title <- "Multicollinearity Assessment"
      }

      # Convert to data frame
      vif_df <- data.frame(
        Predictor = rownames(vif_results),
        VIF_Value = vif_results[, vif_column],
        stringsAsFactors = FALSE
      )
    },
    error = function(e) {
      stop(paste("Error calculating VIF:", e$message))
    }
  )

  # Identify problematic predictors
  problematic_predictors <- vif_df$Predictor[vif_df$VIF_Value > threshold]
  max_vif <- max(vif_df$VIF_Value)

  # Create GT table
  vif_table <- vif_df %>%
    gt::gt() %>%
    gt::tab_header(title = table_title) %>%
    gt::fmt_number(columns = VIF_Value, decimals = 3) %>%
    gt::cols_label(VIF_Value = vif_label) %>%
    gt::cols_align(align = "center", columns = "VIF_Value") %>%
    gt::tab_footnote(
      footnote = paste(
        "Values >",
        threshold,
        "indicate problematic multicollinearity"
      ),
      locations = gt::cells_column_labels(columns = "VIF_Value")
    )

  # Add conditional formatting for problematic values
  if (length(problematic_predictors) > 0) {
    vif_table <- vif_table %>%
      gt::tab_style(
        style = gt::cell_fill(color = "#ffcccc"),
        locations = gt::cells_body(
          columns = VIF_Value,
          rows = VIF_Value > threshold
        )
      )
  }

  # Generate interpretation text
  if (length(problematic_predictors) == 0) {
    interpretation <- paste0(
      "We use VIF to test for multicollinearity. The results indicate that all ",
      ifelse(interactions, "adjusted GVIF", "VIF"),
      " values are ≤ ",
      threshold,
      ".\n",
      "Multicollinearity is not a concern.\n",
      "Maximum ",
      ifelse(interactions, "adjusted GVIF", "VIF"),
      " value: ",
      round(max_vif, 3),
      "."
    )
  } else {
    interpretation <- paste0(
      "We use VIF to test for multicollinearity. The results indicate ",
      ifelse(interactions, "adjusted GVIF", "VIF"),
      " values > ",
      threshold,
      " detected.\n",
      "Multicollinearity is a concern for: ",
      paste(problematic_predictors, collapse = ", "),
      ".\n",
      "Maximum ",
      ifelse(interactions, "adjusted GVIF", "VIF"),
      " value: ",
      round(max_vif, 3),
      "."
    )
  }

  # Return results
  return(list(
    table = vif_table,
    interpretation = interpretation,
    max_vif = max_vif,
    problematic_predictors = problematic_predictors,
    threshold = threshold
  ))
}
