#' Generate Stargazer Regression Tables with Optional Coefficient Exponentiation
#'
#' This function creates formatted regression tables using the stargazer package
#' with enhanced options for coefficient exponentiation and customizable output.
#' It supports exponentiation of all coefficients or selective exponentiation
#' of specific models.
#'
#' @param models A list of regression model objects (e.g., lm, glm objects)
#' @param tag Character string used as the table label and output filename prefix.
#'   Default is "reg-out"
#' @param cov_labels Character vector of custom covariate labels. If NULL,
#'   original variable names are used
#' @param coef_exp Logical. If TRUE, exponentiates all model coefficients.
#'   Default is FALSE
#' @param exp_ctrl Logical vector indicating which models should have
#'   exponentiated coefficients. Must be same length as models list.
#'   Only used when coef_exp is FALSE
#' @param title Character string for the table title. Default is empty string
#' @param notes Character string or vector for table notes. If NULL,
#'   automatic notes about exponentiation are generated
#' @param table_type Character string specifying stargazer output type
#'   (e.g., "html", "latex", "text"). Default is "html"
#' @param out_type Character string specifying output file extension.
#'   Default is "html"
#'
#' @return Invisibly returns the stargazer output. Primary effect is generating
#'   a formatted table file with the specified tag and out_type extension
#'
#' @details
#' The function provides two modes for coefficient exponentiation:
#' \itemize{
#'   \item When coef_exp = TRUE: All coefficients across all models are exponentiated
#'   \item When exp_ctrl is specified: Only coefficients from selected models are exponentiated
#' }
#'
#' When notes is NULL, the function automatically generates appropriate notes
#' about which coefficients have been exponentiated.
#'
#' @examples
#' \dontrun{
#' # Basic usage with two models
#' model1 <- lm(mpg ~ wt + hp, data = mtcars)
#' model2 <- lm(mpg ~ wt + hp + cyl, data = mtcars)
#' af_stargazer(list(model1, model2), tag = "my-results")
#'
#' # With coefficient exponentiation for all models
#' af_stargazer(list(model1, model2), coef_exp = TRUE, tag = "exp-results")
#'
#' # With selective exponentiation
#' af_stargazer(list(model1, model2), exp_ctrl = c(FALSE, TRUE), tag = "partial-exp")
#' }
#'
#' @export
af_stargazer <- function(
  models,
  tag = "reg-out",
  cov_labels = NULL,
  coef_exp = FALSE,
  exp_ctrl = NULL,
  title = "",
  notes = NULL,
  table_type = NULL,
  out_type = 'html'
) {
  # Parameter exp_ctrl enable to exponent only selected models' coefficients
  # Set coef_exp to FALSE
  # Set exp_ctrl to a Boolean vector with TRUE for each model to exponent.

  new_coef <- NULL

  cnotes <- notes # The default case is the notes parameter has text
  notes_align = 'l'
  append_notes <- FALSE
  notes_label <- ""

  if (is.null(table_type)) {
    # If no table_type is selected than set text for console more or html for RMD mode
    table_type <- ifelse(af_is_in_rmd(), "html", "text")
  }

  if (is.null(notes)) {
    # If notes is null we build simple notes
    notes_align = 'r'
    append_notes = TRUE
    notes.label = NULL
    if (coef_exp) {
      cnotes <- "All estimates are exponentiated."
    } else if (!is.null(exp_ctrl)) {
      new_coef <- rep(list(NULL), length(exp_ctrl))
      for (i in seq_along(models)) {
        if (exp_ctrl[i]) {
          new_coef[[i]] <- exp(coef(models[[i]]))
        }
      }
      exp_ctrl_idxs <- which(exp_ctrl)
      cnotes <- paste(
        "Estimates of models ",
        toString(exp_ctrl_idxs),
        " are exponentiated."
      )
    }
  }

  # Print the table

  if (coef_exp) {
    suppressWarnings(
      stargazer(
        models,
        apply.coef = exp,
        p.auto = FALSE,
        t.auto = F, # Do not re-calculate p-values
        title = title,
        notes = cnotes,
        notes.append = append_notes,
        notes.align = notes_align,
        notes.label = notes_label,
        star.cutoffs = c(0.05, 0.01, 0.001),
        type = table_type,
        font.size = "small", # to make font size smaller
        column.sep.width = "1pt", # to reduce column width
        no.space = FALSE, # to remove the spaces after each line of coefficients
        dep.var.labels.include = TRUE,
        covariate.labels = cov_labels,
        column.labels = names(models),
        omit = c("Constant"),
        keep.stat = c("n", "rsq", "adj.rsq"),
        style = "all2",
        label = paste0("tab:", tag),
        out = paste0(tag, ".", out_type),
        header = FALSE # to get rid of r package output text
      )
    )
  } else {
    suppressWarnings(
      stargazer(
        models,
        coef = new_coef,
        p.auto = FALSE,
        t.auto = F, # Do not re-calculate p-values
        title = title,
        notes = cnotes,
        notes.append = append_notes,
        notes.align = notes_align,
        notes.label = notes_label,
        star.cutoffs = c(0.05, 0.01, 0.001),
        type = table_type,
        font.size = "small", # to make font size smaller
        column.sep.width = "1pt", # to reduce column width
        no.space = FALSE, # to remove the spaces after each line of coefficients
        dep.var.labels.include = TRUE,
        covariate.labels = cov_labels,
        column.labels = names(models),
        omit = c("Constant"),
        keep.stat = c("n", "rsq", "adj.rsq"),
        style = "all2",
        label = paste0("tab:", tag),
        out = paste0(tag, ".", out_type),
        header = FALSE # to get rid of r package output text
      )
    )
  }
}

#' Create regression table notes
#'
#' Generate standardized notes for regression tables including information about
#' significance levels, reference categories, sample sizes, standard error types,
#' and coefficient transformations.
#'
#' @param df A data frame containing the original data used for the regression models
#' @param models A single regression model object or a list of regression model objects
#' @param display_names A named character vector mapping variable names to display names.
#'   If NULL, original variable names are used. Default is NULL.
#' @param is_robust Logical value or vector indicating whether robust standard errors
#'   were used. If a single value is provided for multiple models, it will be recycled.
#'   Default is FALSE.
#' @param is_bootstrap Logical value or vector indicating whether bootstrapped standard
#'   errors were used. If a single value is provided for multiple models, it will be
#'   recycled. Default is FALSE.
#' @param bootstrap_iterations Integer specifying the number of bootstrap iterations
#'   used. Only relevant when is_bootstrap is TRUE. Default is 1000.
#' @param is_exp Logical value or vector indicating whether coefficients are
#'   exponentiated. If a single value is provided for multiple models, it will be
#'   recycled. Default is FALSE.
#' @param show_significance Logical indicating whether to include significance level
#'   legend in the notes. Default is FALSE.
#' @param show_n Logical indicating whether to include sample size information in
#'   the notes. Default is FALSE.
#' @param significance_levels Numeric vector of significance levels in descending
#'   order. Default is c(0.05, 0.01, 0.001).
#'
#' @return A character string containing the formatted regression table notes
#'
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' # Single model
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' af_create_regression_notes(mtcars, model, show_significance = TRUE)
#'
#' # Multiple models
#' model1 <- lm(mpg ~ wt, data = mtcars)
#' model2 <- lm(mpg ~ wt + cyl, data = mtcars)
#' af_create_regression_notes(mtcars, list(model1, model2),
#'                           is_robust = c(TRUE, FALSE),
#'                           show_n = TRUE)
#' }
#'
#' @export
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
  # Input validation
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }

  if (
    !inherits(models, "list") &&
      !inherits(models, c("lm", "glm", "lmer", "glmer"))
  ) {
    stop(
      "models must be a regression model object or a list of regression model objects"
    )
  }

  if (
    !is.null(display_names) &&
      !is.character(display_names) &&
      !is.list(display_names)
  ) {
    stop("display_names must be a character vector, list, or NULL")
  }

  if (!is.logical(is_robust)) {
    stop("is_robust must be logical")
  }

  if (!is.logical(is_bootstrap)) {
    stop("is_bootstrap must be logical")
  }

  if (!is.numeric(bootstrap_iterations) || bootstrap_iterations <= 0) {
    stop("bootstrap_iterations must be a positive number")
  }

  if (!is.logical(is_exp)) {
    stop("is_exp must be logical")
  }

  if (!is.logical(show_significance) || length(show_significance) != 1) {
    stop("show_significance must be a single logical value")
  }

  if (!is.logical(show_n) || length(show_n) != 1) {
    stop("show_n must be a single logical value")
  }

  if (
    !is.numeric(significance_levels) ||
      any(significance_levels <= 0) ||
      any(significance_levels >= 1)
  ) {
    stop("significance_levels must be numeric values between 0 and 1")
  }

  note_parts <- list()

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

  get_reference_levels <- function(model) {
    all_vars <- all.vars(formula(model))

    # Check if variables exist in df and issue warning if not
    missing_vars <- all_vars[!all_vars %in% names(df)]
    if (length(missing_vars) > 0) {
      warning(sprintf(
        "The following variables from the model are not found in df: %s",
        paste(missing_vars, collapse = ", ")
      ))
    }

    # Only process variables that exist in df
    existing_vars <- all_vars[all_vars %in% names(df)]
    factor_vars <- existing_vars[sapply(df[existing_vars], is.factor)]

    ref_levels <- character()
    for (var in factor_vars) {
      ref_level <- levels(df[[var]])[1]
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

  if (inherits(models, "list")) {
    all_ref_levels <- unique(unlist(lapply(models, get_reference_levels)))
  } else {
    all_ref_levels <- unique(get_reference_levels(models))
  }

  if (length(all_ref_levels) > 0) {
    note_parts$ref_levels <- paste0(all_ref_levels, collapse = ". ")
    note_parts$ref_levels <- paste0(note_parts$ref_levels, ".")
  }

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
      sample_sizes <- nrow(model.frame(models))
      note_parts$n <- sprintf("N = %d", sample_sizes)
    }
  }

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
    if (!is_bootstrap) {
      note_parts$robust <- "Standard errors in parentheses"
    }
  }

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
        model_nums <- bootstrap_models # Fixed: was using robust_models
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

  # Ensure all notes end with proper punctuation
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

#' Extract Variable Order from Stargazer Output
#'
#' @description
#' Extracts the ordering of coefficient names from stargazer regression table output.
#' Captures stargazer text output, filters for variable lines (excluding statistics and
#' notes), and parses the coefficient names in their display order. Useful for creating
#' consistent variable ordering across different table formats.
#'
#' @param df (data.frame) The data frame used in the regression models (not directly used but included for consistency)
#' @param models (model object or list) A single fitted model or list of models to pass to stargazer
#'
#' @return (character vector) An unnamed character vector containing coefficient names in the order they appear in stargazer output
#'
af_get_stargazer_order <- function(df, models) {
  # The function expands categorical variables into their individual levels (excluding the reference level)
  # while keeping numeric variables as-is. For categorical variables, it creates separate named entries for
  # each non-reference level, using the original variable name as the key. For numeric variables, it creates
  # a single unnamed entry.
  tmp <- capture.output(
    stargazer(models, type = "text", keep.stat = "n")
  )

  var_lines <- tmp[grep("^[[:alnum:]]|^Constant", tmp)]
  var_lines <- var_lines[
    !grepl("^Observations|^R2|^Adjusted|^Residual|^F |^Note:", var_lines)
  ]

  ordered_terms <- sapply(var_lines, function(x) {
    coef_start <- regexpr("\\s+[-]?[0-9]|\\s+\\*", x)[1]
    if (coef_start > 0) {
      return(trimws(substr(x, 1, coef_start - 1)))
    }
    return(trimws(x))
  })

  return(unname(ordered_terms))
}

#' Create Display Names for Regression Coefficients
#'
#' @description
#' Transforms technical regression coefficient names into human-readable display labels
#' for tables and plots. Maps variable names to display names, formats categorical variable
#' levels in brackets, converts interaction terms using multiplication symbols, and renames
#' the intercept. Preserves the ordering from stargazer output.
#'
#' @param df (data.frame) The data frame used in the regression models
#' @param models (model object or list) A single fitted model or list of models
#' @param display_names (named character vector) Mapping of variable names to display labels
#'
#' @return (character vector) A character vector of formatted display names in the order of coefficients, with categorical levels in brackets and interactions formatted with multiplication symbols
#'
#'
#' @examples
#' \dontrun{
#' # Example usage
#' model <- lm(mpg ~ factor(cyl) + wt + factor(gear):wt, data = mtcars)
#' display_names <- c(
#'  "factor(cyl)" = "Cylinders",
#'  "wt" = "Weight",
#'  "factor(gear)" = "Gears"
#' )
#' af_cov_names(mtcars, model, display_names)
#' }
#' @export
af_cov_names <- function(df, models, display_names) {
  # Get ordered terms from stargazer
  ordered_terms <- af_get_stargazer_order(df, models)

  # Initialize result vector
  result <- vector("character", length(ordered_terms))
  names(result) <- ordered_terms

  # Helper function to process single term
  process_term <- function(term) {
    for (base_name in names(display_names)) {
      if (startsWith(term, base_name)) {
        level <- substring(term, nchar(base_name) + 1)
        if (level != "") {
          return(sprintf("%s[%s]", display_names[base_name], level))
        } else {
          return(display_names[base_name])
        }
      }
    }
    return(term)
  }

  # Process each variable
  for (var in ordered_terms) {
    if (var == "Constant") {
      result[var] <- "Intercept"
      next
    }

    # Handle interaction terms
    if (grepl(":", var)) {
      parts <- strsplit(var, ":")[[1]]
      processed_parts <- sapply(parts, process_term)
      result[var] <- paste(processed_parts, collapse = " × ")
    } else {
      result[var] <- process_term(var)
    }
  }

  return(as.character(result))
}
