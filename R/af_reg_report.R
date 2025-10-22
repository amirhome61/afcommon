#' Interpret Regression Model Results
#'
#' @description
#' Dispatcher function that generates interpretive text for different types of regression
#' models. Routes to appropriate specialized interpretation functions based on the specified
#' regression type (OLS, Negative Binomial, Poisson, Ordinal Logistic, or Binary Logistic).
#' Returns formatted HTML text describing model fit and coefficient effects.
#'
#' @param model (model object) A fitted regression model object
#' @param df (data.frame) The data frame used to fit the model
#' @param y_name (character) The name of the dependent variable
#' @param x_list (character vector or named vector) List of independent variable names, optionally with categorical level specifications
#' @param reg_type (character) The type of regression model. Options are "OLS", "NB", "Poisson", "polr", or "logistic"
#' @param digits (numeric) Number of digits for rounding numeric output. Default is 2
#'
#' @return (character) HTML-formatted interpretation text describing the regression results, or an error message if regression type is not supported
#'
#' @export
af_reg_interpret <- function(model, df, y_name, x_list, reg_type, digits = 2) {
  if (reg_type == "OLS") {
    af_rr_ols(model, df, y_name, x_list, digits = digits)
  } else if (reg_type == "NB") {
    af_rr_nb(model, df, y_name, x_list)
  } else if (reg_type == "Poisson") {
    af_rr_poisson(model, df, y_name, x_list)
  } else if (reg_type == "polr") {
    af_rr_polr(model, df, y_name, x_list, digits = digits)
  } else if (reg_type == "logistic") {
    af_rr_logistic(model, df, y_name, x_list, digits = digits)
  } else {
    paste("regression type", reg_type, "is not supported")
  }
}

#' Interpret OLS Regression Results
#'
#' @description
#' Generates detailed interpretation text for Ordinary Least Squares (OLS) linear regression
#' results. Creates narrative descriptions of model fit, R-squared, and effects of significant
#' predictors on the outcome. Handles both numeric and categorical predictors, expressing
#' effects in units and standard deviations. Returns HTML-formatted text.
#'
#' @param model (lm) A fitted OLS linear regression model object
#' @param df (data.frame) The data frame used to fit the model
#' @param y_name (character) The name of the dependent variable
#' @param x_list (character vector or named vector) List of independent variable names, optionally with categorical level specifications
#' @param digits (numeric) Number of digits for rounding numeric output. Default is 2
#'
#' @return (character) HTML-formatted text describing the OLS regression results, including model significance, R-squared, and effects of significant predictors
#'
#' @export
af_rr_ols <- function(model, df, y_name, x_list, digits = 2) {
  tmpl_reg_type <-
    paste(
      "To examine the correlation of <ind_names> with <dep_name>,",
      "we estimated an <reg_type> model (n=<num_samples>).",
      "The model was statistically significant (p = <model_pval>) ",
      "and explained <rsquared_pct>% of the variance in <dep_name> (R² = <rsquared>)."
    )

  tmpl_cov_sig <-
    "The <cov_name> variable has statistically significant correlation."
  tmpl_effect_n <-
    paste(
      "An increase of 1 unit of <cov_name> <cov_dir> <dep_name> by",
      "<cov_aval> units, assuming all other variables stay constant."
    )
  tmpl_effect_d <-
    paste(
      "When <cov_name> is <cov_cat>, <dep_name> <cov_dir> by <cov_aval> units ",
      "compared to <cov_name> being <cov_first_cat>, ",
      "assuming all other variables stay constant."
    )
  tmpl_effect_sd <-
    paste(
      "This represents a change of more than <cov_vsd> standard deviations",
      "(SD[<dep_name>]= <dep_sd>)."
    )

  tmpl_non_sig <- "<cov_name> (coef = <cov_val>, p_val = <cov_pval>)"

  reg_type <- "OLS Linear regression"

  dep_name <- ifelse(is.null(names(y_name)), y_name, names(y_name))
  ind_names <- af_format_string_list(x_list)
  num_samples <- nobs(model)
  rsquared <- summary(model)$r.squared
  rsquared_pct <- rsquared * 100
  model_pval <- anova(model)$"Pr(>F)"[1]

  dep_sd <- sd(na.omit(df[, y_name]))
  names(dep_sd) <- NULL

  replacement <- c(
    "<reg_type>" = reg_type,
    "<dep_name>" = dep_name,
    "<num_samples>" = formatC(num_samples, format = "d"),
    "<rsquared>" = formatC(rsquared, format = "g", digits = 3),
    "<rsquared_pct>" = formatC(rsquared_pct, format = "f", digits = 1),
    "<model_pval>" = formatC(model_pval, format = "g", digits = 3),
    "<ind_names>" = ind_names
  )
  rr_shiny_text <- af_rr_par(tmpl_reg_type, replacement)

  x_list <- af_expand_x_list(df, x_list)

  numx <- length(x_list)
  reg_coef <- summary(model)$coefficients[2:(numx + 1), 1]
  names(reg_coef) <- NULL
  reg_pv <- summary(model)$coefficients[2:(numx + 1), 4]
  names(reg_pv) <- NULL
  reg_sig <- reg_pv <= 0.05

  no_sig_list <- NULL

  for (i in 1:numx) {
    if (reg_sig[i]) {
      cov_name <- names(x_list[i])
      cov_cat <- as.character(x_list[i])
      cov_val <- reg_coef[i]
      cov_aval <- abs(cov_val)
      cov_dir <- af_select_val(cov_val > 0, "increases", "decreases")
      cov_vsd <- abs(cov_val / dep_sd)

      if (cov_cat == "") {
        # Non-Categorical variable
        tmpl_effect <- tmpl_effect_n # Numeric template
        cov_first_cat <- ""
      } else {
        # Categorical variable
        tmpl_effect <- tmpl_effect_d # Categorical template
        cov_first_cat <- levels(df[[names(x_list[i])]])[1] # First level of orginal variable
      }

      tmpl_cov <- c(tmpl_cov_sig, tmpl_effect, tmpl_effect_sd)

      replacement <- c(
        "<reg_type>" = reg_type,
        "<dep_name>" = dep_name,
        "<dep_sd>" = formatC(dep_sd, format = "g", digits = digits),
        "<cov_name>" = cov_name,
        "<cov_val>" = formatC(cov_val, format = "g", digits = digits),
        "<cov_aval>" = formatC(cov_aval, format = "g", digits = digits),
        "<cov_cat>" = cov_cat,
        "<cov_first_cat>" = cov_first_cat,
        "<cov_dir>" = cov_dir,
        "<cov_vsd>" = formatC(cov_vsd, format = "g", digits = digits)
      )

      rr_shiny_text <- paste(
        rr_shiny_text,
        af_rr_par(tmpl_cov, replacement),
        sep = '<br/>'
      )
    } else {
      cov_name <- names(x_list[i])
      cov_cat <- as.character(x_list[i])
      if (cov_cat != "") {
        cov_name <- paste0(cov_name, "[", cov_cat, "]")
      }
      cov_val <- exp(reg_coef[i])
      cov_pval <- reg_pv[i]

      replacement <- c(
        "<cov_name>" = cov_name,
        "<cov_val>" = formatC(cov_val, format = "g", digits = 3),
        "<cov_pval>" = formatC(cov_pval, format = "g", digits = 3)
      )
      no_sig_list <- c(no_sig_list, af_rr_par(tmpl_non_sig, replacement))
    }
  }

  if (!is.null(no_sig_list)) {
    if (length(no_sig_list) == 1) {
      rr_shiny_text <- paste(
        rr_shiny_text,
        paste(
          "Variable",
          af_format_string_list(no_sig_list),
          "is not statistically significant."
        ),
        sep = '<br/>'
      )
    } else {
      rr_shiny_text <- paste(
        rr_shiny_text,
        paste(
          "Variables",
          af_format_string_list(no_sig_list),
          "are not statistically significant."
        ),
        sep = '<br/>'
      )
    }
  }

  return(rr_shiny_text)
}

#' Interpret Negative Binomial Regression Results
#'
#' @description
#' Generates interpretation text for Negative Binomial regression results. Creates narrative
#' descriptions of the effects of significant predictors on the count outcome, expressing
#' effects as percentage changes. Handles both numeric and categorical predictors. Returns
#' HTML-formatted text suitable for reporting.
#'
#' @param model (negbin or glm.nb) A fitted Negative Binomial regression model object
#' @param df (data.frame) The data frame used to fit the model
#' @param y_name (character) The name of the dependent variable
#' @param x_list (character vector) List of independent variable names
#'
#' @return (character) HTML-formatted text describing the Negative Binomial regression results and percentage effects of significant predictors
#'
#' @export
af_rr_nb <- function(model, df, y_name, x_list) {
  tmpl_reg_type <-
    "<reg_type> was used to analyze the effects on <dep_name>."
  tmpl_cov_sig <-
    "The <cov_name> variable has statistically significant effect."
  tmpl_effect_n <-
    paste(
      "An increase of 1 unit in <cov_name> <cov_dir> <dep_name> by",
      "<cov_aval>%, assuming all other variables stay constant."
    )
  tmpl_effect_d <-
    paste(
      "When <cov_name> is <cov_cat>, <dep_name> <cov_dir> by <cov_aval>% ",
      "assuming all other variables stay constant."
    )

  reg_type <- "Negative Binomial regression"

  dep_name <- ifelse(is.null(names(y_name)), y_name, names(y_name))
  dep_sd <- sd(df[, y_name])
  names(dep_sd) <- NULL

  replacement <- c("<reg_type>" = reg_type, "<dep_name>" = dep_name)
  rr_shiny_text <- af_rr_par(tmpl_reg_type, replacement)

  numx <- length(x_list)
  reg_coef <- summary(model)$coefficients[2:(numx + 1), 1]
  names(reg_coef) <- NULL
  reg_pv <- summary(model)$coefficients[2:(numx + 1), 4]
  names(reg_pv) <- NULL
  reg_sig <- reg_pv <= 0.05

  for (i in 1:numx) {
    if (reg_sig[i]) {
      cov_name <- ifelse(is.null(names(x_list[i])), x_list[i], names(x_list[i]))
      cov_type <- af_col_type(df, x_list[i])
      cov_val <- 100 * (exp(reg_coef[i]) - 1)
      cov_aval <- abs(cov_val)
      if (cov_type == "factor") {
        cov_cat <- af_last(levels(df[, x_list[i]]))
      } else {
        cov_cat <- ""
      }
      cov_dir <- af_select_val(cov_val > 0, "increases", "decreases")

      tmpl_effect <-
        af_select_val(cov_type == "numeric", tmpl_effect_n, tmpl_effect_d)
      tmpl_cov <- c(tmpl_cov_sig, tmpl_effect)

      replacement <- c(
        "<reg_type>" = reg_type,
        "<dep_name>" = dep_name,
        "<dep_sd>" = formatC(dep_sd, format = "g"),
        "<cov_name>" = cov_name,
        "<cov_type>" = cov_type,
        "<cov_val>" = formatC(cov_val, format = "g"),
        "<cov_aval>" = formatC(cov_aval, format = "g"),
        "<cov_cat>" = cov_cat,
        "<cov_dir>" = cov_dir
      )

      rr_shiny_text <- paste(
        rr_shiny_text,
        af_rr_par(tmpl_cov, replacement),
        sep = '<br/>'
      )
    }
  }

  return(rr_shiny_text)
}

#' Interpret Poisson Regression Results
#'
#' @description
#' Generates interpretation text for Poisson regression results. Creates narrative descriptions
#' of the effects of significant predictors on the count outcome, expressing effects as
#' percentage changes. Handles both numeric and categorical predictors. Returns HTML-formatted
#' text suitable for reporting.
#'
#' @param model (glm) A fitted Poisson regression model object
#' @param df (data.frame) The data frame used to fit the model
#' @param y_name (character) The name of the dependent variable
#' @param x_list (character vector) List of independent variable names
#'
#' @return (character) HTML-formatted text describing the Poisson regression results and percentage effects of significant predictors
#'
#' @export
af_rr_poisson <- function(model, df, y_name, x_list) {
  tmpl_reg_type <-
    "<reg_type> was used to analyze the effects on <dep_name>."
  tmpl_cov_sig <-
    "The <cov_name> variable has statistically significant effect."
  tmpl_effect_n <-
    paste(
      "An increase of 1 unit in <cov_name> <cov_dir> <dep_name> by",
      "<cov_aval>%, assuming all other variables stay constant."
    )
  tmpl_effect_d <-
    paste(
      "When <cov_name> is <cov_cat>, <dep_name> <cov_dir> by <cov_aval>% ",
      "assuming all other variables stay constant."
    )

  reg_type <- "Poisson regression"

  dep_name <- y_name
  dep_sd <- sd(df[, y_name])

  replacement <- c("<reg_type>" = reg_type, "<dep_name>" = dep_name)
  rr_shiny_text <- af_rr_par(tmpl_reg_type, replacement)

  numx <- length(x_list)
  reg_coef <- summary(model)$coefficients[2:(numx + 1), 1]
  names(reg_coef) <- NULL
  reg_pv <- summary(model)$coefficients[2:(numx + 1), 4]
  names(reg_pv) <- NULL
  reg_sig <- reg_pv <= 0.05

  for (i in 1:numx) {
    if (reg_sig[i]) {
      cov_name <- x_list[i]
      cov_type <- af_col_type(df, cov_name)
      cov_val <- 100 * (exp(reg_coef[i]) - 1)
      cov_aval <- abs(cov_val)
      if (cov_type == "factor") {
        cov_cat <- af_last(levels(df[, cov_name]))
      } else {
        cov_cat <- ""
      }
      cov_dir <- af_select_val(cov_val > 0, "increases", "decreases")

      tmpl_effect <-
        af_select_val(cov_type == "numeric", tmpl_effect_n, tmpl_effect_d)
      tmpl_cov <- c(tmpl_cov_sig, tmpl_effect)

      replacement <- c(
        "<reg_type>" = reg_type,
        "<dep_name>" = dep_name,
        "<dep_sd>" = formatC(dep_sd, format = "g"),
        "<cov_name>" = cov_name,
        "<cov_type>" = cov_type,
        "<cov_val>" = formatC(cov_val, format = "g"),
        "<cov_aval>" = formatC(cov_aval, format = "g"),
        "<cov_cat>" = cov_cat,
        "<cov_dir>" = cov_dir
      )

      rr_shiny_text <- paste(
        rr_shiny_text,
        af_rr_par(tmpl_cov, replacement),
        sep = '<br/>'
      )
    }
  }

  return(rr_shiny_text)
}

#' Interpret Ordinal Logistic Regression Results
#'
#' @description
#' Generates interpretation text for Ordinal Proportional Odds Logistic regression (polr)
#' results. Creates narrative descriptions of model fit statistics, including chi-square test,
#' log-likelihood, and pseudo R-squared, along with effects of significant predictors on the
#' likelihood of moving between ordinal categories. Returns HTML-formatted text.
#'
#' @param model (polr) A fitted ordinal logistic regression model object from MASS::polr
#' @param df (data.frame) The data frame used to fit the model
#' @param y_name (character) The name of the dependent variable
#' @param x_list (character vector or named vector) List of independent variable names, optionally with categorical level specifications
#' @param digits (numeric) Number of digits for rounding numeric output
#'
#' @return (character) HTML-formatted text describing the ordinal logistic regression results, including model fit statistics and percentage effects on odds of category transition
#'
#' @import lmtest
#' @import DescTools
#'
#' @export
af_rr_polr <- function(model, df, y_name, x_list, digits) {
  tmpl_reg_type <-
    paste(
      "To examine the correlation of <ind_names> with <dep_name>,",
      "we estimated a <reg_type> model (n=<num_samples>).",
      "The dependent variable <dep_name> has <dep_num_levels> levels: <dep_levels>.",
      "The model yielded a significant result [χ²(<xi_df>) = <xi_stat>, p < <xi_pval>],",
      "with a log-likelihood value of <log_likelihood> and pseudo R² = <pseudo_r2>."
    )

  tmpl_cov_sig <-
    "The <cov_name> variable has statistically significant correlation."
  tmpl_effect_n <-
    paste(
      "An increase of 1 unit in <cov_name> <cov_dir> the likelyhood to move from one",
      "category of <dep_name> to the next one by <cov_aval>%, ",
      "assuming all other variables stay constant."
    )
  tmpl_effect_d <-
    paste(
      "When <cov_name> is <cov_cat>, the odds to move from one",
      "category of <dep_name> to the next one <cov_dir> by <cov_aval>% ",
      "compared to <cov_name> being <cov_first_cat>,",
      "assuming all other variables stay constant."
    )

  tmpl_non_sig <- "<cov_name> (odds ratio = <cov_val>, p_val = <cov_pval>)"

  reg_type <- "Ordinal Proportional Odds Logistic regression"

  test_res <- lmtest::lrtest(model)
  xi_df <- abs(test_res$Df[2])
  xi_stat <- test_res$Chisq[2]
  xi_pval <- test_res$`Pr(>Chisq)`[2]
  log_likelihood <- as.numeric(stats::logLik(model))
  pseudo_r2 <- as.numeric(DescTools::PseudoR2(model))

  dep_name <- y_name
  dep_levels <- af_format_string_list(levels(df[[dep_name]]))
  dep_num_levels <- length(levels(df[[dep_name]]))
  ind_names <- af_format_string_list(x_list)

  num_samples <- nobs(model)

  replacement <- c(
    "<reg_type>" = reg_type,
    "<xi_df>" = formatC(xi_df, format = "d"),
    "<xi_stat>" = formatC(xi_stat, format = "g", digits = 3),
    "<xi_pval>" = formatC(xi_pval, format = "g", digits = 3),
    "<log_likelihood>" = formatC(log_likelihood, format = "g", digits = 3),
    "<pseudo_r2>" = formatC(pseudo_r2, format = "g", digits = 3),
    "<num_samples>" = formatC(num_samples, format = "d"),
    "<dep_name>" = dep_name,
    "<dep_levels>" = dep_levels,
    "<dep_num_levels>" = formatC(dep_num_levels, format = "d"),
    "<ind_names>" = ind_names
  )
  rr_shiny_text <- af_rr_par(tmpl_reg_type, replacement)

  x_list <- af_expand_x_list(df, x_list)

  numx <- length(x_list)
  coef_tbl <- lmtest::coeftest(model)
  reg_coef <- coef_tbl[1:numx, 1]
  names(reg_coef) <- NULL
  reg_pv <- coef_tbl[1:numx, 4]
  names(reg_pv) <- NULL
  reg_sig <- reg_pv <= 0.05

  no_sig_list <- NULL

  for (i in 1:numx) {
    if (reg_sig[i]) {
      cov_name <- names(x_list[i])
      cov_cat <- as.character(x_list[i])
      cov_val <- 100 * (exp(reg_coef[i]) - 1)
      cov_aval <- abs(cov_val)
      cov_dir <- af_select_val(cov_val > 0, "increases", "decreases")

      if (cov_cat == "") {
        # Non-Categorical variable
        tmpl_effect <- tmpl_effect_n # Numeric template
        cov_first_cat <- ""
      } else {
        # Categorical variable
        tmpl_effect <- tmpl_effect_d # Categorical template
        cov_first_cat <- levels(df[[names(x_list[i])]])[1] # First level of orginal variable
      }

      tmpl_cov <- c(tmpl_cov_sig, tmpl_effect)

      replacement <- c(
        "<reg_type>" = reg_type,
        "<dep_name>" = dep_name,
        "<cov_name>" = cov_name,
        "<cov_val>" = formatC(cov_val, format = "g"),
        "<cov_aval>" = formatC(cov_aval, format = "g"),
        "<cov_cat>" = cov_cat,
        "<cov_first_cat>" = cov_first_cat,
        "<cov_dir>" = cov_dir
      )

      rr_shiny_text <- paste(
        rr_shiny_text,
        af_rr_par(tmpl_cov, replacement),
        sep = '<br/>'
      )
    } else {
      cov_name <- names(x_list[i])
      cov_cat <- as.character(x_list[i])
      if (cov_cat != "") {
        cov_name <- paste0(cov_name, "[", cov_cat, "]")
      }
      cov_val <- exp(reg_coef[i])
      cov_pval <- reg_pv[i]

      replacement <- c(
        "<cov_name>" = cov_name,
        "<cov_val>" = formatC(cov_val, format = "g", digits = 3),
        "<cov_pval>" = formatC(cov_pval, format = "g", digits = 3)
      )
      no_sig_list <- c(no_sig_list, af_rr_par(tmpl_non_sig, replacement))
    }
  }

  if (!is.null(no_sig_list)) {
    if (length(no_sig_list) == 1) {
      rr_shiny_text <- paste(
        rr_shiny_text,
        paste(
          "Variable",
          af_format_string_list(no_sig_list),
          "is not statistically significant."
        ),
        sep = '<br/>'
      )
    } else {
      rr_shiny_text <- paste(
        rr_shiny_text,
        paste(
          "Variables",
          af_format_string_list(no_sig_list),
          "are not statistically significant."
        ),
        sep = '<br/>'
      )
    }
  }

  return(rr_shiny_text)
}

#' Interpret Binary Logistic Regression Results
#'
#' @description
#' Generates interpretation text for binary logistic regression results. Creates narrative
#' descriptions of model fit statistics, including chi-square test, log-likelihood, and pseudo
#' R-squared, along with effects of significant predictors on the odds of the outcome. Expresses
#' effects as percentage changes in odds. Returns HTML-formatted text.
#'
#' @param model (glm) A fitted binary logistic regression model object
#' @param df (data.frame) The data frame used to fit the model
#' @param y_name (character) The name of the dependent variable
#' @param x_list (character vector or named vector) List of independent variable names, optionally with categorical level specifications
#' @param digits (numeric) Number of digits for rounding numeric output
#'
#' @return (character) HTML-formatted text describing the logistic regression results, including model fit statistics and percentage effects on odds ratios
#'
#' @import lmtest
#' @import DescTools
#'
#' @export
af_rr_logistic <- function(model, df, y_name, x_list, digits) {
  tmpl_reg_type <-
    paste(
      "To examine the correlation of <ind_names> with <dep_name>,",
      "we estimated a <reg_type> model (n=<num_samples>).",
      "The dependent variable <dep_name> has two levels: <dep_levels>.",
      "The model yielded a significant result [χ²(<xi_df>) = <xi_stat>, p < <xi_pval>],",
      "with a log-likelihood value of <log_likelihood> and pseudo R² = <pseudo_r2>."
    )

  tmpl_cov_sig <-
    "The <cov_name> variable has statistically significant correlation."
  tmpl_effect_n <-
    paste(
      "An increase of 1 unit in <cov_name> <cov_dir> the odds of",
      "<dep_name> being <dep_cat> by <cov_aval>%,",
      "assuming all other variables stay constant."
    )
  tmpl_effect_d <-
    paste(
      "When <cov_name> is <cov_cat>, the odds of",
      "<dep_name> being <dep_cat> <cov_dir> by <cov_aval>% compared to <cov_name> being <cov_first_cat>,",
      "assuming all other variables stay constant."
    )

  tmpl_non_sig <- "<cov_name> (odds ratio = <cov_val>, p_val = <cov_pval>)"

  reg_type <- "Logistic regression"

  test_res <- lmtest::lrtest(model)
  xi_df <- abs(test_res$Df[2])
  xi_stat <- test_res$Chisq[2]
  xi_pval <- test_res$`Pr(>Chisq)`[2]
  log_likelihood <- as.numeric(stats::logLik(model))
  pseudo_r2 <- as.numeric(DescTools::PseudoR2(model))

  dep_name <- y_name
  dep_levels <- af_format_string_list(levels(df[[dep_name]]))
  dep_cat <- af_last(levels(df[, dep_name]))
  ind_names <- af_format_string_list(x_list)

  num_samples <- nobs(model)

  replacement <- c(
    "<reg_type>" = reg_type,
    "<xi_df>" = formatC(xi_df, format = "d"),
    "<xi_stat>" = formatC(xi_stat, format = "g", digits = 3),
    "<xi_pval>" = formatC(xi_pval, format = "g", digits = 3),
    "<log_likelihood>" = formatC(log_likelihood, format = "g", digits = 3),
    "<pseudo_r2>" = formatC(pseudo_r2, format = "g", digits = 3),
    "<num_samples>" = formatC(num_samples, format = "d"),
    "<dep_name>" = dep_name,
    "<dep_levels>" = dep_levels,
    "<ind_names>" = ind_names
  )
  rr_shiny_text <- af_rr_par(tmpl_reg_type, replacement)

  x_list <- af_expand_x_list(df, x_list)

  numx <- length(x_list)
  reg_coef <- summary(model)$coefficients[2:(numx + 1), 1]
  names(reg_coef) <- NULL
  reg_pv <- summary(model)$coefficients[2:(numx + 1), 4]
  names(reg_pv) <- NULL
  reg_sig <- reg_pv <= 0.05

  no_sig_list <- NULL

  for (i in 1:numx) {
    if (reg_sig[i]) {
      cov_name <- names(x_list[i])
      cov_cat <- as.character(x_list[i])
      cov_val <- 100 * (exp(reg_coef[i]) - 1)
      cov_aval <- abs(cov_val)
      cov_dir <- af_select_val(cov_val > 0, "increases", "decreases")

      if (cov_cat == "") {
        # Non-Categorical variable
        tmpl_effect <- tmpl_effect_n # Numeric template
        cov_first_cat <- ""
      } else {
        # Categorical variable
        tmpl_effect <- tmpl_effect_d # Categorical template
        cov_first_cat <- levels(df[[names(x_list[i])]])[1] # First level of orginal variable
      }

      tmpl_cov <- c(tmpl_cov_sig, tmpl_effect)

      replacement <- c(
        "<reg_type>" = reg_type,
        "<dep_name>" = dep_name,
        "<dep_cat>" = dep_cat,
        "<cov_name>" = cov_name,
        "<cov_val>" = formatC(cov_val, format = "g"),
        "<cov_aval>" = formatC(cov_aval, format = "g"),
        "<cov_cat>" = cov_cat,
        "<cov_first_cat>" = cov_first_cat,
        "<cov_dir>" = cov_dir
      )

      rr_shiny_text <- paste(
        rr_shiny_text,
        af_rr_par(tmpl_cov, replacement),
        sep = '<br/>'
      )
    } else {
      cov_name <- names(x_list[i])
      cov_cat <- as.character(x_list[i])
      if (cov_cat != "") {
        cov_name <- paste0(cov_name, "[", cov_cat, "]")
      }
      cov_val <- exp(reg_coef[i])
      cov_pval <- reg_pv[i]

      replacement <- c(
        "<cov_name>" = cov_name,
        "<cov_val>" = formatC(cov_val, format = "g", digits = 3),
        "<cov_pval>" = formatC(cov_pval, format = "g", digits = 3)
      )
      no_sig_list <- c(no_sig_list, af_rr_par(tmpl_non_sig, replacement))
    }
  }

  if (!is.null(no_sig_list)) {
    if (length(no_sig_list) == 1) {
      rr_shiny_text <- paste(
        rr_shiny_text,
        paste(
          "Variable",
          af_format_string_list(no_sig_list),
          "is not statistically significant."
        ),
        sep = '<br/>'
      )
    } else {
      rr_shiny_text <- paste(
        rr_shiny_text,
        paste(
          "Variables",
          af_format_string_list(no_sig_list),
          "are not statistically significant."
        ),
        sep = '<br/>'
      )
    }
  }

  return(rr_shiny_text)
}

#' Replace Placeholders in Template Text
#'
#' @description
#' Helper function that performs string replacement of placeholders in template text. Takes
#' a template string or vector of strings containing placeholders (e.g., "<variable_name>")
#' and replaces them with corresponding values from a named replacement vector. Returns the
#' processed text as a single string.
#'
#' @param template (character or character vector) Template text containing placeholders to be replaced
#' @param replacement (named character vector) Named vector where names are placeholders and values are replacement text
#'
#' @return (character) A single string with all placeholders replaced by their corresponding values
#'
#' @import stringr
#'
#' @export
af_rr_par <- function(template, replacement) {
  par <- template %>%
    stringr::str_c(collapse = " ") %>%
    stringr::str_replace_all(replacement)

  return(par)
}
