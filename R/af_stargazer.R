af_stargazer <- function (models, tag = "reg-out", cov_labels = NULL, 
                          coef_exp = FALSE, exp_ctrl=NULL, 
                          notes = NULL,
                          table_type = 'html', out_type = 'html') {
  
  # Parameter exp_ctrl enable to exponent only selected models' coefficients 
  # Set coef_exp to FALSE 
  # Set exp_ctrl to a Boolean vector with TRUE for each model to exponent. 
  
  new_coef <- NULL
  
  cnotes <- notes   # The default case is the notes parameter has text
  notes_align = 'l'
  append_notes <- FALSE
  notes_label <- ""
  
  if (is.null(notes)) { # If notes is null we build simple notes
    notes_align = 'r'
    append_notes = TRUE
    notes.label = NULL
    if (coef_exp) {
      cnotes <- "All estimates are exponentiated."
      } else if (!is.null(exp_ctrl)) {
        new_coef <- rep(list(NULL), length(exp_ctrl))
        for (i in seq_along(models)) {
          if (exp_ctrl[i]) {
            new_coef[[i]]<- exp(coef(models[[i]]))
          } 
        }
        exp_ctrl_idxs <- which(exp_ctrl)
        cnotes <- paste("Estimates of models ", toString(exp_ctrl_idxs), " are exponentiated.")
      }
    }
  
  
  # Print the table
  
  if (coef_exp) {
    suppressWarnings(
      stargazer(
        models,
        apply.coef = exp, p.auto=FALSE, t.auto=F, # Do not re-calculate p-values 
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
        label = paste0("tab:",tag),
        out = paste0(tag, ".", out_type),
        header=FALSE # to get rid of r package output text
      ))
  } else {
    suppressWarnings(
      stargazer(
        models,
        coef = new_coef,
        p.auto=FALSE, t.auto=F, # Do not re-calculate p-values 
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
        label = paste0("tab:",tag),
        out = paste0(tag, ".", out_type),
        header=FALSE # to get rid of r package output text
      ))
  }
}

# The function expands categorical variables into their individual levels (excluding the reference level)
# while keeping numeric variables as-is. For categorical variables, it creates separate named entries for
# each non-reference level, using the original variable name as the key. For numeric variables, it creates
# a single unnamed entry.

af_get_stargazer_order <- function(df, models) {
  tmp <- capture.output(
    stargazer(models, 
              type = "text",
              keep.stat = "n")
  )
  
  var_lines <- tmp[grep("^[[:alnum:]]|^Constant", tmp)]
  var_lines <- var_lines[!grepl("^Observations|^R2|^Adjusted|^Residual|^F |^Note:", var_lines)]
  
  ordered_terms <- sapply(var_lines, function(x) {
    coef_start <- regexpr("\\s+[-]?[0-9]|\\s+\\*", x)[1]
    if (coef_start > 0) {
      return(trimws(substr(x, 1, coef_start - 1)))
    }
    return(trimws(x))
  })
  
  return(unname(ordered_terms))
}

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