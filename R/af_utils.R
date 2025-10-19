cls <- function() {
  # Clear the console (manual action required in RStudio)
  cat("\014")  # Sends a form feed to clear console in some terminals
  
  # Clear all plots
  while (!is.null(dev.list())) dev.off()
  
  # Clear the viewer
  cat("")
  
  # Clear the environment
  rm(list = ls(envir = parent.frame()), envir = parent.frame())
}

af_add_brackets_to_levels <- function(x) {
  if (is.factor(x)) {
    levels(x) <- paste0("[", levels(x), "]")
    x
  } else {
    x
  }
}

gt_coeftest <- function(model) {
  # Perform coeftest
  coef_results <- lmtest::coeftest(model)
  
  # Convert to data frame
  coef_df <- data.frame(
    variable = rownames(coef_results),
    estimate = coef_results[, 1],                # "Estimate"
    std_error = coef_results[, 2],            # "Std. Error"
    t_value = coef_results[, 3],               # "t value"
    p_value = coef_results[, 4]               # "Pr(>|t|)"
  )
  
  # Create GT table
  gt_table <- coef_df %>%
    gt() %>%
    fmt_number(
      columns = c(estimate, std_error, t_value, p_value),
      decimals = 3
    ) %>%
    cols_label(
      variable = "Variable",
      estimate = "Coefficient",
      std_error = "Std. Error",
      t_value = "t-statistic",
      p_value = "p-value"
    ) %>%
    tab_header(
      title = "Regression Coefficient Results",
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) 
  
  # Display the table
  return(gt_table)
}

af_ggsave <- function(filename,
                      plot = last_plot(),
                      units = "mm",
                      width = 180,
                      height = 90,
                      dpi = 600,
                      display = TRUE) {
  ggsave(
    plot = plot,
    filename = filename,
    units = units,
    width = width,
    height = height,
    dpi = dpi
  )
  
  if (display)
    print(plot)
}


af_prepare_regression <- function(df, dependent_var, independent_vars) {
  notes <- character()
  valid_vars <- c()
  
  # Check if dependent variable exists
  if (!(dependent_var %in% names(df))) {
    notes <- c(notes, sprintf("Dependent variable '%s' does not exist in the data", dependent_var))
    dependent_var <- NULL
  }
  
  for (var in independent_vars) {
    if (!(var %in% names(df))) {
      notes <- c(notes, sprintf("Independent variable '%s' does not exist in the data", var))
    } else if (is.factor(df[[var]])) {
      unique_values <- unique(df[[var]])
      if (length(unique_values) == 1) {
        notes <- c(notes, sprintf("Variable '%s' had a single value of '%s'", var, as.character(unique_values)))
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
    formula <- as.formula(paste(dependent_var, "~", paste(valid_vars, collapse = " + ")))
  }
  
  # notes <- if (length(notes) > 0) paste(notes, collapse = "\n") else ""
  
  list(formula = formula, notes = notes)
}

# Create the function to process the data
af_clean_data <- function(df, 
                         critical_vars, # list of critical variable - no NA is allowed  
                         country_critical_pct = 0.7 # min complete rows required to keep the country
                         ) {
  # Initialize an empty list to store the problematic countries per wave
  problematic_countries_list <- list()
  
  # Loop through each wave
  waves <- af_order(unique(df$wave))
  for (wave in waves) {
    # Get the countries for the current wave
    countries <- unique(df[df$wave == wave, "country"])
    
    # Check the percentage of missing values in the critical variables for each country
    missing_pct <- df[df$wave == wave, c("wave", "country", critical_vars)] %>%
      group_by(country) %>%
      summarize_all(~mean(is.na(.)))
    
    # Find the countries with any missing_pct value in a critical_var column > 1-country_critical_pct
    problematic_countries <- countries[apply(missing_pct[, critical_vars], 1, function(x) any(x > 1 - country_critical_pct))]
    
    # # Print the list of problematic countries for the current wave
    # cat("Problematic countries in wave", wave, ":", paste(problematic_countries, collapse = ", "), "\n")
    
    # Store the problematic countries for the current wave
    problematic_countries_list[[as.character(wave)]] <- problematic_countries
    
    # Remove the problematic countries from the dataframe
    df <- df[!(df$wave == wave & df$country %in% problematic_countries), ]
  }
  
  # Calculate the percentage of rows with at least one NA in critical variables for the remaining countries
  percentages_df <- df %>%
    group_by(wave, country) %>%
    summarize_at(critical_vars, ~paste0(round(mean(is.na(.)) * 100, 2), "%")) %>%
    ungroup() %>%
    as.data.frame() 
  
  # Remove the rows with at least one NA in any of the critical variables from the remaining countries
  cleaned_df <- df[rowSums(is.na(df[, critical_vars])) == 0, ]
  
  # Return the cleaned dataframe, the list of problematic countries, and the data table with percentages
  return(list(df = cleaned_df, problematic_countries = problematic_countries_list, percentages = percentages_df))
  
}

# filters a list of df vars (var_list) to retain only those that contain at least one non-missing and non-NULL value.
af_remove_empty_vars <- function(df, var_list) {
  # Filter var_list to include only columns in df that have non-missing data
  var_list[sapply(var_list, function(var) {
    !all(is.na(df[[var]]) | is.null(df[[var]]))
  })]
}

# Fold survey question scale around the center
af_folded_scale <- function(score, center) 
  {ifelse(is.na(score)|is.null(score), score, abs(score-center))}

# reverse a survey question scale (from 1..7 to 7..1)
af_reverse_scale <- function(score, scale_min, scale_max) 
  {ifelse(is.na(score)|is.null(score), score, (scale_min + scale_max) - score)}

# Assign right/center/left values based on center thresholds
af_rcl <- function(pe_var, c_low, c_up) {
  factor(ifelse(pe_var < c_low, "right",
                ifelse(pe_var > c_up, "left", "center")),
         levels = c("right", "center", "left"))
}

# Assign left/center/right values based on center thresholds
af_lcr <- function(pe_var, c_low, c_up) {
  factor(ifelse(pe_var < c_low, "left",
                ifelse(pe_var > c_up, "right", "center")),
         levels = c("left", "center", "right"))
}

# calulate the sqrt((A1^2+A2^2 ... An^2)n). 
# Example: df$new <- af_distance(df$A, df$B, df$C)
af_distance <- function(...) {
  values <- as.data.frame(list(...))  # Combine inputs into a dataframe
  row_distances <- sqrt(rowMeans(values^2))  # Calculate distance for each row
  return(row_distances)  # Result is a vector, suitable for a dataframe column
}

# Calculate row means and create a new column
af_mean <- function(df, var_list) {
  result <- rowMeans(df[var_list], na.rm = TRUE)
  return(result)
}

# Calculate the distance of df columns
af_dist <- function(df, var_list) {
  result <- sqrt(rowMeans(df[var_list]^2, na.rm = TRUE))
  return(result)
}

# Calculate standard significance asterisks
af_asterisks <- function(p_value) {
  ifelse(p_value < 0.001, "***",
         ifelse(p_value < 0.01, "**", 
                ifelse(p_value < 0.05, "*", " ")))
}

# check if all values of a df variable are the same
af_is_same <- function(df, var_name) {
  return(length(unique(df[[var_name]])) == 1)
}

# returns a vector sorted in ascending order
af_order <- function(l) {l[order(l)]}

# Order a df by a column var_name
af_order_df <- function(df, var_name) {
  df[order(df[[var_name]]), ]
}

# Transpose a dataframe
af_transpose_df <- function(df) {
  df %>%
    pivot_longer(
      cols = -1,  # all columns except the first
      names_to = "Variable", 
      values_to = "Value"
    ) %>%
    pivot_wider(
      names_from = 1,  # first column 
      values_from = "Value"
    )
}

# replaces values in a df column with corresponding names from a provided labels vector,
# leaving unmatched values unchanged.
af_apply_labels <- function(df, var_name, var_labels) {
  # Check if the specified column exists in the dataframe
  if (!(var_name %in% colnames(df))) {
    stop("The specified column does not exist in the dataframe.")
  }
  
  # Replace values in the specified column using the named vector
  df[[var_name]] <- ifelse(df[[var_name]] %in% var_labels, 
                           names(var_labels)[match(df[[var_name]], var_labels)], 
                           df[[var_name]])
  
  # Return the updated dataframe
  return(df)
}

# test type of a dataframe column
af_col_type <- function(df, col_name)
{
  if (af_is_col_numeric(df, col_name)) return ("numeric") 
  else if (af_is_col_factor(df, col_name)) return("factor")
  else if (af_is_col_ordinal(df, col_name)) return("ordinal")
  else if (af_is_col_nominal(df, col_name)) return("nominal")
  else if (af_is_col_surv(df, col_name)) return("surv")
  else return("unknown")
}

# test type of a variable
af_is_numeric <- function(v) is.numeric(v)
af_is_factor <- function(v) is.factor(v) & length(levels(v))==2
af_is_ordinal <- function(v) is.factor(v) & length(levels(v))>2 & is.ordered(v)
af_is_nominal <- function(v) is.factor(v) & length(levels(v))>2 & !is.ordered(v)
af_is_ymd <- function(v)  all(!is.na(strptime(na.omit(v), "%Y-%m-%d")))

# test type of a data frame column
af_is_col_numeric <- function(df, col_name) af_is_numeric(as.data.frame(df)[,col_name])
af_is_col_factor <- function(df, col_name) af_is_factor(as.data.frame(df)[,col_name])
af_is_col_ordinal <- function(df, col_name) af_is_ordinal(as.data.frame(df)[,col_name])
af_is_col_nominal <- function(df, col_name) af_is_nominal(as.data.frame(df)[,col_name])
af_is_col_ordered <- function(df, col_name) is.ordered(as.data.frame(df)[,col_name])
af_is_col_surv <- function(df, col_name) is.Surv(as.data.frame(df)[,col_name])
af_is_col_date <- function(df, col_name) is.Date(as.data.frame(df)[,col_name])
af_is_col_ymd <-  function(df, col_name) af_is_ymd(as.data.frame(df)[,col_name])

# return the first / last element of a vector
af_first <- function(v) {return(v[1])}
af_last <- function (v) {return(v[length(v)])}

af_select_val <- function(cond,a,b) if (cond) return(a) else return(b)

af_gtsave <- function(table, output_file) {
  # Remove the file if it exists
  if (file.exists(output_file)) {
    file.remove(output_file)
  }
  
  # Save the table to a Word document
  gt::gtsave(table, output_file)
}

af_format_string_list <- function(strings) {
  n <- length(strings)
  if (n == 1) return(strings)
  if (n == 2) return(paste(strings[1], "and", strings[2]))
  return(paste(paste(strings[1:(n-1)], collapse=", "), "and", strings[n]))
}

# Convert categorical variables in Stata or SPSS dataset to factors

af_make_sav_dta_factors <- function (df) {
  # Function to check first element name in labels attribute
  check_first_element_name <- function(col_name) {
    # Get the attribute for the specific column
    col_attr <- attr(df[[col_name]], "labels")
    # Check if labels attribute exists and has non-empty elements
    if (!is.null(col_attr) && length(col_attr) > 0) {
      # Extract the first element from the labels attribute
      name1 <- names(col_attr[1])
      # Check if it's a named list and name follows the pattern
      return(!(grepl("^\\d\\s*=.*$", name1) || grepl("\n\\d$",name1)))
    }
    return(FALSE)
  }
  
  # Get column names with matching attribute
  col_names <- names(df)[sapply(names(df), check_first_element_name)]
  
  # Convert desired columns to factors with labels (keeping attributes)
  for (col_name in col_names) {
    col_attr <- attributes(df[[col_name]])
    col <- df[[col_name]]
    df[[col_name]] <- 
      factor(col, levels = attr(col, "labels"), labels = names(attr(col, "labels")))
    attr(df[[col_name]],"label") <- col_attr$label
    attr(df[[col_name]],"labels") <- col_attr$labels
  }
  return(df)
}

# Converts a numeric vector into a grammatically correct string by joining all elements with commas except the last one, which is joined with "and" - turning c(1,2,3,4) into "1,2,3 and 4".

af_comma_and_list <- function(items) {
  if (length(items) > 1)
    items_string <- paste(paste(items[-length(items)], collapse = ","), "and", items[length(items)])
  else
    items_string <- paste(items)
  
  return(items_string)
}