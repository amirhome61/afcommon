#' Check if R code is running within an RMarkdown document
#'
#' @description
#' This function determines whether the current R code is being executed
#' within an RMarkdown document during knitting/rendering, as opposed to
#' running in the R console or as a standalone script.
#'
#' @details
#' The function checks the 'knitr.in.progress' option which is set to TRUE
#' when knitr is actively processing RMarkdown chunks. This provides a reliable
#' way to detect the execution context and can be useful for conditional
#' behavior such as adjusting plot output, formatting, or display options
#' based on whether code is running interactively or in a document.
#'
#' @examples
#' if (af_is_in_rmd()) {
#'   cat("This code is running in an RMarkdown document\n")
#' } else {
#'   cat("This code is running in the console or script\n")
#' }
#'
#' # Use in conditional plotting
#' if (af_is_in_rmd()) {
#'   # Smaller plots for documents
#'   options(repr.plot.width = 6, repr.plot.height = 4)
#' }
#'
#' @importFrom knitr is_html_output
#' @importFrom rlang is_true
#'
#' @seealso \code{\link[base]{interactive}} for detecting interactive sessions
#'
#' @return Logical value: TRUE if running in RMarkdown, FALSE otherwise
#'
#' @export
af_is_in_rmd <- function() {
  return(isTRUE(getOption('knitr.in.progress')))
}

#' Create Categorical Groups from Continuous Variable
#'
#' This function converts a continuous variable into categorical groups
#' based on specified breakpoints and labels. Can be used for age, education years,
#' income levels, or any other continuous variable.
#'
#' @param numeric_vector Numeric vector of continuous values
#' @param breaks Numeric vector of breakpoints (e.g., c(18, 30, 45, 60, Inf))
#' @param labels Character vector of group labels (e.g., c("18-30", "31-45", "46-60", "60+"))
#' @param include_lowest Logical, whether to include the lowest break point (default: TRUE)
#' @param right Logical, whether intervals are right-closed (default: FALSE)
#'
#' @return Factor vector of categorical groups
#'
#' @examples
#' # Age groups
#' ages <- c(25, 35, 45, 55, 65)
#' af_create_groups(ages, c(18, 30, 45, 60, Inf), c("18-30", "31-45", "46-60", "60+"))
#'
#' # Education years
#' edu_years <- c(12, 14, 16, 18, 20)
#' af_create_groups(edu_years, c(0, 12, 16, 20, Inf), c("High School", "Some College", "Bachelor+", "Advanced"))
#'
#' @export
af_create_groups <- function(
  numeric_vector,
  breaks,
  labels,
  include_lowest = TRUE,
  right = FALSE
) {
  # Input validation
  if (!is.numeric(numeric_vector)) {
    stop("Error: 'numeric_vector' must be numeric")
  }

  if (!is.numeric(breaks)) {
    stop("Error: 'breaks' must be numeric")
  }

  if (!is.character(labels)) {
    stop("Error: 'labels' must be character")
  }

  if (length(breaks) != (length(labels) + 1)) {
    stop("Error: Length of 'breaks' must be one more than length of 'labels'")
  }

  if (!is.logical(include_lowest) || length(include_lowest) != 1) {
    stop("Error: 'include_lowest' must be a single logical value")
  }

  if (!is.logical(right) || length(right) != 1) {
    stop("Error: 'right' must be a single logical value")
  }

  if (any(is.na(numeric_vector))) {
    warning(
      "Warning: 'numeric_vector' contains NA values. These will result in NA groups."
    )
  }

  if (any(breaks[-length(breaks)] >= breaks[-1])) {
    stop("Error: 'breaks' must be in strictly increasing order")
  }

  if (min(numeric_vector, na.rm = TRUE) < breaks[1]) {
    warning(
      "Warning: Some values are below the minimum break point and will result in NA values"
    )
  }

  if (
    max(numeric_vector, na.rm = TRUE) >= breaks[length(breaks)] &&
      !is.infinite(breaks[length(breaks)])
  ) {
    warning(
      "Warning: Some values are at or above the maximum break point and may result in NA values"
    )
  }

  # Create categorical groups
  categorical_groups <- cut(
    numeric_vector,
    breaks = breaks,
    labels = labels,
    include.lowest = include_lowest,
    right = right
  )

  # Check if any values couldn't be categorized
  na_count <- sum(is.na(categorical_groups)) - sum(is.na(numeric_vector))
  if (na_count > 0) {
    warning(paste(
      "Warning:",
      na_count,
      "values could not be categorized into groups"
    ))
  }

  return(categorical_groups)
}

#' Convert 'none' selection to NULL for virtualSelectInput
#'
#' @description
#' This function is designed to work with virtualSelectInput widgets in Shiny
#' applications. It converts the string 'none' to NULL, which is useful when
#' you want to represent "no selection" as NULL rather than as a character string.
#'
#' @details
#' Many Shiny input widgets, including virtualSelectInput, return character strings
#' even when representing "no selection" states. This function provides a simple
#' way to convert the 'none' string to NULL, which is often more appropriate for
#' downstream processing, conditional logic, or database operations where NULL
#' represents missing or unselected values.
#'
#' @param c A character string or other value from a virtualSelectInput selection
#'
#' @return NULL if the input is 'none', otherwise returns the input unchanged
#'
#' @examples
#' #' af_none("none")     # Returns NULL
#' af_none("option1")  # Returns "option1"
#' af_none(NULL)       # Returns NULL
#'
#' # In Shiny reactive context
#' # selected_value <- af_none(input$my_virtual_select)
#' # if (is.null(selected_value)) {
#' #   # Handle no selection case
#' # }
#'
#' @seealso \code{\link[shinyWidgets]{virtualSelectInput}} for the input widget this function is designed to work with
#'
#' @export
af_none <- function(c) {
  if (c == 'none') return(NULL) else return(c)
}

#' Display R Output in Formatted HTML Block
#'
#' Formats and displays R output within an HTML pre-formatted block with
#' monospace styling, gray background, and border. Useful for displaying
#' code output in HTML contexts such as R Markdown documents or Shiny applications.
#'
#' @param results An R object or expression whose output will be captured and
#'   displayed. This can be any object that produces output when printed or
#'   evaluated (e.g., data frames, model summaries, vectors, etc.).
#'
#' @return None. This function is called for its side effect of printing
#'   HTML-formatted output to the console or output device.
#'
#' @examples
#' # Display a data frame
#' af_cat(head(mtcars))
#'
#' # Display model summary
#' model <- lm(mpg ~ wt, data = mtcars)
#' af_cat(summary(model))
#'
#' # Display a vector
#' af_cat(1:10)
#'
#' @export
af_cat <- function(results) {
  cat(
    "<pre style='font-family: monospace; font-size: 12px; background-color: #f5f5f5; padding: 10px; border: 1px solid #ddd;'>"
  )
  cat(paste(capture.output(results), collapse = "\n"))
  cat("</pre>")
}

#' Clear Console, Plots, Viewer, and Environment
#'
#' @description
#' This function clears the R console, closes all open plot devices,
#' clears the viewer pane, and removes all objects from the global environment.
#' Useful for resetting the R session state during interactive work.
#'
#' @return None. This function is called for its side effects.
#'
#' @examples
#' #' # Clear the R session state
#' cls()
#'
#' @export
cls <- function() {
  # Clear the console (manual action required in RStudio)
  cat("\014") # Sends a form feed to clear console in some terminals

  # Clear all plots
  while (!is.null(dev.list())) {
    dev.off()
  }

  # Clear the viewer
  cat("")

  # Clear the environment
  rm(list = ls(envir = parent.frame()), envir = parent.frame())
}

#' Add Brackets to Factor Levels
#'
#' @description
#' This function adds square brackets around the levels of a factor variable.
#' If the input is not a factor, it returns the input unchanged.
#'
#' @param x A vector, potentially a factor
#'
#' @return A factor with levels enclosed in square brackets, or the original input if not a factor
#'
#' @examples
#' # Example with a factor
#' f <- factor(c("A", "B", "C"))
#' af_add_brackets_to_levels(f)
#' # Example with a non-factor
#' v <- c("A", "B", "C")
#' af_add_brackets_to_levels(v)
#'
#' @export
af_add_brackets_to_levels <- function(x) {
  if (is.factor(x)) {
    levels(x) <- paste0("[", levels(x), "]")
    x
  } else {
    x
  }
}

#' Create Formatted Coefficient Test Table
#'
#' @description
#' Performs coefficient testing on a regression model using lmtest::coeftest and formats
#' the results into a professional gt table. Displays coefficient estimates, standard errors,
#' t-statistics, and p-values with appropriate formatting and column labels. Provides a
#' publication-ready summary of regression coefficients.
#'
#' @param model (model object) A fitted regression model object (lm, glm, etc.)
#'
#' @return (gt) A formatted gt table object containing coefficient estimates, standard errors, t-statistics, and p-values
#'
#' @import lmtest
#' @import gt
#' @import dplyr
#'
#' @examples
#' model <- lm(mpg ~ hp + wt, data = mtcars)
#' gt_coeftest(model)
#'
#' @export
gt_coeftest <- function(model) {
  # Perform coeftest
  coef_results <- lmtest::coeftest(model)

  # Convert to data frame
  coef_df <- data.frame(
    variable = rownames(coef_results),
    estimate = coef_results[, 1], # "Estimate"
    std_error = coef_results[, 2], # "Std. Error"
    t_value = coef_results[, 3], # "t value"
    p_value = coef_results[, 4] # "Pr(>|t|)"
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

#' Save ggplot with specified dimensions and DPI, optionally display it
#'
#' @description
#' This function saves a ggplot object to a file with specified width, height, and DPI.
#' It also provides an option to display the plot after saving.
#'
#' @param filename The file path to save the plot
#' @param plot The ggplot object to save (default: last_plot())
#' @param units Units for width and height (default: "mm")
#' @param width Width of the plot (default: 180)
#' @param height Height of the plot (default: 90)
#' @param dpi Dots per inch for the saved plot (default: 600)
#' @param display Logical, whether to display the plot after saving (default: TRUE)
#'
#' @return None. The function saves the plot to a file and optionally displays it.
#'
#' @examples
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' af_ggsave("mtcars_plot.png", plot = p, width = 200, height = 100, dpi = 300)
#'
#' @export
af_ggsave <- function(
  filename,
  plot = last_plot(),
  units = "mm",
  width = 180,
  height = 90,
  dpi = 600,
  display = TRUE
) {
  ggsave(
    plot = plot,
    filename = filename,
    units = units,
    width = width,
    height = height,
    dpi = dpi
  )

  if (display) {
    print(plot)
  }
}

#' Clean Data by Removing Countries and Rows with Missing Critical Variables
#'
#' @description
#' Cleans a longitudinal dataset by identifying and removing countries with excessive
#' missing data in critical variables, then removing any remaining rows with missing values.
#' For each wave, calculates the percentage of complete rows per country and removes countries
#' below the threshold. Returns cleaned data, list of removed countries, and summary statistics.
#'
#' @param df (data.frame) The input data frame containing wave and country columns
#' @param critical_vars (character vector) List of critical variable names where no NA values are allowed
#' @param country_critical_pct (numeric) Minimum proportion of complete rows required to keep a country in each wave. Default is 0.7
#'
#' @return (list) A named list with three elements: df (cleaned data frame with no missing values in critical variables), problematic_countries (list of removed countries by wave), and percentages (data frame showing missing percentages for remaining countries)
#'
#' @import dplyr
#'
#' @export
af_clean_data <- function(
  df,
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
      summarize_all(~ mean(is.na(.)))

    # Find the countries with any missing_pct value in a critical_var column > 1-country_critical_pct
    problematic_countries <- countries[apply(
      missing_pct[, critical_vars],
      1,
      function(x) any(x > 1 - country_critical_pct)
    )]

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
    summarize_at(
      critical_vars,
      ~ paste0(round(mean(is.na(.)) * 100, 2), "%")
    ) %>%
    ungroup() %>%
    as.data.frame()

  # Remove the rows with at least one NA in any of the critical variables from the remaining countries
  cleaned_df <- df[rowSums(is.na(df[, critical_vars])) == 0, ]

  # Return the cleaned dataframe, the list of problematic countries, and the data table with percentages
  return(list(
    df = cleaned_df,
    problematic_countries = problematic_countries_list,
    percentages = percentages_df
  ))
}

#' Remove variables from var_list that are completely empty in df
#'
#' @description
#' This function checks a data frame for variables listed in var_list and removes
#' those that contain only missing values (NA or NULL). It returns a character vector
#' of variable names that have at least some non-missing data.
#'
#' @param df Data frame to check for empty variables
#' @param var_list Character vector of variable names to check
#'
#' @return Character vector of variable names from var_list that have non-missing data in df
#'
#' @examples
#' df <- data.frame(A = c(1, 2, NA), B = c(NA, NA, NA), C = c(3, 4, 5))
#' var_list <- c("A", "B", "C")
#' af_remove_empty_vars(df, var_list)
#'
#'
af_remove_empty_vars <- function(df, var_list) {
  # Filter var_list to include only columns in df that have non-missing data
  var_list[sapply(var_list, function(var) {
    !all(is.na(df[[var]]) | is.null(df[[var]]))
  })]
}

#' Fold survey question scale around the center
#'
#' @description
#' This function folds a survey question scale around a specified center point.
#' It calculates the absolute difference between each score and the center,
#' effectively transforming the scale to reflect distance from the center.
#'
#' @param score Numeric vector of survey scores
#' @param center Numeric value representing the center point of the scale
#'
#' @return Numeric vector of folded scores
#'
#' @examples
#' scores <- c(1, 2, 3, 4, 5, 6, 7)
#' af_folded_scale(scores, center = 4)
#'
#' @export
af_folded_scale <- function(score, center) {
  ifelse(is.na(score) | is.null(score), score, abs(score - center))
}

#' reverse a survey question scale (from 1..7 to 7..1)
#'
#' @description
#' This function reverses a survey question scale by transforming each score
#' based on the provided minimum and maximum scale values.
#' It effectively flips the scale, so that higher scores become lower scores and vice versa.
#'
#' @param score Numeric vector of survey scores
#' @param scale_min Numeric value representing the minimum value of the scale
#' @param scale_max Numeric value representing the maximum value of the scale
#'
#' @return Numeric vector of reversed scores
#'
#' @examples
#' scores <- c(1, 2, 3, 4, 5, 6, 7)
#' af_reverse_scale(scores, scale_min = 1, scale_max = 7)
#'
#' @export
af_reverse_scale <- function(score, scale_min, scale_max) {
  ifelse(is.na(score) | is.null(score), score, (scale_min + scale_max) - score)
}

#' Assign right/center/left values based on center thresholds
#'
#' @description
#' This function categorizes a continuous variable into three groups:
#' "right", "center", and "left" based on specified lower and upper center thresholds.
#' Values below the lower threshold are labeled "right",
#' values above the upper threshold are labeled "left",
#' and values within the thresholds are labeled "center".
#'
#' @param pe_var Numeric vector of continuous values
#' @param c_low Numeric value representing the lower center threshold
#' @param c_up Numeric value representing the upper center threshold
#'
#' @return Factor vector with levels "right", "center", and "left"
#'
#' @examples
#' pe_values <- c(1, 3, 5, 7, 9)
#' af_rcl(pe_values, c_low = 4, c_up = 6)
#'
#' @export
af_rcl <- function(pe_var, c_low, c_up) {
  factor(
    ifelse(pe_var < c_low, "right", ifelse(pe_var > c_up, "left", "center")),
    levels = c("right", "center", "left")
  )
}

#' Assign left/center/right values based on center thresholds
#'
#' @description
#' This function categorizes a continuous variable into three groups:
#' "left", "center", and "right" based on specified lower and upper center
#' thresholds. Values below the lower threshold are labeled "left",
#' values above the upper threshold are labeled "right",
#' and values within the thresholds are labeled "center".
#'
#' @param pe_var Numeric vector of continuous values
#' @param c_low Numeric value representing the lower center threshold
#' @param c_up Numeric value representing the upper center threshold
#'
#' @return Factor vector with levels "left", "center", and "right"
#'
#' @examples
#' pe_values <- c(1, 3, 5, 7, 9)
#' af_lcr(pe_values, c_low = 4, c_up = 6)
#'
#' @export
af_lcr <- function(pe_var, c_low, c_up) {
  factor(
    ifelse(pe_var < c_low, "left", ifelse(pe_var > c_up, "right", "center")),
    levels = c("left", "center", "right")
  )
}

#' calulate the sqrt((A1^2+A2^2 ... An^2)n) for each row
#'
#' @description
#' This function calculates the root mean square (RMS) distance for each row
#' across multiple numeric vectors. It computes the square root of the average
#' of the squared values for each row, effectively measuring the distance from
#' the origin in a multi-dimensional space.
#'
#' @param ... Numeric vectors representing different dimensions
#'
#' @return Numeric vector of RMS distances for each row
#'
#' @examples
#' df <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6), C = c(7, 8, 9))
#' df$new <- af_distance(df$A, df$B, df$C)
#'
#' @export
af_distance <- function(...) {
  values <- as.data.frame(list(...)) # Combine inputs into a dataframe
  row_distances <- sqrt(rowMeans(values^2)) # Calculate distance for each row
  return(row_distances) # Result is a vector, suitable for a dataframe column
}

#' Calculate Row Means
#'
#' @description
#' Calculates the mean value across specified columns for each row in a data frame.
#' Missing values are excluded from the calculation using na.rm = TRUE. Returns a
#' numeric vector with the row-wise means.
#'
#' @param df (data.frame) The data frame containing the variables
#' @param var_list (character vector) Names of columns to include in the mean calculation
#'
#' @return (numeric vector) A numeric vector containing the mean value for each row
#'
#' @export
af_mean <- function(df, var_list) {
  result <- rowMeans(df[var_list], na.rm = TRUE)
  return(result)
}

#' Calculate Euclidean Distance of Row Values
#'
#' @description
#' Calculates the Euclidean distance (L2 norm) of values across specified columns for
#' each row. Computes the square root of the mean of squared values, with missing values
#' excluded from the calculation. Useful for measuring magnitude or distance in multivariate data.
#'
#' @param df (data.frame) The data frame containing the variables
#' @param var_list (character vector) Names of columns to include in the distance calculation
#'
#' @return (numeric vector) A numeric vector containing the Euclidean distance for each row
#'
#' @export
af_dist <- function(df, var_list) {
  result <- sqrt(rowMeans(df[var_list]^2, na.rm = TRUE))
  return(result)
}

#' Generate Significance Asterisks
#'
#' @description
#' Converts p-values into standard significance notation using asterisks. Returns three
#' asterisks for p < 0.001, two for p < 0.01, one for p < 0.05, and a space otherwise.
#' Commonly used for annotating statistical results in tables and figures.
#'
#' @param p_value (numeric) P-value(s) to convert to asterisk notation
#'
#' @return (character) Character string or vector with asterisk notation for significance levels
#'
#' @export
af_asterisks <- function(p_value) {
  ifelse(
    p_value < 0.001,
    "***",
    ifelse(p_value < 0.01, "**", ifelse(p_value < 0.05, "*", " "))
  )
}

#' Check if Variable Has Single Unique Value
#'
#' @description
#' Checks whether all values in a specified data frame column are identical. Returns TRUE
#' if the variable has only one unique value, FALSE otherwise. Useful for identifying
#' constant variables that may need to be excluded from analyses.
#'
#' @param df (data.frame) The data frame containing the variable
#' @param var_name (character) Name of the column to check
#'
#' @return (logical) TRUE if all values are the same, FALSE otherwise
#'
#' @export
af_is_same <- function(df, var_name) {
  return(length(unique(df[[var_name]])) == 1)
}

#' Sort Vector in Ascending Order
#'
#' @description
#' Returns a vector sorted in ascending order. A simple wrapper around the order function
#' that sorts elements from smallest to largest. Works with numeric, character, and other
#' orderable data types.
#'
#' @param l (vector) A vector to be sorted
#'
#' @return (vector) The input vector sorted in ascending order
#'
#' @export
af_order <- function(l) {
  l[order(l)]
}

#' Sort Data Frame by Column
#'
#' @description
#' Reorders the rows of a data frame based on the values in a specified column. Sorts in
#' ascending order by default. Returns the entire data frame with rows rearranged according
#' to the sort order of the specified variable.
#'
#' @param df (data.frame) The data frame to sort
#' @param var_name (character) Name of the column to use for sorting
#'
#' @return (data.frame) The data frame with rows sorted by the specified column
#'
#' @export
af_order_df <- function(df, var_name) {
  df[order(df[[var_name]]), ]
}

#' Transpose Data Frame
#'
#' @description
#' Transposes a data frame by converting the first column into row names and pivoting
#' remaining columns into rows. Uses tidyr functions to reshape the data, making columns
#' into rows and vice versa. The first column becomes the new column headers.
#'
#' @param df (data.frame) The data frame to transpose, where the first column will become column names
#'
#' @return (data.frame) A transposed data frame with original columns as rows and first column values as new column names
#'
#' @import tidyr
#'
#' @export
af_transpose_df <- function(df) {
  df %>%
    pivot_longer(
      cols = -1, # all columns except the first
      names_to = "Variable",
      values_to = "Value"
    ) %>%
    pivot_wider(
      names_from = 1, # first column
      values_from = "Value"
    )
}

#' Apply Value Labels to Variable
#'
#' @description
#' Replaces numeric or coded values in a data frame column with corresponding text labels
#' from a named vector. Values not found in the labels vector remain unchanged. Useful for
#' converting coded survey responses to readable labels while preserving unmapped values.
#'
#' @param df (data.frame) The data frame containing the variable to recode
#' @param var_name (character) Name of the column to apply labels to
#' @param var_labels (named vector) Named vector where values are codes and names are labels
#'
#' @return (data.frame) The data frame with labels applied to the specified column
#'
#' @export
af_apply_labels <- function(df, var_name, var_labels) {
  # Check if the specified column exists in the dataframe
  if (!(var_name %in% colnames(df))) {
    stop("The specified column does not exist in the dataframe.")
  }

  # Replace values in the specified column using the named vector
  df[[var_name]] <- ifelse(
    df[[var_name]] %in% var_labels,
    names(var_labels)[match(df[[var_name]], var_labels)],
    df[[var_name]]
  )

  # Return the updated dataframe
  return(df)
}

#' Determine Column Type
#'
#' @description
#' Identifies the data type of a data frame column by testing against multiple type
#' categories. Returns a character string indicating whether the column is numeric, factor
#' (binary), ordinal, nominal, survival object, or unknown. Used for selecting appropriate
#' statistical methods based on variable type.
#'
#' @param df (data.frame) The data frame containing the column
#' @param col_name (character) Name of the column to test
#'
#' @return (character) A string indicating the column type: "numeric", "factor", "ordinal", "nominal", "surv", or "unknown"
#'
#' @export
af_col_type <- function(df, col_name) {
  if (af_is_col_numeric(df, col_name)) {
    return("numeric")
  } else if (af_is_col_factor(df, col_name)) {
    return("factor")
  } else if (af_is_col_ordinal(df, col_name)) {
    return("ordinal")
  } else if (af_is_col_nominal(df, col_name)) {
    return("nominal")
  } else if (af_is_col_surv(df, col_name)) {
    return("surv")
  } else {
    return("unknown")
  }
}

#' Check if Vector is Numeric
#'
#' @description
#' Tests whether a vector is numeric. A simple wrapper around is.numeric for consistency
#' with other type-checking functions in this package.
#'
#' @param v (vector) The vector to test
#'
#' @return (logical) TRUE if the vector is numeric, FALSE otherwise
#'
#' @export
af_is_numeric <- function(v) is.numeric(v)

#' Check if Vector is Binary Factor
#'
#' @description
#' Tests whether a vector is a factor with exactly two levels. Returns TRUE only if the
#' input is both a factor and has precisely two distinct levels, making it suitable for
#' binary analyses.
#'
#' @param v (vector) The vector to test
#'
#' @return (logical) TRUE if the vector is a factor with two levels, FALSE otherwise
#'
#' @export
af_is_factor <- function(v) is.factor(v) & length(levels(v)) == 2

#' Check if Vector is Ordered Factor with Multiple Levels
#'
#' @description
#' Tests whether a vector is an ordered factor with more than two levels. Returns TRUE
#' only if the input is both ordered and has at least three levels, indicating an ordinal
#' variable suitable for ordinal regression or similar analyses.
#'
#' @param v (vector) The vector to test
#'
#' @return (logical) TRUE if the vector is an ordered factor with more than two levels, FALSE otherwise
#'
#' @export
af_is_ordinal <- function(v) {
  is.factor(v) & length(levels(v)) > 2 & is.ordered(v)
}

#' Check if Vector is Unordered Factor with Multiple Levels
#'
#' @description
#' Tests whether a vector is an unordered (nominal) factor with more than two levels.
#' Returns TRUE only if the input is a factor without ordering and has at least three
#' levels, indicating a nominal categorical variable.
#'
#' @param v (vector) The vector to test
#'
#' @return (logical) TRUE if the vector is an unordered factor with more than two levels, FALSE otherwise
#'
#' @export
af_is_nominal <- function(v) {
  is.factor(v) & length(levels(v)) > 2 & !is.ordered(v)
}

#' Check if Vector Contains Valid Year-Month-Day Dates
#'
#' @description
#' Tests whether all values in a vector can be successfully parsed as dates in YYYY-MM-DD
#' format. Returns TRUE only if all non-missing values match the expected date format.
#' Useful for validating date columns before conversion.
#'
#' @param v (vector) The vector to test for date format
#'
#' @return (logical) TRUE if all non-NA values are valid YYYY-MM-DD dates, FALSE otherwise
#'
#' @export
af_is_ymd <- function(v) all(!is.na(strptime(na.omit(v), "%Y-%m-%d")))

#' Check if Data Frame Column is Numeric
#'
#' @description
#' Tests whether a specified column in a data frame is numeric. Ensures the column is
#' accessed as a data frame and applies the numeric type check. Used for validating
#' variable types before numerical operations.
#'
#' @param df (data.frame) The data frame containing the column
#' @param col_name (character) Name of the column to test
#'
#' @return (logical) TRUE if the column is numeric, FALSE otherwise
#'
#' @export
af_is_col_numeric <- function(df, col_name) {
  af_is_numeric(as.data.frame(df)[, col_name])
}

#' Check if Data Frame Column is Binary Factor
#'
#' @description
#' Tests whether a specified column in a data frame is a factor with exactly two levels.
#' Useful for identifying binary categorical variables appropriate for logistic regression
#' or similar binary analyses.
#'
#' @param df (data.frame) The data frame containing the column
#' @param col_name (character) Name of the column to test
#'
#' @return (logical) TRUE if the column is a factor with two levels, FALSE otherwise
#'
#' @export
af_is_col_factor <- function(df, col_name) {
  af_is_factor(as.data.frame(df)[, col_name])
}

#' Check if Data Frame Column is Ordered Factor with Multiple Levels
#'
#' @description
#' Tests whether a specified column in a data frame is an ordered factor with more than
#' two levels. Identifies ordinal categorical variables suitable for ordinal regression
#' or other ordered categorical analyses.
#'
#' @param df (data.frame) The data frame containing the column
#' @param col_name (character) Name of the column to test
#'
#' @return (logical) TRUE if the column is an ordered factor with more than two levels, FALSE otherwise
#'
#' @export
af_is_col_ordinal <- function(df, col_name) {
  af_is_ordinal(as.data.frame(df)[, col_name])
}

#' Check if Data Frame Column is Unordered Factor with Multiple Levels
#'
#' @description
#' Tests whether a specified column in a data frame is an unordered (nominal) factor with
#' more than two levels. Identifies nominal categorical variables for appropriate statistical
#' treatment in analyses.
#'
#' @param df (data.frame) The data frame containing the column
#' @param col_name (character) Name of the column to test
#'
#' @return (logical) TRUE if the column is an unordered factor with more than two levels, FALSE otherwise
#'
#' @export
af_is_col_nominal <- function(df, col_name) {
  af_is_nominal(as.data.frame(df)[, col_name])
}

#' Check if Data Frame Column is Ordered Factor
#'
#' @description
#' Tests whether a specified column in a data frame is an ordered factor, regardless of
#' the number of levels. Returns TRUE for any ordered factor variable, useful for determining
#' if ordinal methods should be applied.
#'
#' @param df (data.frame) The data frame containing the column
#' @param col_name (character) Name of the column to test
#'
#' @return (logical) TRUE if the column is an ordered factor, FALSE otherwise
#'
#' @export
af_is_col_ordered <- function(df, col_name) {
  is.ordered(as.data.frame(df)[, col_name])
}

#' Check if Data Frame Column is Survival Object
#'
#' @description
#' Tests whether a specified column in a data frame is a Surv object from the survival
#' package. Used to identify survival time variables appropriate for survival analysis
#' methods like Cox regression or Kaplan-Meier estimation.
#'
#' @param df (data.frame) The data frame containing the column
#' @param col_name (character) Name of the column to test
#'
#' @return (logical) TRUE if the column is a Surv object, FALSE otherwise
#'
#' @export
af_is_col_surv <- function(df, col_name) is.Surv(as.data.frame(df)[, col_name])

#' Check if Data Frame Column is Date Object
#'
#' @description
#' Tests whether a specified column in a data frame is a Date object. Used to verify that
#' date columns are properly formatted as R Date objects before performing date arithmetic
#' or time-based analyses.
#'
#' @param df (data.frame) The data frame containing the column
#' @param col_name (character) Name of the column to test
#'
#' @return (logical) TRUE if the column is a Date object, FALSE otherwise
#'
#' @export
af_is_col_date <- function(df, col_name) is.Date(as.data.frame(df)[, col_name])

#' Check if Data Frame Column Contains Valid Year-Month-Day Dates
#'
#' @description
#' Tests whether all values in a specified data frame column can be successfully parsed
#' as dates in YYYY-MM-DD format. Returns TRUE only if all non-missing values match the
#' expected date format. Useful for validating date columns before conversion to Date objects.
#'
#' @param df (data.frame) The data frame containing the column
#' @param col_name (character) Name of the column to test
#'
#' @return (logical) TRUE if all non-NA values in the column are valid YYYY-MM-DD dates, FALSE otherwise
#'
#' @export
af_is_col_ymd <- function(df, col_name) af_is_ymd(as.data.frame(df)[, col_name])

#' Get First Element of Vector
#'
#' @description
#' Returns the first element of a vector. A convenience function that extracts the initial
#' value, useful for obtaining reference levels, minimum values, or starting points in
#' ordered sequences.
#'
#' @param v (vector) A vector from which to extract the first element
#'
#' @return The first element of the vector, maintaining its original type
#'
#' @export
af_first <- function(v) {
  return(v[1])
}

#' Get Last Element of Vector
#'
#' @description
#' Returns the last element of a vector. A convenience function that extracts the final
#' value, useful for obtaining the most recent observation, maximum level, or endpoint
#' in ordered sequences.
#'
#' @param v (vector) A vector from which to extract the last element
#'
#' @return The last element of the vector, maintaining its original type
#'
#' @export
af_last <- function(v) {
  return(v[length(v)])
}

#' Conditional Value Selection
#'
#' @description
#' Returns one of two values based on a logical condition. A simple if-else wrapper that
#' returns the first value if the condition is TRUE, otherwise returns the second value.
#' Useful for inline conditional assignments.
#'
#' @param cond (logical) The condition to evaluate
#' @param a The value to return if condition is TRUE
#' @param b The value to return if condition is FALSE
#'
#' @return Either a or b depending on the condition
#'
#' @export
af_select_val <- function(cond, a, b) if (cond) return(a) else return(b)

#' Save GT Table to File
#'
#' @description
#' Saves a gt table object to a file, removing any existing file with the same name first.
#' Ensures clean file creation by deleting previous versions before saving. Commonly used
#' to export formatted tables to Word documents or other supported formats.
#'
#' @param table (gt) A gt table object to save
#' @param output_file (character) File path where the table should be saved
#'
#' @return NULL (called for side effect of saving file)
#'
#' @import gt
#'
#' @export
af_gtsave <- function(table, output_file) {
  # Remove the file if it exists
  if (file.exists(output_file)) {
    file.remove(output_file)
  }

  # Save the table to a Word document
  gt::gtsave(table, output_file)
}

#' Format String List with Commas and 'And'
#'
#' @description
#' Converts a character vector into a grammatically correct list string. For single items,
#' returns the item. For two items, joins with "and". For three or more items, uses commas
#' between all but the last two, which are joined with "and". Creates natural language lists.
#'
#' @param strings (character vector) Vector of strings to format into a list
#'
#' @return (character) A single formatted string with proper comma and 'and' placement
#'
#' @export
af_format_string_list <- function(strings) {
  n <- length(strings)
  if (n == 1) {
    return(strings)
  }
  if (n == 2) {
    return(paste(strings[1], "and", strings[2]))
  }
  return(paste(paste(strings[1:(n - 1)], collapse = ", "), "and", strings[n]))
}

#' Convert Labeled Variables in SPSS/Stata Data to Factors
#'
#' @description
#' Converts labeled numeric variables from SPSS (.sav) or Stata (.dta) files into R factors.
#' Automatically identifies variables with value labels, creates factors using those labels,
#' and preserves original label attributes. Handles properly formatted label attributes while
#' filtering out malformed or pattern-based labels.
#'
#' @param df (data.frame) A data frame imported from SPSS or Stata containing labeled variables
#'
#' @return (data.frame) The data frame with labeled variables converted to factors while preserving label and labels attributes
#'
#' @export
af_make_sav_dta_factors <- function(df) {
  # Function to check first element name in labels attribute
  check_first_element_name <- function(col_name) {
    # Get the attribute for the specific column
    col_attr <- attr(df[[col_name]], "labels")
    # Check if labels attribute exists and has non-empty elements
    if (!is.null(col_attr) && length(col_attr) > 0) {
      # Extract the first element from the labels attribute
      name1 <- names(col_attr[1])
      # Check if it's a named list and name follows the pattern
      return(!(grepl("^\\d\\s*=.*$", name1) || grepl("\n\\d$", name1)))
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
      factor(
        col,
        levels = attr(col, "labels"),
        labels = names(attr(col, "labels"))
      )
    attr(df[[col_name]], "label") <- col_attr$label
    attr(df[[col_name]], "labels") <- col_attr$labels
  }
  return(df)
}

#' Format Numeric Vector as Comma-Separated List with 'And'
#'
#' @description
#' Converts a numeric vector into a grammatically correct string by joining elements with
#' commas, except the last element which is joined with "and". For example, c(1,2,3,4)
#' becomes "1,2,3 and 4". Useful for creating readable lists of model numbers or indices.
#'
#' @param items (numeric vector) Vector of numbers to format into a list string
#'
#' @return (character) A single formatted string with comma-separated values and 'and' before the last item
#'
#' @export
af_comma_and_list <- function(items) {
  if (length(items) > 1) {
    items_string <- paste(
      paste(items[-length(items)], collapse = ","),
      "and",
      items[length(items)]
    )
  } else {
    items_string <- paste(items)
  }

  return(items_string)
}
