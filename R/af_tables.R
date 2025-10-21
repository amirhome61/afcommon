#' @title Create printable wide GT table
#' @description Generates a standalone HTML file with the full wide table
#' @param gt_tbl A gt table object
#' @param filename Output HTML filename
#' @param title Page title
#' @return Invisible NULL, creates HTML file as side effect
#' @export
af_print_wide_gt <- function(
  gt_tbl,
  filename = "wide_table.html",
  title = "Wide Table"
) {
  # Validate inputs
  if (!inherits(gt_tbl, "gt_tbl")) {
    stop("Input must be a gt table object")
  }

  if (!is.character(filename) || !grepl("\\.html$", filename)) {
    stop("Filename must be a character string ending with .html")
  }

  # Create HTML file with wide table
  html_content <- paste0(
    "<!DOCTYPE html>\n",
    "<html>\n",
    "<head>\n",
    "  <title>",
    title,
    "</title>\n",
    "  <style>\n",
    "    body { margin: 20px; }\n",
    "    .gt_table { width: 100% !important; }\n",
    "  </style>\n",
    "</head>\n",
    "<body>\n",
    gt::as_raw_html(gt_tbl),
    "\n</body>\n",
    "</html>"
  )

  # Write to file
  writeLines(html_content, filename)
  message("Wide table saved to: ", filename)

  invisible(NULL)
}

#' Transpose and Display Table with gt, Using Specified Column as Row Names
#'
#' This function transposes a data frame, optionally uses a specified column as row names,
#' and displays it using gt package.
#'
#' @param data A data frame or tibble to transpose and display
#' @param rowname_col Column name to use as row names in transposed table (optional)
#' @param title Optional title for the gt table
#' @param stub_col Name for the stub column containing original column names (default: "Variable")
#'
#' @return A gt table object
#' @export
af_transpose_gt <- function(
  data,
  rowname_col = NULL,
  title = NULL,
  stub_col = "Variable"
) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame or tibble")
  }

  if (nrow(data) == 0) {
    stop("Input data frame is empty")
  }

  if (!is.character(stub_col) || length(stub_col) != 1) {
    stop("'stub_col' must be a single character string")
  }

  if (!is.null(title) && (!is.character(title) || length(title) != 1)) {
    stop("'title' must be NULL or a single character string")
  }

  # Check if gt is available
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("Package 'gt' is required but not installed")
  }

  # Handle rowname column
  data_to_transpose <- data
  new_rownames <- NULL

  if (!is.null(rowname_col)) {
    if (!rowname_col %in% colnames(data)) {
      stop(paste("Column", rowname_col, "not found in data"))
    }
    new_rownames <- as.character(data[[rowname_col]])
    data_to_transpose <- data[, !colnames(data) %in% rowname_col, drop = FALSE]
  }

  # Transpose the data
  transposed <- as.data.frame(t(data_to_transpose))

  # Set new column names if specified
  if (!is.null(new_rownames)) {
    colnames(transposed) <- new_rownames
  }

  # Add original column names as the stub column
  transposed[[stub_col]] <- rownames(transposed)

  # Reorder columns to put the stub column first
  transposed <- transposed[, c(ncol(transposed), 1:(ncol(transposed) - 1))]

  # Create gt table
  gt_table <- gt::gt(transposed, rowname_col = stub_col)

  # Add title if provided
  if (!is.null(title)) {
    gt_table <- gt::tab_header(gt_table, title = title)
  }

  return(gt_table)
}

#' Create a horizontal gt table from x, y, and grouping variables
#'
#' This function takes a data frame with x, y, and grouping variables and creates
#' a gt table where the grouping variable forms the rows, the x variable forms
#' the columns, and the y variable provides the cell values.
#'
#' @param df A data frame containing the variables
#' @param x_var Character string. Name of the variable to use for columns
#' @param y_var Character string. Name of the variable to use for cell values
#' @param g_var Character string. Name of the grouping variable to use for rows
#' @param decimals Numeric. Number of decimal places to display. Default is 2
#' @param title Character string. Title for the table. Default is NULL (no title)
#'
#' @return A gt table object
#'
#' @import tidyr
#' @import gt
#' @import dplyr
#'
#' @examples
#' # Example usage:
#' # af_create_xy_table(plot_data, "Wave", "onp_c", "pe_left_center_right")
#' # af_create_xy_table(plot_data, "Wave", "onp_c", "pe_left_center_right", decimals = 3)
#' # af_create_xy_table(plot_data, "Wave", "onp_c", "pe_left_center_right", title = "Survey Results by Wave")
#'
#' @export
af_create_xy_table <- function(
  df,
  x_var,
  y_var,
  g_var,
  decimals = 2,
  title = NULL
) {
  # Input validation
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }

  if (!is.character(x_var) || length(x_var) != 1) {
    stop("x_var must be a single character string")
  }

  if (!is.character(y_var) || length(y_var) != 1) {
    stop("y_var must be a single character string")
  }

  if (!is.character(g_var) || length(g_var) != 1) {
    stop("g_var must be a single character string")
  }

  if (
    !is.numeric(decimals) ||
      length(decimals) != 1 ||
      decimals < 0 ||
      decimals != round(decimals)
  ) {
    stop("decimals must be a non-negative integer")
  }

  if (!is.null(title) && (!is.character(title) || length(title) != 1)) {
    stop("title must be a single character string or NULL")
  }

  # Check if variables exist in the data frame
  missing_vars <- setdiff(c(x_var, y_var, g_var), names(df))
  if (length(missing_vars) > 0) {
    stop(paste(
      "The following variables are not found in df:",
      paste(missing_vars, collapse = ", ")
    ))
  }

  # Check for missing values
  if (any(is.na(df[[x_var]]) | is.na(df[[y_var]]) | is.na(df[[g_var]]))) {
    warning("Missing values detected in one or more variables")
  }

  # Store factor levels order if x_var is a factor
  if (is.factor(df[[x_var]])) {
    x_levels <- levels(df[[x_var]])
  } else {
    x_levels <- NULL
  }

  # Select only the required columns and reshape to wide format
  wide_data <- df[, c(x_var, y_var, g_var)] %>%
    tidyr::pivot_wider(names_from = !!sym(x_var), values_from = !!sym(y_var))

  # Reorder columns if x_var was a factor
  if (!is.null(x_levels)) {
    # Get the grouping variable column name (first column)
    group_col <- names(wide_data)[1]

    # Get available columns that match factor levels
    available_levels <- x_levels[x_levels %in% names(wide_data)]

    # Reorder columns: grouping variable first, then factor levels in order
    wide_data <- wide_data[, c(group_col, available_levels)]
  }

  # Create the gt table with decimal formatting
  gt_table <- gt::gt(wide_data) %>%
    gt::fmt_number(decimals = decimals)

  # Add title if provided
  if (!is.null(title)) {
    gt_table <- gt_table %>%
      gt::tab_header(title = title)
  }

  return(gt_table)
}

#' Create a horizontal gt table from x variable and multiple y variables
#'
#' This function takes a data frame with one x variable and multiple y variables and creates
#' a gt table where each y variable forms a row, the x variable forms the columns,
#' and the y variable values provide the cell values. Optionally applies an aggregate
#' function to summarize multiple observations within each x-y combination.
#'
#' @param df A data frame containing the variables
#' @param x_var Character string. Name of the variable to use for columns
#' @param y_var_names Character vector. Names of the y variables to use for rows
#' @param decimals Numeric. Number of decimal places to display. Default is 2
#' @param agg_func Function or character string. Aggregate function to apply to each y variable
#'   for each level of x (e.g., mean, median, sum). If NULL (default), no aggregation is performed
#' @param na.rm Logical. Should missing values be removed when applying agg_func? Default is TRUE
#'
#' @return A gt table object
#'
#' @import tidyr
#' @import gt
#' @import dplyr
#'
#' @examples
#' # Basic usage without aggregation:
#' # af_create_x_multi_y_table(df, "Wave", c("onp_c", "trust_score", "satisfaction"))
#'
#' # With aggregation (e.g., median for survey data):
#' # af_create_x_multi_y_table(df, "Wave", c("onp_c", "trust_score", "satisfaction"),
#' #                          agg_func = median)
#'
#' # With custom decimal places:
#' # af_create_x_multi_y_table(df, "Wave", c("onp_c", "trust_score"),
#' #                          decimals = 3, agg_func = mean)
#'
#' @export
af_create_x_multi_y_table <- function(
  df,
  x_var,
  y_var_names,
  decimals = 2,
  agg_func = NULL,
  na.rm = TRUE
) {
  # Input validation
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }

  if (!is.character(x_var) || length(x_var) != 1) {
    stop("x_var must be a single character string")
  }

  if (!is.character(y_var_names) || length(y_var_names) < 1) {
    stop("y_var_names must be a character vector with at least one element")
  }

  if (
    !is.numeric(decimals) ||
      length(decimals) != 1 ||
      decimals < 0 ||
      decimals != round(decimals)
  ) {
    stop("decimals must be a non-negative integer")
  }

  if (!is.null(agg_func) && !is.function(agg_func) && !is.character(agg_func)) {
    stop(
      "agg_func must be NULL, a function, or a character string naming a function"
    )
  }

  if (!is.logical(na.rm) || length(na.rm) != 1) {
    stop("na.rm must be a single logical value")
  }

  # Check if variables exist in the data frame
  all_vars <- c(x_var, y_var_names)
  missing_vars <- setdiff(all_vars, names(df))
  if (length(missing_vars) > 0) {
    stop(paste(
      "The following variables are not found in df:",
      paste(missing_vars, collapse = ", ")
    ))
  }

  # Check for missing values
  if (any(is.na(df[, all_vars]))) {
    warning("Missing values detected in one or more variables")
  }

  # Store factor levels order if x_var is a factor
  if (is.factor(df[[x_var]])) {
    x_levels <- levels(df[[x_var]])
  } else {
    x_levels <- NULL
  }

  # Select required columns and reshape to long format first
  long_data <- df[, all_vars] %>%
    tidyr::pivot_longer(
      cols = all_of(y_var_names),
      names_to = "variable",
      values_to = "value"
    )

  # Apply aggregation function if provided
  if (!is.null(agg_func)) {
    # Convert character function name to function if needed
    if (is.character(agg_func)) {
      agg_func <- get(agg_func)
    }

    long_data <- long_data %>%
      dplyr::group_by(!!sym(x_var), variable) %>%
      dplyr::summarise(value = agg_func(value, na.rm = na.rm), .groups = "drop")
  }

  # Then reshape to wide format with x_var as columns
  wide_data <- long_data %>%
    tidyr::pivot_wider(names_from = !!sym(x_var), values_from = value)

  # Reorder columns if x_var was a factor
  if (!is.null(x_levels)) {
    # Get available columns that match factor levels
    available_levels <- x_levels[x_levels %in% names(wide_data)]

    # Reorder columns: variable column first, then factor levels in order
    wide_data <- wide_data[, c("variable", available_levels)]
  }

  # Create the gt table with decimal formatting
  gt::gt(wide_data) %>%
    gt::fmt_number(decimals = decimals)
}

af_count_categorical_levels <- function(
  data,
  variable_name,
  title = NULL,
  wave = NULL,
  wave_var = "Wave"
) {
  # Filter by wave if specified
  if (!is.null(wave)) {
    data <- data[data[[wave_var]] == wave, ]
  }

  if (is.null(title)) {
    if (!is.null(wave)) {
      title <- paste("Count by", variable_name, "- Wave", wave)
    } else {
      title <- paste("Count by", variable_name)
    }
  }

  # Extract the variable and preserve factor levels if it's a factor
  var_data <- data[[variable_name]]

  if (is.factor(var_data)) {
    # For factors, use the defined levels order
    level_order <- levels(var_data)
    count_table <- data %>%
      count(!!sym(variable_name)) %>%
      mutate(
        !!sym(variable_name) := factor(
          !!sym(variable_name),
          levels = level_order
        )
      ) %>%
      arrange(!!sym(variable_name))
  } else {
    # For non-factors, use natural order
    count_table <- data %>%
      count(!!sym(variable_name)) %>%
      arrange(!!sym(variable_name))
  }

  gt_table <- count_table %>%
    gt() %>%
    tab_header(title = title) %>%
    cols_label(
      !!sym(variable_name) := tools::toTitleCase(gsub("_", " ", variable_name)),
      n = "Count"
    ) %>%
    fmt_number(columns = n, decimals = 0)

  return(gt_table)
}
