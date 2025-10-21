#' Comprehensive Descriptive Statistics
#'
#' @description
#' Generates comprehensive descriptive statistics tables for variables in a data frame.
#' Creates separate summary tables for numeric variables (mean, SD, median, IQR, min, max),
#' binary factor variables (level counts and percentages), and categorical variables
#' (type, categories, mode). Returns formatted gt tables for each variable type.
#'
#' @param df (data.frame) A data frame to analyze and provide descriptive information for
#' @param var_list (character vector) Optional list of variable names to analyze. Default is NULL, which analyzes all variables in the data frame
#'
#' @return (list) A named list with three elements: numeric, binary, and categorical, each containing a gt table object or NULL if no variables of that type exist
#'
#' @import dplyr
#' @import gt
#' @importFrom stats median sd quantile IQR
#'
#' @examples
#' results <- af_descriptive(mtcars)
#' results <- af_descriptive(mtcars, c("mpg", "cyl", "hp"))
#'
#' @export
af_descriptive <- function(df, var_list = NULL) {
  # Validate inputs
  if (!is.data.frame(df)) {
    stop("Input must be a dataframe")
  }

  # If var_list is not provided, use all columns
  if (is.null(var_list)) {
    var_list <- names(df)
  } else {
    # Validate var_list
    if (!all(var_list %in% names(df))) {
      stop("Some specified variables not found in the dataframe")
    }
  }

  # Subset dataframe to selected variables
  df <- df[, var_list, drop = FALSE]

  # Helper function to identify missing values
  is_missing <- function(x) {
    is.null(x) | is.na(x) | x == "" | x == chr(0)
  }

  # Numeric Variables Table
  numeric_vars <- names(df)[sapply(df, function(x) is.numeric(x))]

  numeric_table <- NULL
  if (length(numeric_vars) > 0) {
    numeric_summary <- data.frame(
      Variable = numeric_vars,
      Mean = sapply(df[numeric_vars], function(x) mean(x, na.rm = TRUE)),
      SD = sapply(df[numeric_vars], function(x) sd(x, na.rm = TRUE)),
      Median = sapply(df[numeric_vars], function(x) median(x, na.rm = TRUE)),
      IQR = sapply(df[numeric_vars], function(x) IQR(x, na.rm = TRUE)),
      Min = sapply(df[numeric_vars], function(x) min(x, na.rm = TRUE)),
      Max = sapply(df[numeric_vars], function(x) max(x, na.rm = TRUE)),
      N_Missing = sapply(df[numeric_vars], function(x) sum(is.na(x))),
      N = sapply(df[numeric_vars], function(x) sum(!is.na(x)))
    )

    numeric_table <- numeric_summary %>%
      gt() %>%
      fmt_number(
        columns = c(Mean, SD, Median, IQR, Min, Max),
        decimals = 2
      ) %>%
      tab_header(title = "Numeric Variables Summary")
  }

  # Binary Variables Table
  binary_vars <- names(df)[sapply(df, function(x) {
    is.factor(x) && nlevels(x) == 2
  })]

  binary_table <- NULL
  if (length(binary_vars) > 0) {
    binary_summary <- data.frame(
      Variable = binary_vars,
      First_Level = sapply(df[binary_vars], function(x) levels(x)[1]),
      Percent_First_Level = sapply(df[binary_vars], function(x) {
        mean(x == levels(x)[1], na.rm = TRUE) * 100
      }),
      Second_Level = sapply(df[binary_vars], function(x) levels(x)[2]),
      Percent_Second_Level = sapply(df[binary_vars], function(x) {
        mean(x == levels(x)[2], na.rm = TRUE) * 100
      }),
      N_Missing = sapply(df[binary_vars], function(x) sum(is.na(x))),
      N = sapply(df[binary_vars], function(x) sum(!is.na(x)))
    )

    binary_table <- binary_summary %>%
      gt() %>%
      fmt_number(
        columns = c(Percent_First_Level, Percent_Second_Level),
        decimals = 0
      ) %>%
      cols_label(
        Percent_First_Level = "%",
        Percent_Second_Level = "%"
      ) %>%
      tab_header(title = "Binary Variables Summary")
  }

  # Categorical Variables Table
  categorical_vars <- names(df)[sapply(df, function(x) {
    is.factor(x) &&
      nlevels(x) > 2 ||
      is.character(x) ||
      (is.ordered(x) && nlevels(x) > 2)
  })]

  categorical_table <- NULL
  if (length(categorical_vars) > 0) {
    categorical_summary <- data.frame(
      Variable = categorical_vars,
      Type = sapply(df[categorical_vars], function(x) {
        if (is.ordered(x)) "ordinal" else "nominal"
      }),
      N_Categories = sapply(df[categorical_vars], function(x) {
        length(unique(x))
      }),
      Categories = sapply(df[categorical_vars], function(x) {
        if (length(unique(x)) <= 7) {
          paste(unique(x), collapse = ", ")
        } else {
          "More than 7 categories"
        }
      }),
      Min = sapply(df[categorical_vars], function(x) {
        if (is.ordered(x)) levels(x)[1] else NA
      }),
      Max = sapply(df[categorical_vars], function(x) {
        if (is.ordered(x)) levels(x)[length(levels(x))] else NA
      }),
      Mode = sapply(df[categorical_vars], function(x) {
        names(table(x)[which.max(table(x))])
      }),
      N_Missing = sapply(df[categorical_vars], function(x) sum(is.na(x))),
      N = sapply(df[categorical_vars], function(x) sum(!is.na(x)))
    )

    categorical_table <- categorical_summary %>%
      gt() %>%
      tab_header(title = "Categorical Variables Summary")
  }

  # Return the tables
  return(list(
    numeric = numeric_table,
    binary = binary_table,
    categorical = categorical_table
  ))
}

#' Create Data Dictionary
#'
#' @description
#' Generates a data dictionary for a given data frame, summarizing variable names,
#' labels, types, and ranges or levels. The resulting information is formatted
#' as a gt table for easy viewing.
#'
#' @param df (data.frame) A data frame to create a data dictionary for
#'
#' @return (gt table) A gt table summarizing the data dictionary information
#'
#' @import dplyr
#' @import gt
#'
#' @examples
#' data_dict <- af_create_data_dictionary(mtcars)
#' data_dict
#'
#' @export
af_create_data_dictionary <- function(df) {
  info <- data.frame(
    Name = character(),
    Label = character(),
    Type = character(),
    Range = character(),
    stringsAsFactors = FALSE
  )

  for (col_name in names(df)) {
    # Get the column
    col <- df[[col_name]]

    # Get the label attribute
    if ("label" %in% names(attributes(col))) {
      col_label <- attr(col, "label")
    } else {
      col_label <- col_name
    }

    if (is.factor(col)) {
      col_type <- "Factor"
      if (length(levels(col)) <= 15) {
        col_range <- paste(levels(col), collapse = ", ")
      } else {
        col_range <- "..."
      }
    } else if (is.numeric(col)) {
      col_type <- "Numeric"
      if ("labels" %in% names(attributes(col))) {
        col_labels <- attr(col, "labels")
        first <- col_labels[1]
        last <- col_labels[length(col_labels)]
        col_range <- paste("[", names(first), "-", names(last), "]")
      } else {
        col_range <- NA
      }
    } else if (is.character(col)) {
      col_type <- "Character"
      col_range <- NA
    } else if (inherits(col, "Date")) {
      col_type <- "Date"
      col_range <- NA
    } else if ("POSIXct" %in% class(col)) {
      col_type = "POSIX datetime"
      col_range <- NA
    } else {
      col_type <- "Unknown"
      col_range <- NA
    }

    info <- rbind(
      info,
      data.frame(
        Name = col_name,
        Label = col_label,
        Type = col_type,
        Levels = col_range
      )
    )
  }

  info_tbl <- info %>%
    gt() %>%
    opt_all_caps() %>%
    opt_row_striping() %>%
    tab_options(
      # table.width = px(800),
      heading.title.font.weight = "bold",
      heading.align = "left",
      row.striping.background_color = "lightgrey",
    )

  return(info_tbl)
}
