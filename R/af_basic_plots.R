#' Create a histogram plot with automatic handling of numeric and categorical data
#'
#' @description
#' Creates a ggplot2 histogram for numeric data or a bar chart for categorical data,
#' with automatic percentage labels and total population count displayed.
#'
#' @param df A data frame containing the variable to plot
#' @param y_name Character string specifying the name of the variable to plot
#' @param nbins Numeric value specifying number of bins for numeric histograms (default: 50)
#'
#' @return A ggplot2 object
#'
#'
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' # Numeric variable histogram
#' af_plot_histogram(mtcars, "mpg")
#'
#' # Categorical variable bar chart
#' af_plot_histogram(mtcars, "cyl", nbins = 30)
#' }
#'
#' @export
af_plot_histogram <- function(df, y_name, nbins = 50) {
  total_population <- nrow(df) # Calculate total population size

  if (is.numeric(df[[y_name]])) {
    ggplot(df, aes(x = .data[[y_name]])) +
      geom_histogram(
        alpha = 0.5,
        position = "identity",
        color = "black",
        bins = nbins
      ) +
      annotate(
        "text",
        x = Inf,
        y = Inf,
        label = paste("Total N =", total_population),
        hjust = 1.1,
        vjust = 1.5,
        size = 4
      ) +
      labs(x = y_name, y = "Count")
  } else {
    # Calculate percentages
    df_summary <- df %>%
      group_by(.data[[y_name]]) %>%
      summarise(count = n()) %>%
      mutate(percentage = count / sum(count) * 100)

    ggplot(df_summary, aes(x = .data[[y_name]], y = count)) +
      geom_bar(stat = "identity", alpha = 0.5, position = "identity") +
      geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
      annotate(
        "text",
        x = Inf,
        y = Inf,
        label = paste("Total N =", total_population),
        hjust = 1.1,
        vjust = 1.5,
        size = 4
      ) +
      labs(x = y_name, y = "Count")
  }
}

#' Create Customizable XY Line Plot
#'
#' Creates a line plot with optional grouping, faceting, trend lines, and aggregation.
#' If no x variable is provided, an index (1:n) is used automatically.
#'
#' @param data A data frame containing the variables to plot
#' @param x_var Character string specifying the x-axis variable name. If NULL, an index will be created. Default is NULL
#' @param y_var Character string specifying the y-axis variable name (required)
#' @param grouping_variable Character string specifying the primary grouping variable name. Default is "None"
#' @param subgrouping_variable Character string specifying the secondary grouping variable name. Default is "None"
#' @param use_facet Logical indicating whether to create faceted plots. Default is FALSE
#' @param facet_cols Integer specifying the number of columns for faceted plots. Default is 2
#' @param trend_line Character string specifying trend line type. Options: NULL (no trend, shows lines), "smooth" (loess/linear adaptive), or "linear". Default is NULL
#' @param show_points Logical indicating whether to display data points. Default is FALSE
#' @param flip_axes Logical indicating whether to flip x and y axes. Default is FALSE
#' @param agg_func Character string specifying aggregation function. Options: "None", "mean", "median", "min", "max", "sum", "count". Default is "None"
#' @param title Character string for plot title. Default is NULL
#' @param subtitle Character string for plot subtitle. Default is NULL
#' @param x_label Character string for x-axis label. If NULL, uses variable name or "Index". Default is NULL
#' @param y_label Character string for y-axis label. If NULL, uses variable name. Default is NULL
#' @param line_size Numeric value for line width. Default is 1
#' @param point_size Numeric value for point size. Default is 3
#' @param legend_position Character string specifying legend position. Options: "right", "bottom", "left", "top", "none". Default is "right"
#' @param legend_label Character string for custom legend title. If NULL, uses grouping variable name(s). Default is NULL
#'
#' @return A ggplot object
#'
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom rlang sym
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple line plot
#' af_create_xy_plot(mtcars, x_var = "wt", y_var = "mpg")
#'
#' # Grouped plot with smooth trend line
#' af_create_xy_plot(mtcars, x_var = "wt", y_var = "mpg",
#'                   grouping_variable = "cyl", trend_line = "smooth")
#'
#' # Faceted plot with points
#' af_create_xy_plot(mtcars, x_var = "wt", y_var = "mpg",
#'                   grouping_variable = "cyl", use_facet = TRUE, show_points = TRUE)
#' }
af_create_xy_plot <- function(
  data,
  x_var = NULL,
  y_var,
  grouping_variable = "None",
  subgrouping_variable = "None",
  use_facet = FALSE,
  facet_cols = 2,
  trend_line = NULL,
  show_points = FALSE,
  flip_axes = FALSE,
  agg_func = "None",
  title = NULL,
  subtitle = NULL,
  x_label = NULL,
  y_label = NULL,
  line_size = 1,
  point_size = 3,
  legend_position = "right",
  legend_label = NULL
) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame")
  }

  if (!is.null(trend_line) && !trend_line %in% c("smooth", "linear")) {
    stop("Parameter 'trend_line' must be NULL, 'smooth', or 'linear'")
  }

  if (!y_var %in% names(data)) {
    stop(paste("Variable", y_var, "not found in data"))
  }

  if (!is.null(x_var) && !x_var %in% names(data)) {
    stop(paste("Variable", x_var, "not found in data"))
  }

  if (
    grouping_variable != "None" &&
      !is.null(grouping_variable) &&
      !grouping_variable %in% names(data)
  ) {
    stop(paste("Grouping variable", grouping_variable, "not found in data"))
  }

  if (
    subgrouping_variable != "None" &&
      !is.null(subgrouping_variable) &&
      !subgrouping_variable %in% names(data)
  ) {
    stop(paste(
      "Subgrouping variable",
      subgrouping_variable,
      "not found in data"
    ))
  }

  if (!legend_position %in% c("right", "bottom", "left", "top", "none")) {
    stop(
      "Parameter 'legend_position' must be one of: 'right', 'bottom', 'left', 'top', 'none'"
    )
  }

  if (is.null(x_var)) {
    data$.index <- 1:nrow(data)
    x_var <- ".index"
  } else {
    data <- data %>% filter(!is.na(!!sym(x_var)))
  }
  data <- data %>% filter(!is.na(!!sym(y_var)))

  if ((grouping_variable != "None") && (!is.null(grouping_variable))) {
    data <- data %>%
      filter(!is.na(!!sym(grouping_variable))) %>%
      mutate(across(all_of(grouping_variable), as.factor))
  }
  if ((subgrouping_variable != "None") && (!is.null(subgrouping_variable))) {
    data <- data %>%
      filter(!is.na(!!sym(subgrouping_variable))) %>%
      mutate(across(all_of(subgrouping_variable), as.factor))
  }
  if (agg_func != "None") {
    group_vars <- c(x_var, grouping_variable, subgrouping_variable)
    group_vars <- group_vars[group_vars != "None"]
    agg_func_lower <- tolower(agg_func)
    agg_func_to_use <- switch(
      agg_func_lower,
      mean = mean,
      median = median,
      min = min,
      max = max,
      sum = sum,
      count = length
    )
    data <- data %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(y = agg_func_to_use(!!sym(y_var)), .groups = "drop")
    y_var <- "y"
  }
  p <- ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var)))

  # Apply minimal theme
  p <- p + theme_minimal()

  if (grouping_variable != "None") {
    if (subgrouping_variable != "None") {
      p <- p +
        aes(
          color = interaction(
            !!sym(grouping_variable),
            !!sym(subgrouping_variable),
            sep = " - "
          ),
          group = interaction(
            !!sym(grouping_variable),
            !!sym(subgrouping_variable),
            sep = " - "
          )
        )
      legend_title <- paste(grouping_variable, subgrouping_variable, sep = ", ")
    } else {
      p <- p +
        aes(color = !!sym(grouping_variable), group = !!sym(grouping_variable))
      legend_title <- grouping_variable
    }
  }
  if (show_points) {
    p <- p + geom_point(alpha = 0.6, size = point_size)
  }

  # Handle trend lines based on new trend_line parameter
  if (!is.null(trend_line)) {
    if (trend_line == "smooth") {
      unique_x_count <- length(unique(data[[x_var]]))
      if (unique_x_count >= 4) {
        if (unique_x_count < 10) {
          p <- p +
            geom_smooth(
              se = FALSE,
              method = "lm",
              formula = y ~ x,
              na.rm = TRUE,
              linewidth = line_size
            )
        } else {
          span <- min(0.75, max(0.25, 10 / unique_x_count))
          p <- p +
            geom_smooth(
              se = FALSE,
              method = "loess",
              formula = y ~ x,
              span = span,
              na.rm = TRUE,
              linewidth = line_size
            )
        }
      }
    } else if (trend_line == "linear") {
      p <- p +
        geom_smooth(
          se = FALSE,
          method = "lm",
          formula = y ~ x,
          na.rm = TRUE,
          linewidth = line_size
        )
    }
  } else {
    # Add regular lines when no trend_line is specified
    p <- p + geom_line(alpha = 0.8, linewidth = line_size)
  }

  if (use_facet) {
    if (subgrouping_variable != "None") {
      p <- p +
        facet_wrap(
          as.formula(paste("~", grouping_variable, "+", subgrouping_variable)),
          ncol = facet_cols,
          labeller = label_both
        )
    } else if (grouping_variable != "None") {
      p <- p +
        facet_wrap(
          as.formula(paste("~", grouping_variable)),
          ncol = facet_cols,
          labeller = label_both
        )
    }
  }

  # Set axis labels (with defaults)
  default_x_label <- if (x_var == ".index") "Index" else x_var
  default_y_label <- y_var

  # Use custom labels if provided, otherwise use defaults
  x_axis_label <- if (!is.null(x_label)) x_label else default_x_label
  y_axis_label <- if (!is.null(y_label)) y_label else default_y_label

  # Create labs with all optional elements
  plot_labs <- list(x = x_axis_label, y = y_axis_label)

  # Add title and subtitle if provided
  if (!is.null(title)) {
    plot_labs[["title"]] <- title
  }
  if (!is.null(subtitle)) {
    plot_labs[["subtitle"]] <- subtitle
  }

  # Add legend title if grouping is used
  if (grouping_variable != "None") {
    # Use custom legend_label if provided, otherwise use default
    final_legend_title <- if (!is.null(legend_label)) {
      legend_label
    } else {
      legend_title
    }
    plot_labs[["color"]] <- final_legend_title
  }

  # Apply all labels
  p <- p + do.call(labs, plot_labs)

  if (grouping_variable != "None") {
    p <- p + scale_color_viridis(discrete = TRUE, na.value = "grey50")
  } else {
    p <- p + scale_color_viridis(discrete = FALSE)
  }

  # Set legend position
  p <- p + theme(legend.position = legend_position)

  if (flip_axes) {
    p <- p + coord_flip()
  }

  p
}

#' Create Customizable XY Line Plot
#'
#' Creates a line plot with optional grouping, faceting, trend lines, and aggregation.
#' If no x variable is provided, an index (1:n) is used automatically.
#'
#' @param data A data frame containing the variables to plot
#' @param x_var Character string specifying the x-axis variable name. If NULL, an index will be created. Default is NULL
#' @param y_var Character string specifying the y-axis variable name (required)
#' @param grouping_variable Character string specifying the primary grouping variable name. Default is "None"
#' @param subgrouping_variable Character string specifying the secondary grouping variable name. Default is "None"
#' @param use_facet Logical indicating whether to create faceted plots. Default is FALSE
#' @param facet_cols Integer specifying the number of columns for faceted plots. Default is 2
#' @param trend_line Character string specifying trend line type. Options: NULL (no trend, shows lines), "smooth" (loess/linear adaptive), or "linear". Default is NULL
#' @param show_points Logical indicating whether to display data points. Default is FALSE
#' @param flip_axes Logical indicating whether to flip x and y axes. Default is FALSE
#' @param agg_func Character string specifying aggregation function. Options: "None", "mean", "median", "min", "max", "sum", "count". Default is "None"
#' @param title Character string for plot title. Default is NULL
#' @param subtitle Character string for plot subtitle. Default is NULL
#' @param x_label Character string for x-axis label. If NULL, uses variable name or "Index". Default is NULL
#' @param y_label Character string for y-axis label. If NULL, uses variable name. Default is NULL
#' @param line_size Numeric value for line width. Default is 1
#' @param point_size Numeric value for point size. Default is 3
#' @param legend_position Character string specifying legend position. Options: "right", "bottom", "left", "top", "none". Default is "right"
#' @param legend_label Character string for custom legend title. If NULL, uses grouping variable name(s). Default is NULL
#'
#' @return A ggplot object
#'
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom rlang sym
#' @importFrom tidyr pivot_longer
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple line plot
#' af_create_xy_plot(mtcars, x_var = "wt", y_var = "mpg")
#'
#' # Grouped plot with smooth trend line
#' af_create_xy_plot(mtcars, x_var = "wt", y_var = "mpg",
#'                   grouping_variable = "cyl", trend_line = "smooth")
#'
#' # Faceted plot with points
#' af_create_xy_plot(mtcars, x_var = "wt", y_var = "mpg",
#'                   grouping_variable = "cyl", use_facet = TRUE, show_points = TRUE)
#' }
af_create_x_multi_y_plot <- function(
  data,
  x_var = NULL,
  y_var_names,
  y_var_labels = NULL,
  use_facet = FALSE,
  facet_cols = 2,
  smooth = FALSE,
  show_points = FALSE,
  flip_axes = FALSE,
  agg_func = "None",
  title = NULL,
  subtitle = NULL,
  x_label = NULL,
  y_label = NULL,
  legend_title = NULL,
  line_size = 1,
  point_size = 3,
  legend_position = "right",
  viridis_option = "viridis"
) {
  # Validate input parameters
  if (!is.character(y_var_names)) {
    stop("y_var_names must be a character vector of column names")
  }

  # Validate y_var_labels if provided
  if (!is.null(y_var_labels)) {
    if (!is.character(y_var_labels)) {
      stop("y_var_labels must be a character vector")
    }
    if (length(y_var_labels) != length(y_var_names)) {
      stop("y_var_labels must have the same length as y_var_names")
    }
  }

  missing_vars <- y_var_names[!y_var_names %in% colnames(data)]
  if (length(missing_vars) > 0) {
    stop(paste(
      "The following y variables are not found in the data:",
      paste(missing_vars, collapse = ", ")
    ))
  }

  if (is.null(x_var)) {
    data$.index <- 1:nrow(data)
    x_var <- ".index"
  } else {
    if (!x_var %in% colnames(data)) {
      stop(paste("x variable", x_var, "not found in data"))
    }
    data <- data %>% filter(!is.na(!!sym(x_var)))
  }

  for (y_var in y_var_names) {
    data <- data %>% filter(!is.na(!!sym(y_var)))
  }

  if (agg_func != "None") {
    agg_func_lower <- tolower(agg_func)
    agg_func_to_use <- switch(
      agg_func_lower,
      mean = mean,
      median = median,
      min = min,
      max = max,
      sum = sum,
      count = length
    )
    agg_data <- data %>% group_by(!!sym(x_var))
    agg_data <- agg_data %>%
      summarise(across(all_of(y_var_names), agg_func_to_use), .groups = "drop")
    data <- agg_data
  }

  plot_data <- data %>%
    select(all_of(c(x_var, y_var_names))) %>%
    pivot_longer(
      cols = all_of(y_var_names),
      names_to = "variable",
      values_to = "value"
    )

  # Apply custom labels and maintain order
  if (!is.null(y_var_labels)) {
    # Create mapping from variable names to labels
    label_mapping <- setNames(y_var_labels, y_var_names)
    plot_data$variable_label <- label_mapping[plot_data$variable]
    plot_data$variable_label <- factor(
      plot_data$variable_label,
      levels = y_var_labels
    )
  } else {
    plot_data$variable_label <- factor(plot_data$variable, levels = y_var_names)
  }

  p <- ggplot(
    plot_data,
    aes(
      x = !!sym(x_var),
      y = value,
      color = variable_label,
      group = variable_label
    )
  )

  # Apply minimal theme
  p <- p + theme_minimal()

  if (show_points) {
    p <- p + geom_point(alpha = 0.6, size = point_size)
  }

  if (!smooth) {
    p <- p + geom_line(alpha = 0.8, linewidth = line_size)
  }

  if (smooth) {
    unique_x_count <- length(unique(data[[x_var]]))
    if (unique_x_count >= 4) {
      if (unique_x_count < 10) {
        p <- p +
          geom_smooth(
            se = FALSE,
            method = "lm",
            formula = y ~ x,
            na.rm = TRUE,
            linewidth = line_size
          )
      } else {
        span <- min(0.75, max(0.25, 10 / unique_x_count))
        p <- p +
          geom_smooth(
            se = FALSE,
            method = "loess",
            formula = y ~ x,
            span = span,
            na.rm = TRUE,
            linewidth = line_size
          )
      }
    }
  }

  if (use_facet) {
    p <- p +
      facet_wrap(~variable_label, ncol = facet_cols, labeller = label_value)
  }

  # Set axis labels (with defaults)
  default_x_label <- if (x_var == ".index") "Index" else x_var
  default_y_label <- "Value"

  # Use custom labels if provided, otherwise use defaults
  x_axis_label <- if (!is.null(x_label)) x_label else default_x_label
  y_axis_label <- if (!is.null(y_label)) y_label else default_y_label

  # Create labs with all optional elements
  plot_labs <- list(x = x_axis_label, y = y_axis_label, color = legend_title)

  # Add title and subtitle if provided
  if (!is.null(title)) {
    plot_labs[["title"]] <- title
  }
  if (!is.null(subtitle)) {
    plot_labs[["subtitle"]] <- subtitle
  }

  # Apply all labels
  p <- p + do.call(labs, plot_labs)

  # Apply viridis colors with specified option
  p <- p + scale_color_viridis_d(option = viridis_option, na.value = "grey50")

  # Set legend position
  p <- p + theme(legend.position = legend_position)

  if (flip_axes) {
    p <- p + coord_flip()
  }

  return(p)
}

#' Create Y Variable Plots with Flexible Grouping and Plot Types
#'
#' @description
#' This function creates various types of plots for a single y variable with optional
#' grouping and subgrouping. Supports histograms, bar plots, boxplots, violin plots,
#' and density plots with automatic count labels.
#'
#' @param data A data frame containing the variables to plot
#' @param y_var Character string specifying the name of the y variable to plot
#' @param group_var Character string specifying the name of the grouping variable (optional)
#' @param subgroup_var Character string specifying the name of the subgrouping variable (optional)
#' @param plot_types Character vector specifying plot types. Options: "bar", "histogram",
#'   "histogram dodged", "boxplot", "violin", "density". Default is c("bar")
#' @param bins Numeric value specifying number of bins for histograms. Default is 30
#' @param use_facet Logical indicating whether to use faceting. Default is FALSE
#' @param facet_cols Numeric value specifying number of columns for faceting. Default is 2
#' @param agg_func Aggregation function for numeric data when using bar plots (not currently implemented)
#' @param title Character string for plot title. Default is NULL (no title)
#' @param subtitle Character string for plot subtitle. Default is NULL (no subtitle)
#'
#' @return A ggplot2 object
#'
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom rlang sym
#'
#' @examples
#' \dontrun{
#' # Basic histogram
#' af_create_y_plot(mtcars, "mpg", plot_types = "histogram")
#'
#' # Bar plot with grouping
#' af_create_y_plot(mtcars, "cyl", group_var = "gear", plot_types = "bar")
#'
#' # Multiple plot types with title
#' af_create_y_plot(mtcars, "mpg", group_var = "cyl",
#'                  plot_types = c("histogram", "density"), title = "MPG Distribution")
#' }
#'
#'
#' @export
af_create_y_plot <- function(
  data,
  y_var,
  group_var = NULL,
  subgroup_var = NULL,
  plot_types = c("bar"),
  bins = 30,
  use_facet = FALSE,
  facet_cols = 2,
  agg_func = NULL,
  title = NULL,
  subtitle = NULL
) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  if (!is.character(y_var) || length(y_var) != 1) {
    stop("'y_var' must be a single character string")
  }

  if (!y_var %in% names(data)) {
    stop(paste("'y_var'", y_var, "not found in data"))
  }

  if (!is.null(group_var)) {
    if (!is.character(group_var) || length(group_var) != 1) {
      stop("'group_var' must be a single character string or NULL")
    }
    if (!group_var %in% names(data)) {
      stop(paste("'group_var'", group_var, "not found in data"))
    }
  }

  if (!is.null(subgroup_var)) {
    if (!is.character(subgroup_var) || length(subgroup_var) != 1) {
      stop("'subgroup_var' must be a single character string or NULL")
    }
    if (!subgroup_var %in% names(data)) {
      stop(paste("'subgroup_var'", subgroup_var, "not found in data"))
    }
  }

  valid_plot_types <- c(
    "bar",
    "histogram",
    "histogram dodged",
    "boxplot",
    "violin",
    "density"
  )
  if (!all(plot_types %in% valid_plot_types)) {
    stop(paste(
      "'plot_types' must be one or more of:",
      paste(valid_plot_types, collapse = ", ")
    ))
  }

  if (!is.numeric(bins) || length(bins) != 1 || bins <= 0) {
    stop("'bins' must be a positive numeric value")
  }

  if (!is.logical(use_facet) || length(use_facet) != 1) {
    stop("'use_facet' must be TRUE or FALSE")
  }

  if (!is.numeric(facet_cols) || length(facet_cols) != 1 || facet_cols <= 0) {
    stop("'facet_cols' must be a positive numeric value")
  }

  if (!is.null(title) && (!is.character(title) || length(title) != 1)) {
    stop("'title' must be a single character string or NULL")
  }

  if (
    !is.null(subtitle) && (!is.character(subtitle) || length(subtitle) != 1)
  ) {
    stop("'subtitle' must be a single character string or NULL")
  }

  # Convert grouping variables to factors and remove NA values
  if (!is.null(group_var)) {
    data <- data %>%
      filter(!is.na(!!sym(group_var))) %>%
      mutate(across(all_of(group_var), as.factor))
  }

  if (!is.null(subgroup_var)) {
    data <- data %>%
      filter(!is.na(!!sym(subgroup_var))) %>%
      mutate(across(all_of(subgroup_var), as.factor))
  }

  # Remove NA values from y_var
  data <- data %>% filter(!is.na(!!sym(y_var)))

  # Determine if y_var is numeric or categorical
  is_numeric_y <- is.numeric(data[[y_var]])

  # Base plot
  p <- ggplot(data)

  if (is_numeric_y) {
    # Handling numeric y_var
    if (!is.null(group_var) && is.null(subgroup_var)) {
      p <- p + aes_string(fill = group_var, group = group_var)
    } else if (!is.null(subgroup_var)) {
      if (!is.null(group_var)) {
        p <- p +
          aes_string(
            fill = paste0("interaction(", group_var, ", ", subgroup_var, ")"),
            group = paste0("interaction(", group_var, ", ", subgroup_var, ")")
          )
      } else {
        p <- p + aes_string(fill = subgroup_var, group = subgroup_var)
      }
    }

    # Add layers based on plot types
    for (type in plot_types) {
      switch(
        type,
        "histogram" = {
          if (is.null(group_var) && is.null(subgroup_var)) {
            p <- p +
              geom_histogram(
                aes_string(x = y_var),
                bins = bins,
                color = "black",
                alpha = 0.7
              ) +
              stat_bin(
                aes_string(x = y_var, label = "after_stat(count)"),
                bins = bins,
                geom = "text",
                vjust = -0.5
              )
          } else {
            p <- p +
              geom_histogram(
                aes_string(x = y_var),
                bins = bins,
                color = "black",
                alpha = 0.7,
                position = "identity"
              ) +
              stat_bin(
                aes_string(x = y_var, label = "after_stat(count)"),
                bins = bins,
                geom = "text",
                vjust = -0.5,
                position = "identity"
              )
          }
        },
        "histogram dodged" = {
          p <- p +
            geom_histogram(
              aes_string(x = y_var),
              bins = bins,
              color = "black",
              alpha = 0.7,
              position = position_dodge()
            ) +
            stat_bin(
              aes_string(x = y_var, label = "after_stat(count)"),
              bins = bins,
              geom = "text",
              vjust = -0.5,
              position = position_dodge(width = 0.9)
            )
        },
        "boxplot" = {
          p <- p +
            geom_boxplot(
              aes_string(
                x = ifelse(is.null(group_var), "''", group_var),
                y = y_var
              ),
              color = "black",
              alpha = 0.7
            )
        },
        "violin" = {
          p <- p +
            geom_violin(
              aes_string(
                x = ifelse(is.null(group_var), "''", group_var),
                y = y_var
              ),
              color = "black",
              alpha = 0.7
            )
        },
        "density" = {
          if (is.null(group_var) && is.null(subgroup_var)) {
            total_n <- nrow(data)
            p <- p +
              geom_density(
                aes_string(x = y_var),
                color = "black",
                alpha = 0.7
              ) +
              annotate(
                "text",
                x = Inf,
                y = Inf,
                label = paste("N =", total_n),
                hjust = 1.1,
                vjust = 1.1,
                size = 3
              )
          } else {
            p <- p +
              geom_density(aes_string(x = y_var), color = "black", alpha = 0.7)
            # For grouped density, add N in facets or legend
            if (use_facet) {
              group_counts <- data %>%
                group_by(across(all_of(c(group_var, subgroup_var)[
                  !is.null(c(group_var, subgroup_var))
                ]))) %>%
                summarise(n = n(), .groups = "drop")
              # Note: Adding group-specific N labels to density plots with grouping is complex
              # and may require custom positioning. This is a basic implementation.
            }
          }
        },
        "bar" = {
          if (!is.null(group_var) || !is.null(subgroup_var)) {
            # This case needs aggregated data for numeric y_var
            warning(
              "Bar plot for numeric y_var requires aggregation. Consider using agg_func parameter."
            )
          }
        }
      )
    }
  } else {
    # Handling categorical y_var
    p <- ggplot(data) + aes_string(x = y_var)

    if (!is.null(group_var)) {
      p <- p + aes_string(fill = group_var, group = group_var)
    }
    if (!is.null(subgroup_var)) {
      if (!is.null(group_var)) {
        p <- p +
          aes_string(
            fill = paste0("interaction(", group_var, ", ", subgroup_var, ")"),
            group = paste0("interaction(", group_var, ", ", subgroup_var, ")")
          )
      } else {
        p <- p + aes_string(fill = subgroup_var, group = subgroup_var)
      }
    }

    # Add layers based on plot types
    for (type in plot_types) {
      if (type == "bar") {
        p <- p +
          geom_bar(position = position_dodge(), color = "black", alpha = 0.7) +
          stat_count(
            aes_string(label = "after_stat(count)"),
            geom = "text",
            vjust = -0.5,
            position = position_dodge(width = 0.9)
          )
      }
    }
  }

  # Add faceting if specified
  if (use_facet) {
    if (!is.null(group_var) && is.null(subgroup_var)) {
      p <- p + facet_wrap(vars(!!sym(group_var)), ncol = facet_cols)
    } else if (is.null(group_var) && !is.null(subgroup_var)) {
      p <- p + facet_wrap(vars(!!sym(subgroup_var)), ncol = facet_cols)
    } else if (!is.null(group_var) && !is.null(subgroup_var)) {
      p <- p +
        facet_wrap(
          vars(interaction(!!sym(group_var), !!sym(subgroup_var))),
          ncol = facet_cols
        )
    }
  }

  # Customize the theme and labels
  legend_title <- NULL
  if (!is.null(group_var) && !is.null(subgroup_var)) {
    legend_title <- paste(group_var, subgroup_var, sep = ".")
  } else if (!is.null(group_var)) {
    legend_title <- group_var
  } else if (!is.null(subgroup_var)) {
    legend_title <- subgroup_var
  }

  p <- p +
    labs(
      x = y_var,
      y = "frequency",
      fill = legend_title,
      title = title,
      subtitle = subtitle
    ) +
    theme_minimal() +
    theme(legend.position = "right") +
    scale_fill_viridis_d(na.value = "grey50")

  return(p)
}

#' Create an XY bar plot with extended styling options
#'
#' @description
#' Creates a ggplot2 bar plot with optional fill variable, faceting, and styling features
#'
#' @param data A data frame containing the variables to plot
#' @param x_var Name of the variable for the x-axis
#' @param y_var Name of the variable for the y-axis
#' @param fill_var Name of the variable for fill color (default: NULL for dark grey)
#' @param use_facet Logical indicating whether to create facets (default: FALSE)
#' @param facet_cols Number of columns for facets when use_facet=TRUE (default: 2)
#' @param flip_axes Logical indicating whether to flip the x and y axes (default: FALSE)
#' @param agg_func Aggregation function to use ("None", "mean", "median", "min", "max", "sum", "count")
#' @param position Type of bar positioning ("dodge", "stack", "fill") (default: "dodge")
#' @param bar_width Width of the bars, between 0 and 1 (default: 0.7)
#' @param title Optional plot title (default: NULL)
#' @param subtitle Optional plot subtitle (default: NULL)
#' @param x_label Optional custom x-axis label (default: NULL, uses variable name)
#' @param y_label Optional custom y-axis label (default: NULL, uses variable name)
#' @param add_labels Logical indicating whether to add value labels on bars (default: FALSE)
#' @param label_size Size of value labels if add_labels=TRUE (default: 3)
#' @param legend_position Position of the legend ("right", "bottom", "left", "top", or "none")
#'
#' @return A ggplot2 object
#'
#'
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom rlang := sym
#' @importFrom scales percent
#'
#' @examples
#' \dontrun{
#' # Basic bar plot with dark grey
#' af_create_xy_bar(mtcars, "cyl", "mpg", agg_func = "mean")
#'
#' # With fill variable and custom styling
#' af_create_xy_bar(mtcars, "cyl", "mpg",
#'                 fill_var = "am",
#'                 agg_func = "mean",
#'                 title = "Average MPG by Cylinder Count and Transmission",
#'                 x_label = "Number of Cylinders",
#'                 y_label = "Average Miles Per Gallon",
#'                 add_labels = TRUE,
#'                 legend_position = "bottom")
#' }
#'
#' @export
af_create_xy_bar <- function(
  data,
  x_var,
  y_var,
  fill_var = NULL,
  use_facet = FALSE,
  facet_cols = 2,
  flip_axes = FALSE,
  agg_func = "None",
  position = "dodge",
  bar_width = 0.7,
  title = NULL,
  subtitle = NULL,
  x_label = NULL,
  y_label = NULL,
  add_labels = FALSE,
  label_size = 3,
  legend_position = "right"
) {
  # Ensure x_var is not NULL
  if (is.null(x_var)) {
    stop("x_var must be specified for bar plots")
  }

  # Filter out NA values
  data <- data %>%
    filter(!is.na(!!sym(x_var))) %>%
    filter(!is.na(!!sym(y_var)))

  # Convert fill variable to factor if provided
  if (!is.null(fill_var)) {
    # Check if fill_var exists in the data
    if (!fill_var %in% names(data)) {
      stop(paste("fill_var '", fill_var, "' not found in data", sep = ""))
    }

    data <- data %>%
      filter(!is.na(!!sym(fill_var))) %>%
      mutate(!!sym(fill_var) := as.factor(!!sym(fill_var)))
  }

  # Ensure x variable is a factor for proper bar plotting
  data <- data %>% mutate(!!sym(x_var) := as.factor(!!sym(x_var)))

  # Apply aggregation if specified
  if (agg_func != "None") {
    group_vars <- c(x_var)
    # Only add fill_var to grouping if it's not NULL
    if (!is.null(fill_var)) {
      group_vars <- c(group_vars, fill_var)
    }

    agg_func_lower <- tolower(agg_func)
    agg_func_to_use <- switch(
      agg_func_lower,
      mean = mean,
      median = median,
      min = min,
      max = max,
      sum = sum,
      count = length
    )
    data <- data %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(y = agg_func_to_use(!!sym(y_var)), .groups = "drop")
    y_var <- "y"
  }

  # Start creating the plot
  p <- ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var)))

  # Apply minimal theme
  p <- p + theme_minimal()

  # Handle fill variable
  if (!is.null(fill_var)) {
    p <- p + aes(fill = !!sym(fill_var))
    legend_title <- fill_var

    # Add bars with specified position
    p <- p + geom_bar(stat = "identity", position = position, width = bar_width)
    # Apply color scale for fill variable
    p <- p + scale_fill_viridis_d(na.value = "grey50")
  } else {
    # Using fixed dark grey when fill_var is NULL
    p <- p +
      geom_bar(
        stat = "identity",
        position = position,
        width = bar_width,
        fill = "darkgrey"
      )
  }

  # Add value labels if requested
  if (add_labels) {
    if (position == "dodge") {
      p <- p +
        geom_text(
          aes(label = round(!!sym(y_var), 1)),
          position = position_dodge(width = bar_width),
          vjust = -0.5,
          size = label_size
        )
    } else if (position == "stack") {
      p <- p +
        geom_text(
          aes(label = round(!!sym(y_var), 1)),
          position = position_stack(vjust = 0.5),
          size = label_size
        )
    } else if (position == "fill") {
      p <- p +
        geom_text(
          aes(label = scales::percent(!!sym(y_var), accuracy = 0.1)),
          position = position_fill(vjust = 0.5),
          size = label_size
        )
    }
  }

  # Handle facets only if fill_var is provided
  if (use_facet && !is.null(fill_var)) {
    p <- p +
      facet_wrap(
        as.formula(paste("~", fill_var)),
        ncol = facet_cols,
        labeller = label_both
      )
  }

  # Set axis labels (with defaults)
  default_x_label <- x_var
  default_y_label <- if (agg_func != "None") {
    paste(agg_func_lower, "of", y_var)
  } else {
    y_var
  }

  # Use custom labels if provided, otherwise use defaults
  x_axis_label <- if (!is.null(x_label)) x_label else default_x_label
  y_axis_label <- if (!is.null(y_label)) y_label else default_y_label

  # Create labs with all optional elements
  plot_labs <- list(x = x_axis_label, y = y_axis_label)

  # Add title and subtitle if provided
  if (!is.null(title)) {
    plot_labs[["title"]] <- title
  }
  if (!is.null(subtitle)) {
    plot_labs[["subtitle"]] <- subtitle
  }

  # Add legend title if fill variable is used
  if (!is.null(fill_var)) {
    plot_labs[["fill"]] <- legend_title
  }

  # Apply all labels
  p <- p + do.call(labs, plot_labs)

  # Set legend position
  p <- p + theme(legend.position = legend_position)

  # Flip axes if requested
  if (flip_axes) {
    p <- p + coord_flip()
  }

  p
}

#' Create a heatmap plot with extended styling options
#'
#' @description
#' Creates a ggplot2 heatmap with optional faceting and styling features using viridis color palette
#'
#' @param data A data frame containing the variables to plot
#' @param x_var Name of the variable for the x-axis
#' @param y_var Name of the variable for the y-axis
#' @param fill_var Name of the variable for the cell fill color
#' @param grouping_variable Name of the variable for faceting (default: "None")
#' @param use_facet Logical indicating whether to create facets (default: FALSE)
#' @param facet_cols Number of columns for facets when use_facet=TRUE (default: 2)
#' @param flip_axes Logical indicating whether to flip the x and y axes (default: FALSE)
#' @param agg_func Aggregation function to use ("None", "mean", "median", "min", "max", "sum", "count")
#' @param title Optional plot title (default: NULL)
#' @param subtitle Optional plot subtitle (default: NULL)
#' @param x_label Optional custom x-axis label (default: NULL, uses variable name)
#' @param y_label Optional custom y-axis label (default: NULL, uses variable name)
#' @param fill_label Optional custom fill legend label (default: NULL, uses variable name)
#' @param viridis_option Viridis color palette option ("viridis", "magma", "inferno", "plasma", "cividis") (default: "viridis")
#' @param viridis_direction Direction of the color scale (1: default, -1: reversed)
#' @param show_labels Logical indicating whether to show value labels in cells (default: TRUE)
#' @param label_size Size of value labels (default: 3.5)
#' @param label_decimals Number of decimal places to display in labels (default: 1)
#' @param label_color Color of the labels when auto_text_color is FALSE (default: "black")
#' @param auto_text_color Logical indicating whether to automatically adjust text color based on background (default: TRUE)
#' @param label_fontface Font face for labels ("plain", "bold", "italic", "bold.italic") (default: "bold")
#' @param legend_position Position of the legend ("right", "bottom", "left", "top", or "none")
#'
#' @return A ggplot2 object
#'
#'
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom rlang sym
#'
#' @examples
#' \dontrun{
#' # Basic heatmap with labels
#' af_create_heatmap(mtcars, "cyl", "gear", "mpg", agg_func = "mean")
#'
#' # Advanced styling
#' af_create_heatmap(mtcars, "cyl", "gear", "mpg",
#'                  agg_func = "mean",
#'                  title = "Average MPG by Cylinder Count and Gear",
#'                  x_label = "Number of Cylinders",
#'                  y_label = "Number of Gears",
#'                  fill_label = "Miles Per Gallon",
#'                  viridis_option = "plasma",
#'                  label_decimals = 2,
#'                  auto_text_color = TRUE,
#'                  legend_position = "bottom")
#' }
#'
#' @export
af_create_heatmap <- function(
  data,
  x_var,
  y_var,
  fill_var,
  grouping_variable = "None",
  use_facet = FALSE,
  facet_cols = 2,
  flip_axes = FALSE,
  agg_func = "None",
  title = NULL,
  subtitle = NULL,
  x_label = NULL,
  y_label = NULL,
  fill_label = NULL,
  viridis_option = "viridis",
  viridis_direction = 1,
  show_labels = TRUE,
  label_size = 3.5,
  label_decimals = 1,
  label_color = "black",
  auto_text_color = TRUE,
  label_fontface = "bold",
  legend_position = "right"
) {
  # Ensure required variables are not NULL
  if (is.null(x_var) || is.null(y_var) || is.null(fill_var)) {
    stop("x_var, y_var, and fill_var must all be specified for heatmaps")
  }

  # Filter out NA values
  data <- data %>%
    filter(!is.na(!!sym(x_var))) %>%
    filter(!is.na(!!sym(y_var))) %>%
    filter(!is.na(!!sym(fill_var)))

  # Convert grouping variable to factor
  if ((grouping_variable != "None") && (!is.null(grouping_variable))) {
    data <- data %>%
      filter(!is.na(!!sym(grouping_variable))) %>%
      mutate(across(all_of(grouping_variable), as.factor))
  }

  # Ensure x and y variables are factors for proper heatmap plotting
  data <- data %>%
    mutate(across(all_of(c(x_var, y_var)), as.factor))

  # Apply aggregation if specified
  if (agg_func != "None") {
    group_vars <- c(x_var, y_var, grouping_variable)
    group_vars <- group_vars[group_vars != "None"]
    agg_func_lower <- tolower(agg_func)
    agg_func_to_use <- switch(
      agg_func_lower,
      mean = mean,
      median = median,
      min = min,
      max = max,
      sum = sum,
      count = length
    )
    data <- data %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(fill_value = agg_func_to_use(!!sym(fill_var)), .groups = "drop")
    fill_var <- "fill_value"
  }

  # Calculate text colors if auto_text_color is TRUE
  if (show_labels && auto_text_color && is.numeric(data[[fill_var]])) {
    # Create a column for text color
    fill_range <- range(data[[fill_var]], na.rm = TRUE)
    fill_mid <- mean(fill_range)

    # Add text color column - different approach depending on viridis option
    # For viridis and cividis, darker colors are on the low end
    if (viridis_option %in% c("viridis", "cividis")) {
      data <- data %>%
        mutate(
          text_color = ifelse(!!sym(fill_var) < fill_mid, "white", "black")
        )
    } else {
      # For other palettes like plasma, inferno, magma, darker colors on high end
      data <- data %>%
        mutate(
          text_color = ifelse(!!sym(fill_var) > fill_mid, "white", "black")
        )
    }
  }

  # Format labels based on data type
  if (show_labels) {
    if (is.integer(data[[fill_var]])) {
      data$label_text <- as.character(data[[fill_var]])
    } else if (is.numeric(data[[fill_var]])) {
      format_string <- paste0("%.", label_decimals, "f")
      data$label_text <- sprintf(format_string, data[[fill_var]])
    } else {
      data$label_text <- as.character(data[[fill_var]])
    }
  }

  # Start creating the plot
  p <- ggplot(
    data,
    aes(x = !!sym(x_var), y = !!sym(y_var), fill = !!sym(fill_var))
  )

  # Apply minimal theme
  p <- p + theme_minimal()

  # Add heatmap tiles
  p <- p + geom_tile(color = "white", linewidth = 0.2)

  # Add value labels if requested
  if (show_labels) {
    if (auto_text_color && is.numeric(data[[fill_var]])) {
      # Use automatic text coloring based on background
      p <- p +
        geom_text(
          aes(label = label_text, color = text_color),
          size = label_size,
          fontface = label_fontface
        ) +
        scale_color_identity() # Use the actual colors in text_color
    } else {
      # Use fixed color
      p <- p +
        geom_text(
          aes(label = label_text),
          size = label_size,
          color = label_color,
          fontface = label_fontface
        )
    }
  }

  # Handle facets
  if (use_facet && grouping_variable != "None") {
    p <- p +
      facet_wrap(
        as.formula(paste("~", grouping_variable)),
        ncol = facet_cols,
        labeller = label_both
      )
  }

  # Set axis labels (with defaults)
  default_x_label <- x_var
  default_y_label <- y_var
  default_fill_label <- if (agg_func != "None") {
    paste(agg_func_lower, "of", fill_var)
  } else {
    fill_var
  }

  # Use custom labels if provided, otherwise use defaults
  x_axis_label <- if (!is.null(x_label)) x_label else default_x_label
  y_axis_label <- if (!is.null(y_label)) y_label else default_y_label
  fill_axis_label <- if (!is.null(fill_label)) {
    fill_label
  } else {
    default_fill_label
  }

  # Create labs with all optional elements
  plot_labs <- list(x = x_axis_label, y = y_axis_label, fill = fill_axis_label)

  # Add title and subtitle if provided
  if (!is.null(title)) {
    plot_labs[["title"]] <- title
  }
  if (!is.null(subtitle)) {
    plot_labs[["subtitle"]] <- subtitle
  }

  # Apply all labels
  p <- p + do.call(labs, plot_labs)

  # Apply viridis color scale based on data type
  if (is.numeric(data[[fill_var]])) {
    p <- p +
      scale_fill_viridis_c(
        option = viridis_option,
        direction = viridis_direction
      )
  } else {
    p <- p +
      scale_fill_viridis_d(
        option = viridis_option,
        direction = viridis_direction
      )
  }

  # Set legend position
  p <- p + theme(legend.position = legend_position)

  # Flip axes if requested
  if (flip_axes) {
    p <- p + coord_flip()
  }

  # Remove grid lines for cleaner heatmap appearance
  p <- p +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

  p
}

#' Plot Combinations Chart
#'
#' @description
#' Generates a horizontal bar chart showing counts for combinations of specified variables.
#' Allows filtering based on a percentage threshold and exclusion of specific strings
#' in combination labels. Displays percentage on each bar and uses "Respondents" for the x-axis label.
#' The subtitle includes filtering information and the count/percentage of respondents remaining after filtering.
#' A multi-line caption is added to show the number of combinations included and the included values for each variable.
#'
#' @param df The input dataframe.
#' @param var_names A character vector containing the names of the variables to combine.
#'                  These variables should ideally be factors or convertible to factors.
#' @param threshold A numeric value (0 to 100). If not NULL, combinations with a
#'                  percentage of total rows below this threshold will not be displayed.
#'                  Defaults to NULL (no threshold filtering).
#' @param exclude_strings A character vector of strings. If not NULL, combinations
#'                        whose label contains any of these strings will not be included.
#'                        Defaults to NULL (no string exclusion filtering).
#' @return A ggplot object representing the horizontal bar chart.
#'
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom rlang syms
#'
#' @examples
#' # Assuming you have a dataframe 'my_data' with columns 'A', 'B', 'C'
#' # my_data <- data.frame(A = sample(letters[1:2], 100, replace = TRUE),
#' #                       B = sample(1:3, 100, replace = TRUE),
#' #                       C = sample(c("X", "Y"), 100, replace = TRUE))
#' #
#' # # Example 1: Basic usage
#' # chart1 <- af_plot_combinations_chart(my_data, c("A", "B", "C"))
#' # print(chart1)
#'
#' # # Example 2: Using a threshold (e.g., exclude combinations less than 5%)
#' # chart2 <- af_plot_combinations_chart(my_data, c("A", "B", "C"), threshold = 5)
#' # print(chart2)
#'
#' # # Example 3: Excluding combinations containing "Low" or "Group A"
#' # chart3 <- af_plot_combinations_chart(my_data, c("pe_left_center_right", "religiosity", "leastliked"), exclude_strings = c("Low", "Group A"))
#' # print(chart3)
#'
#' # # Example 4: Using both threshold and exclusion
#' # chart4 <- af_plot_combinations_chart(my_data, c("pe_left_center_right", "religiosity", "leastliked"), threshold = 10, exclude_strings = c("Right_High"))
#' # print(chart4)
#'
#' # # Example 5: Demonstrating error handling (uncomment to test)
#' # try(af_plot_combinations_chart(dummy_df, c("pe_left_center_right", "non_existent_var")))
#' # try(af_plot_combinations_chart(dummy_df, c("pe_left_center_right"), threshold = 101))
#' # try(af_plot_combinations_chart(dummy_df, c("pe_left_center_right"), exclude_strings = 123))
#'
#'
#' @export
af_plot_combinations_chart <- function(
  df,
  var_names,
  threshold = NULL,
  exclude_strings = NULL
) {
  # --- Input Validation ---
  if (!is.data.frame(df)) {
    stop("Input 'df' must be a dataframe.")
  }
  if (!is.character(var_names) || length(var_names) == 0) {
    stop(
      "Input 'var_names' must be a character vector of at least one variable name."
    )
  }
  if (!all(var_names %in% names(df))) {
    missing_vars <- var_names[!var_names %in% names(df)]
    stop(paste(
      "The following variables are not found in the dataframe:",
      paste(missing_vars, collapse = ", ")
    ))
  }
  if (
    !is.null(threshold) &&
      (!is.numeric(threshold) || threshold < 0 || threshold > 100)
  ) {
    stop("Input 'threshold' must be a numeric value between 0 and 100.")
  }
  if (!is.null(exclude_strings) && !is.character(exclude_strings)) {
    stop("Input 'exclude_strings' must be a character vector.")
  }

  # --- Data Preparation ---
  # Get total number of rows in the original dataframe for percentage calculation
  total_rows <- nrow(df)

  # Select only the specified columns and ensure they are factors
  df_subset <- df %>%
    select(all_of(var_names)) %>%
    mutate(across(everything(), factor)) # Convert all selected columns to factors

  # Calculate the counts for each unique combination of the specified factors
  combination_counts <- df_subset %>%
    count(!!!syms(var_names), name = "n") # Use !!!syms() to count by multiple variables

  # Create a single interaction term for the y-axis label (since it will be horizontal)
  # Encapsulate each part in square brackets
  combination_counts$combination_label <- apply(
    combination_counts[, var_names],
    1,
    function(x) paste0("[", x, "]", collapse = "_")
  )

  # Calculate percentage of total rows for each combination
  combination_counts <- combination_counts %>%
    mutate(percentage = (n / total_rows) * 100) %>%
    mutate(percentage_label = paste0(round(percentage, 1), "%")) # Create a formatted percentage label

  # Store initial counts before filtering for subtitle
  initial_combination_counts <- combination_counts

  # --- Filtering ---
  # Apply threshold filtering if threshold is specified
  if (!is.null(threshold)) {
    combination_counts <- combination_counts %>%
      filter(percentage >= threshold)
  }

  # Apply string exclusion filtering if exclude_strings is specified
  if (!is.null(exclude_strings)) {
    # Create a regex pattern to match any of the exclude_strings
    exclude_pattern <- paste(exclude_strings, collapse = "|")
    combination_counts <- combination_counts %>%
      filter(!grepl(exclude_pattern, combination_label))
  }

  # --- Calculate Post-Filtering Stats ---
  # Calculate total respondents and percentage after filtering
  total_respondents_after_filtering <- sum(combination_counts$n)
  percentage_after_filtering <- (total_respondents_after_filtering /
    total_rows) *
    100

  # Calculate the number of combinations included
  num_combinations_included <- nrow(combination_counts)

  # Identify the unique values included for each variable after filtering
  included_values_list <- lapply(var_names, function(var) {
    unique_vals <- unique(combination_counts[[var]])
    paste0(var, ": [", paste(unique_vals, collapse = ", "), "]")
  })
  # Combine included values with newline characters for multi-line caption
  included_values_text <- paste(included_values_list, collapse = "\n")

  # --- Plotting ---
  # Create the ggplot object
  # Map combination_label to the y-axis and n to the x-axis for a horizontal bar chart
  p <- ggplot(combination_counts, aes(y = combination_label, x = n)) +
    geom_bar(stat = "identity", fill = "skyblue") + # Use stat="identity" and a default fill color
    geom_text(
      aes(label = percentage_label), # Add percentage labels to the bars
      hjust = -0.1, # Adjust horizontal position of the label (outside the bar)
      size = 3
    ) + # Adjust label size
    labs(
      title = paste(
        "Counts per Combination of:",
        paste(var_names, collapse = ", ")
      ),
      y = paste(
        "Combination (",
        paste(var_names, collapse = "_"),
        ")",
        sep = ""
      ), # Y-axis label for horizontal chart
      x = "Respondents" # X-axis label changed to "Respondents"
    ) +
    theme(
      # No need to rotate y-axis labels usually, but adjust if needed
      # axis.text.y = element_text(angle = 0, hjust = 1),
      plot.title = element_text(hjust = 0.5), # Center the plot title
      plot.caption = element_text(hjust = 0) # Left-align the caption
    ) +
    # Ensure the x-axis includes enough space for the labels
    scale_x_continuous(expand = expansion(mult = c(0, 0.1)))

  # Add subtitle indicating if filtering was applied and showing post-filtering stats
  subtitle_text <- NULL
  filter_details <- c()

  if (!is.null(threshold)) {
    filter_details <- c(
      filter_details,
      paste("Combinations below", threshold, "% excluded")
    )
  }
  if (!is.null(exclude_strings)) {
    filter_details <- c(
      filter_details,
      paste(
        "those containing:",
        paste(exclude_strings, collapse = ", "),
        "excluded"
      )
    )
  }

  if (length(filter_details) > 0) {
    subtitle_text <- paste(
      "Filtered:",
      paste(filter_details, collapse = " and "),
      "| Displaying",
      total_respondents_after_filtering,
      "Respondents (",
      round(percentage_after_filtering, 1),
      "% of total)"
    )
  }

  if (!is.null(subtitle_text)) {
    p <- p + labs(subtitle = subtitle_text)
  }

  # Add multi-line caption with number of combinations and included values
  caption_text <- paste0(
    "Included Combinations: ",
    num_combinations_included,
    "\n",
    "Included Values:\n",
    included_values_text
  )

  p <- p + labs(caption = caption_text)

  # Return the plot object
  return(p)
}

#' Create a Q-Q plot for normality assessment
#'
#' @description
#' Creates a quantile-quantile (Q-Q) plot to assess whether data follows a normal distribution.
#' The plot includes reference points, a theoretical line, and a confidence band.
#'
#' @param df A data frame containing the variable to plot
#' @param var_name Character string specifying the name of the variable to assess
#'
#' @return A ggplot2 object
#'
#' @import ggplot2
#' @importFrom qqplotr stat_qq_band stat_qq_line stat_qq_point
#'
#' @examples
#' \dontrun{
#' # Basic Q-Q plot
#' af_plot_qq(mtcars, "mpg")
#'
#' # Assess normality of residuals
#' model <- lm(mpg ~ wt, data = mtcars)
#' residuals_df <- data.frame(residuals = residuals(model))
#' af_plot_qq(residuals_df, "residuals")
#' }
#'
#' @export
af_plot_qq <- function(df, var_name) {
  ggplot(df, aes_string(sample = var_name)) +
    qqplotr::stat_qq_point() +
    qqplotr::stat_qq_line() +
    qqplotr::stat_qq_band(alpha = 0.3) +
    labs(x = "Theoretical Quantiles", y = paste(var_name, "Sample Quantiles"))
}

#' Create a histogram or bar chart with frequency counts
#'
#' @description
#' Generates a histogram for numeric variables or a bar chart for categorical
#' variables. Automatically detects variable type and adjusts visualization
#' accordingly. Bar charts include percentage labels above bars, and all plots
#' display total sample size in the top-right corner.
#'
#' @param df (data.frame) Input data frame containing the variable to plot
#' @param y_name (character) Name of the column to visualize
#' @param nbins (numeric) Number of bins for histogram. Default is 50
#'
#' @return (ggplot) A ggplot object showing either a histogram (numeric) or bar chart (categorical)
#'
#'
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom rlang .data
#'
#' @examples
#' af_plot_histogram(mtcars, "mpg", nbins = 30)
#' af_plot_histogram(mtcars, "cyl")
#'
#' @export
af_plot_histogram <- function(df, y_name, nbins = 50) {
  total_population <- nrow(df) # Calculate total population size

  if (is.numeric(df[[y_name]])) {
    ggplot(df, aes(x = .data[[y_name]])) +
      geom_histogram(
        alpha = 0.5,
        position = "identity",
        color = "black",
        bins = nbins
      ) +
      annotate(
        "text",
        x = Inf,
        y = Inf,
        label = paste("Total N =", total_population),
        hjust = 1.1,
        vjust = 1.5,
        size = 4
      ) +
      labs(x = y_name, y = "Count")
  } else {
    # Calculate percentages
    df_summary <- df %>%
      group_by(.data[[y_name]]) %>%
      summarise(count = n()) %>%
      mutate(percentage = count / sum(count) * 100)

    ggplot(df_summary, aes(x = .data[[y_name]], y = count)) +
      geom_bar(stat = "identity", alpha = 0.5, position = "identity") +
      geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
      annotate(
        "text",
        x = Inf,
        y = Inf,
        label = paste("Total N =", total_population),
        hjust = 1.1,
        vjust = 1.5,
        size = 4
      ) +
      labs(x = y_name, y = "Count")
  }
}

#' Plot Distribution with Histogram and Density Curve
#'
#' @description
#' Creates a distribution plot for a variable in a data frame. For numeric
#' variables, generates a histogram overlaid with a kernel density curve and
#' displays total sample size. For non-numeric variables, creates a bar plot.
#'
#' @param df (data.frame) The data frame containing the variable to plot
#' @param y_name (character) The name of the column to plot
#' @param title (character) The plot title. Default is " "
#' @param nbins (numeric) The number of bins for the histogram when plotting numeric variables. Default is 50
#'
#' @return (ggplot) A ggplot object displaying the distribution
#'
#'
#'
#' @import ggplot2
#'
#' @examples
#' # Numeric variable with density overlay
#' af_plot_distribution(mtcars, "mpg", title = "MPG Distribution")
#'
#' @export
af_plot_distribution <- function(df, y_name, title = " ", nbins = 50) {
  total_population <- nrow(df)

  if (is.numeric(df[, y_name])) {
    p <- ggplot(df, aes_string(x = y_name)) +
      geom_histogram(
        aes(y = ..density..),
        colour = "black",
        fill = "white",
        bins = nbins,
        na.rm = TRUE
      ) +
      geom_density(alpha = .2, fill = "#FF6666", na.rm = TRUE) +
      annotate(
        "text",
        x = Inf,
        y = Inf,
        label = paste("Total N =", total_population),
        hjust = 1.1,
        vjust = 1.5,
        size = 4
      )
  } else {
    p <- ggplot(df, aes_string(x = y_name)) +
      geom_bar(alpha = 0.5, position = "identity", na.rm = TRUE)
  }

  p <- p + labs(title = title)

  return(p)
}

#' Plot Density Distribution
#'
#' @description
#' Creates a kernel density plot for a numeric variable in a data frame. The plot
#' displays a smooth density curve with blue fill and includes an annotation showing
#' the count of non-missing values in the upper right corner.
#'
#' @param df (data.frame) The data frame containing the variable to plot
#' @param y_name (character) The name of the column to plot
#'
#' @return (ggplot) A ggplot object displaying the density distribution with frequency on the y-axis
#'
#'
#'
#' @import ggplot2
#' @importFrom rlang .data
#'
#' @examples
#' af_plot_density(mtcars, "mpg")
#'
#' @export
af_plot_density <- function(df, y_name) {
  n_points <- sum(!is.na(df[[y_name]]))

  p <- ggplot(df, aes(x = .data[[y_name]])) +
    geom_density(fill = "blue", alpha = 0.5) +
    labs(y = "Frequency") +
    annotate(
      "text",
      x = Inf,
      y = Inf,
      label = paste("n =", n_points),
      hjust = 1.1,
      vjust = 1.5,
      size = 3,
      color = "black"
    )

  return(p)
}

#' Plot Bar Chart
#'
#' @description
#' Creates a bar chart for a categorical or discrete variable in a data frame. The plot
#' displays count frequencies with blue fill and includes an annotation showing the count
#' of non-missing values in the upper right corner.
#'
#' @param df (data.frame) The data frame containing the variable to plot
#' @param y_name (character) The name of the column to plot
#'
#' @return (ggplot) A ggplot object displaying the bar chart with count on the y-axis
#'
#'
#'
#' @import ggplot2
#' @importFrom rlang .data
#'
#' @examples
#' af_plot_barchart(mtcars, "cyl")
#'
#' @export
af_plot_barchart <- function(df, y_name) {
  # Create bar charts for each variable
  n_points <- sum(!is.na(df[[y_name]]))

  p <- ggplot(df, aes(x = .data[[y_name]])) +
    geom_bar(fill = "blue", alpha = 0.5) +
    labs(y = "Count") +
    annotate(
      "text",
      x = Inf,
      y = Inf,
      label = paste("n =", n_points),
      hjust = 1.1,
      vjust = 1.5,
      size = 3,
      color = "black"
    )

  return(p)
}

#' Plot Correlation Matrix
#'
#' @description
#' Creates a correlation plot displaying pairwise correlations between numeric variables
#' in a data frame. The plot uses circles in the lower triangle with correlation coefficients
#' as labels, and handles missing values using pairwise complete observations.
#'
#' @param df (data.frame) The data frame containing the variables to correlate
#' @param vars (character vector) The names of variables to include in the correlation plot. Default is NULL, which uses all variables in the data frame
#' @param title (character) The plot title. Default is "Correlation Plot"
#'
#' @return (ggplot) A ggcorrplot object displaying the correlation matrix with circle method and lower triangle layout
#'
#'
#'
#' @import dplyr
#' @importFrom ggcorrplot ggcorrplot
#' @importFrom stats cor
#' @importFrom viridis viridis
#'
#' @examples
#' af_plot_correlation(mtcars)
#' af_plot_correlation(mtcars, c("mpg", "hp", "wt"))
#'
#' @export
af_plot_correlation <- function(df, vars = NULL, title = "Correlation Plot") {
  if (is.null(vars)) {
    vars <- names(df)
  }

  vars <- af_remove_empty_vars(df, vars)

  # Check if all variables in vars exist in df
  if (length(vars) < 2) {
    stop("Not all specified variables are present in the dataframe")
  }

  # Select the relevant columns
  selected_data <- df %>% dplyr::select(all_of(vars))

  # Calculate the correlation matrix
  corr_matrix <- stats::cor(selected_data, use = "na.or.complete")

  # Create the correlation plot
  ggcorrplot::ggcorrplot(
    corr_matrix,
    method = "circle",
    type = "lower",
    lab = TRUE,
    lab_size = 3,
    colors = viridis::viridis(3, begin = 0.4, end = 1),
    title = title
  )
}

#' Plot Correlation Matrices by Group
#'
#' @description
#' Creates separate correlation plots for each level of a grouping variable. For each
#' group value, the function filters the data and generates a correlation matrix plot
#' with the group identifier in the title. Returns a list of correlation plots.
#'
#' @param df (data.frame) The data frame containing the variables to correlate and the grouping variable
#' @param group_var (character) The name of the grouping variable
#' @param group_values (vector) The specific group values to create plots for. Default is NULL, which uses all unique values of the grouping variable
#' @param vars (character vector) The names of variables to include in the correlation plots. Default is NULL, which uses all variables in the data frame
#'
#' @return (list) A list of ggplot objects, one correlation plot for each group value
#'
#'
#'
#' @import dplyr
#' @importFrom rlang .data
#'
#' @examples
#' plots <- af_plot_correlation_per_group(mtcars, "cyl")
#' plots <- af_plot_correlation_per_group(mtcars, "cyl", c(4, 6), c("mpg", "hp", "wt"))
#'
#' @export
af_plot_correlation_per_group <- function(
  df,
  group_var,
  group_values = NULL,
  vars = NULL
) {
  if (is.null(vars)) {
    vars <- names(df)
  }
  if (is.null(group_values)) {
    group_values <- unique(df[[group_var]])
  }

  # Initialize list for storing plots
  plot_list <- list()
  plot_counter <- 1

  for (g in group_values) {
    dfw <- df %>% filter(.data[[group_var]] == g)
    title = paste(group_var, "=", g)
    p <- af_plot_correlation(dfw, vars, title)
    plot_list[[plot_counter]] <- p
    plot_counter <- plot_counter + 1
  }

  return(plot_list)
}

#' Plot Distributions by Group
#'
#' @description
#' Creates separate distribution plots for a variable across different levels of a grouping
#' variable. For each group value, the function filters the data and generates a distribution
#' plot with the group identifier in the title. Returns a list of distribution plots.
#'
#' @param df (data.frame) The data frame containing the variable to plot and the grouping variable
#' @param y_name (character) The name of the column to plot
#' @param group_var (character) The name of the grouping variable
#' @param group_values (vector) The specific group values to create plots for. Default is NULL, which uses all unique values of the grouping variable
#'
#' @return (list) A list of ggplot objects, one distribution plot for each group value
#'
#'
#'
#' @import dplyr
#' @importFrom rlang .data
#'
#' @examples
#' plots <- af_plot_distribution_per_group(mtcars, "mpg", "cyl")
#' plots <- af_plot_distribution_per_group(mtcars, "mpg", "cyl", c(4, 6))
#'
#' @export
af_plot_distribution_per_group <- function(
  df,
  y_name,
  group_var,
  group_values = NULL
) {
  if (is.null(group_values)) {
    group_values <- unique(df[[group_var]])
  }

  # Initialize list for storing plots
  dist_plots <- list()
  plot_counter <- 1

  for (g in group_values) {
    dfw <- df %>% filter(.data[[group_var]] == g)
    p <- af_plot_distribution(dfw, y_name, title = paste(group_var, "=", g))
    dist_plots[[plot_counter]] <- p
    plot_counter <- plot_counter + 1
  }

  return(dist_plots)
}

#' Combine plots in a grid with optional title, subtitle, note, and legend options
#'
#' @description
#' This function arranges a list of ggplot objects in a grid layout and can
#' optionally add a main title, subtitle, note, extract a common legend from plots,
#' or create a custom external legend from color and line type specifications.
#' Spacing between plots can be controlled using row_spacing and col_spacing parameters.
#'
#' @param plot_list A list of ggplot objects to be combined
#' @param ncol Integer. Number of columns in the grid layout (default: 2)
#' @param main_title Character string. Main title for the combined plot (default: NULL)
#' @param subtitle Character string. Subtitle for the combined plot (default: NULL)
#' @param note Character string. Note to be placed at the bottom (default: NULL)
#' @param common_legend Logical. Whether to extract and use a common legend from plots (default: FALSE)
#' @param legend_position Character. Position of the common legend: "right", "bottom", "top", "left" (default: "right")
#' @param external_legend List. List of pairs defining color and line type for custom legend (default: NULL)
#'   Format: list(c("color", "linetype"), c("color", "linetype"), ...)
#'   Example: list(c("red", "dashed"), c("blue", "dotted"), c("green", "solid"))
#' @param legend_labels Character vector. Labels for external legend items (default: NULL, uses "Item 1", "Item 2", etc.)
#' @param row_spacing Numeric. Spacing between rows in the grid layout in lines (default: 0.5)
#' @param col_spacing Numeric. Spacing between columns in the grid layout in lines (default: 0.5)
#'
#' @return Invisibly returns the combined plot object
#'
#'
#' @import ggplot2
#'
#' @examples
#' # Basic usage
#' af_combine_plots(list(plot1, plot2), ncol = 2)
#'
#' # With common legend from plots
#' af_combine_plots(list(plot1, plot2),
#'                  main_title = "Main Title",
#'                  common_legend = TRUE)
#'
#' # With custom external legend
#' af_combine_plots(list(plot1, plot2),
#'                  main_title = "Analysis Results",
#'                  external_legend = list(c("red", "dashed"), c("blue", "dotted"), c("green", "solid")),
#'                  legend_labels = c("Treatment A", "Treatment B", "Control"))
#'
#' # With reduced spacing between plots
#' af_combine_plots(list(plot1, plot2, plot3, plot4),
#'                  ncol = 2,
#'                  main_title = "Tight Layout",
#'                  row_spacing = 0.1,
#'                  col_spacing = 0.1)
#'
#'
#' @export
af_combine_plots <- function(
  plot_list,
  ncol = 2,
  main_title = NULL,
  subtitle = NULL,
  note = NULL,
  common_legend = FALSE,
  legend_position = "right",
  external_legend = NULL,
  legend_labels = NULL,
  row_spacing = 0.5,
  col_spacing = 0.5
) {
  # Input parameter validation
  if (!is.list(plot_list)) {
    stop("plot_list must be a list of ggplot objects")
  }

  if (length(plot_list) == 0) {
    stop("plot_list cannot be empty")
  }

  if (!all(sapply(plot_list, function(x) inherits(x, "gg")))) {
    stop("All elements in plot_list must be ggplot objects")
  }

  if (!is.numeric(ncol) || ncol < 1) {
    stop("ncol must be a positive integer")
  }

  if (!is.logical(common_legend)) {
    stop("common_legend must be TRUE or FALSE")
  }

  if (!legend_position %in% c("right", "bottom", "top", "left")) {
    stop("legend_position must be one of: 'right', 'bottom', 'top', 'left'")
  }

  if (common_legend && !is.null(external_legend)) {
    stop("Cannot use both common_legend and external_legend simultaneously")
  }

  if (!is.null(external_legend)) {
    if (!is.list(external_legend)) {
      stop("external_legend must be a list of color-linetype pairs")
    }
    if (!all(sapply(external_legend, function(x) length(x) == 2))) {
      stop(
        "Each element in external_legend must be a vector of length 2 (color, linetype)"
      )
    }
  }

  if (!is.null(legend_labels) && !is.null(external_legend)) {
    if (length(legend_labels) != length(external_legend)) {
      stop("legend_labels must have the same length as external_legend")
    }
  }

  # Create title elements
  title_grobs <- list()
  if (!is.null(main_title)) {
    title_grobs <- append(
      title_grobs,
      list(textGrob(main_title, gp = gpar(fontsize = 16, fontface = "bold")))
    )
  }
  if (!is.null(subtitle)) {
    title_grobs <- append(
      title_grobs,
      list(textGrob(subtitle, gp = gpar(fontsize = 12)))
    )
  }

  # Create note element
  note_grob <- NULL
  if (!is.null(note)) {
    note_grob <- textGrob(note, gp = gpar(fontsize = 10, fontface = "italic"))
  }

  # Handle legend creation
  legend_grob <- NULL
  plots_to_use <- plot_list

  # Apply custom spacing through plot margins
  # Convert spacing parameters to margin values
  # Smaller spacing = smaller margins between plots
  vertical_margin <- max(1, row_spacing * 8) # Scale factor for readability
  horizontal_margin <- max(1, col_spacing * 8) # Scale factor for readability

  plots_to_use <- lapply(plots_to_use, function(p) {
    p +
      theme(
        plot.margin = margin(
          t = vertical_margin,
          r = horizontal_margin,
          b = vertical_margin,
          l = horizontal_margin,
          unit = "pt"
        )
      )
  })

  # External legend takes precedence
  if (!is.null(external_legend)) {
    # Create labels if not provided
    if (is.null(legend_labels)) {
      legend_labels <- paste("Item", seq_along(external_legend))
    }

    # Create custom legend using the working function
    legend_grob <- af_create_custom_legend(external_legend, legend_labels)

    # Remove legends from all plots
    plots_to_use <- lapply(plots_to_use, function(p) {
      p + theme(legend.position = "none")
    })
  } else if (common_legend) {
    # Extract legend from the first plot that has one
    for (i in seq_along(plot_list)) {
      temp_legend <- ggplotGrob(plot_list[[i]])
      legend_idx <- which(
        sapply(temp_legend$grobs, function(x) x$name) == "guide-box"
      )
      if (length(legend_idx) > 0) {
        legend_grob <- temp_legend$grobs[[legend_idx]]
        break
      }
    }

    # Remove legends from all plots
    plots_to_use <- lapply(plots_to_use, function(p) {
      p + theme(legend.position = "none")
    })
  }

  # Convert plots to grobs
  plot_grobs <- lapply(plots_to_use, ggplotGrob)

  # Create the main plot grid
  main_grid <- arrangeGrob(grobs = plot_grobs, ncol = ncol)

  # For external legend, always place at bottom
  if (!is.null(external_legend)) {
    combined_grid <- arrangeGrob(
      main_grid,
      legend_grob,
      nrow = 2,
      heights = c(10, 1)
    )
  } else if (common_legend && !is.null(legend_grob)) {
    # Handle common legend positioning
    if (legend_position == "right") {
      combined_grid <- arrangeGrob(
        main_grid,
        legend_grob,
        ncol = 2,
        widths = c(4, 1)
      )
    } else if (legend_position == "bottom") {
      combined_grid <- arrangeGrob(
        main_grid,
        legend_grob,
        nrow = 2,
        heights = c(4, 1)
      )
    } else if (legend_position == "top") {
      combined_grid <- arrangeGrob(
        legend_grob,
        main_grid,
        nrow = 2,
        heights = c(1, 4)
      )
    } else if (legend_position == "left") {
      combined_grid <- arrangeGrob(
        legend_grob,
        main_grid,
        ncol = 2,
        widths = c(1, 4)
      )
    }
  } else {
    combined_grid <- main_grid
  }

  # Create final layout with titles and notes
  final_elements <- list()
  final_heights <- c()

  # Add titles
  if (length(title_grobs) > 0) {
    title_grid <- arrangeGrob(grobs = title_grobs, ncol = 1)
    final_elements <- append(final_elements, list(title_grid))
    final_heights <- c(final_heights, length(title_grobs) * 0.5)
  }

  # Add main content
  final_elements <- append(final_elements, list(combined_grid))
  final_heights <- c(final_heights, 10)

  # Add note
  if (!is.null(note_grob)) {
    final_elements <- append(final_elements, list(note_grob))
    final_heights <- c(final_heights, 0.5)
  }

  # Create final arrangement
  if (length(final_elements) > 1) {
    final_plot <- arrangeGrob(
      grobs = final_elements,
      nrow = length(final_elements),
      heights = final_heights
    )
  } else {
    final_plot <- final_elements[[1]]
  }

  # Display the plot
  grid.draw(final_plot)

  return(invisible(final_plot))
}

#' Create custom legend grob
#'
#' @description
#' Create a custom legend from color and line type specifications
#'
#' @param legend_specs List of color-linetype pairs
#' @param labels Character vector of labels
#'
#' @return A grob object representing the legend
#'
#'
#' @import ggplot2
#'
#' @examples
#' #' legend_specs <- list(
#'  c("red", "dashed"),
#' c("blue", "dotted"),
#' c("green", "solid")
#' )
#' labels <- c("Treatment A", "Treatment B", "Control")
#' legend_grob <- af_create_custom_legend(legend_specs, labels)
#' grid.newpage()
#' grid.draw(legend_grob)
#'
#' @export
af_create_custom_legend <- function(legend_specs, labels) {
  # Input validation
  if (!is.list(legend_specs) || length(legend_specs) == 0) {
    stop("legend_specs must be a non-empty list")
  }

  if (!is.character(labels) || length(labels) != length(legend_specs)) {
    stop("labels must be a character vector with same length as legend_specs")
  }

  # Convert line type names to appropriate values
  convert_linetype <- function(lt) {
    lt_lower <- tolower(lt)
    switch(
      lt_lower,
      "solid" = 1,
      "continuous" = 1,
      "dashed" = 2,
      "dotted" = 3,
      "dotdash" = 4,
      "longdash" = 5,
      "twodash" = 6,
      lt # Return original if not recognized
    )
  }

  n_items <- length(legend_specs)
  legend_grobs <- list()

  for (i in seq_along(legend_specs)) {
    color <- legend_specs[[i]][1]
    linetype <- convert_linetype(legend_specs[[i]][2])
    label <- labels[i]

    # Calculate positions for horizontal layout
    item_width <- 1 / n_items
    x_start <- (i - 1) * item_width
    x_end <- x_start + item_width * 0.3 # Line takes 30% of item width
    x_text <- x_start + item_width * 0.35 # Text starts at 35%

    # Create line grob
    line_grob <- linesGrob(
      x = unit(c(x_start + 0.05, x_end), "npc"),
      y = unit(0.5, "npc"),
      gp = gpar(col = color, lty = linetype, lwd = 2)
    )

    # Create text grob
    text_grob <- textGrob(
      label = label,
      x = unit(x_text, "npc"),
      y = unit(0.5, "npc"),
      just = "left",
      gp = gpar(fontsize = 10)
    )

    legend_grobs <- c(legend_grobs, list(line_grob), list(text_grob))
  }

  # Create the combined grob
  legend_grob <- gTree(
    children = do.call(gList, legend_grobs),
    vp = viewport(width = unit(1, "npc"), height = unit(2, "lines"))
  )

  return(legend_grob)
}

#' Save ggplot with Custom Defaults
#'
#' @description
#' Saves a ggplot object to a file with preset dimensions and resolution suitable for
#' publication-quality figures. Optionally displays the plot before saving. Wraps the
#' ggplot2::ggsave function with convenient default parameters.
#'
#' @param filename (character) The file path where the plot will be saved
#' @param plot (ggplot) The plot object to save. Default is last_plot(), which saves the most recently created plot
#' @param width (numeric) The width of the saved plot. Default is 180
#' @param height (numeric) The height of the saved plot. Default is 90
#' @param units (character) The units for width and height. Default is "mm"
#' @param dpi (numeric) The resolution in dots per inch. Default is 600
#' @param display (logical) Whether to display the plot before saving. Default is TRUE
#'
#' @return NULL (called for side effects of saving file and optionally displaying plot)
#'
#'
#' @examples
#' p <- ggplot(mtcars, aes(x = mpg)) + geom_histogram()
#' af_ggsave("myplot.png", p)
#'
#' @export
af_ggsave <- function(
  filename,
  plot = last_plot(),
  width = 180,
  height = 90,
  units = "mm",
  dpi = 600,
  display = TRUE
) {
  if (display) {
    print(plot)
  }

  ggsave(
    filename,
    plot = plot,
    width = width,
    height = height,
    units = units,
    dpi = dpi
  )
}
