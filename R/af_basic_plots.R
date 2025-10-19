
# ----------- af_create_xy_plot ---------------------------------------------------

af_create_xy_plot <- function(data, x_var = NULL, y_var, 
                              grouping_variable = "None", subgrouping_variable = "None", 
                              use_facet = FALSE, facet_cols = 2, 
                              smooth = FALSE, show_points = FALSE, flip_axes = FALSE, 
                              agg_func = "None") {
  
  # Create x variable if NULL
  if (is.null(x_var)) {
    data$.index <- 1:nrow(data)
    x_var <- ".index"
  } else {
    # Filter NA values for x_var
    data <- data %>% filter(!is.na(!!sym(x_var)))
  }
  
  # Filter NA values for y_var
  data <- data %>% filter(!is.na(!!sym(y_var)))
  
  # Convert grouping variables to factors
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
    agg_func_to_use <- switch(agg_func_lower,
                              "mean" = mean,
                              "median" = median,
                              "min" = min,
                              "max" = max,
                              "sum" = sum,
                              "count" = length)
    
    data <- data %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(y = agg_func_to_use(!!sym(y_var)), .groups = "drop")
    y_var <- "y"
  }
  
  # Rest of function remains the same
  p <- ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var)))
  
  if (grouping_variable != "None") {
    if (subgrouping_variable != "None") {
      p <- p + aes(color = interaction(!!sym(grouping_variable), !!sym(subgrouping_variable), sep = " - "),
                   group = interaction(!!sym(grouping_variable), !!sym(subgrouping_variable), sep = " - "))
      legend_title <- paste(grouping_variable, subgrouping_variable, sep = ", ")
    } else {
      p <- p + aes(color = !!sym(grouping_variable), group = !!sym(grouping_variable))
      legend_title <- grouping_variable
    }
  }
  
  if (show_points) {
    p <- p + geom_point(alpha = 0.6)
  }
  
  # Only add connecting lines if not using smoothing
  if (!smooth) {
    p <- p + geom_line(alpha = 0.8)
  }
  
  if (smooth) {
    unique_x_count <- length(unique(data[[x_var]]))
    if (unique_x_count >= 4) {
      if (unique_x_count < 10) {
        p <- p + geom_smooth(se = FALSE, method = "lm", formula = y ~ x, na.rm = TRUE)
      } else {
        span <- min(0.75, max(0.25, 10 / unique_x_count))
        p <- p + geom_smooth(se = FALSE, method = "loess", formula = y ~ x, span = span, na.rm = TRUE)
      }
    }
  }
  
  if (use_facet) {
    if (subgrouping_variable != "None") {
      p <- p + facet_wrap(as.formula(paste("~", grouping_variable, "+", subgrouping_variable)), 
                          ncol = facet_cols, labeller = label_both)
    } else if (grouping_variable != "None") {
      p <- p + facet_wrap(as.formula(paste("~", grouping_variable)), 
                          ncol = facet_cols, labeller = label_both)
    }
  }
  
  # Use appropriate x-axis label
  x_label <- if(x_var == ".index") "Index" else x_var
  p <- p + labs(x = x_label, y = y_var)
  
  if (grouping_variable != "None") {
    p <- p + scale_color_viridis(discrete = TRUE, na.value = "grey50") +
      labs(color = legend_title)
  } else {
    p <- p + scale_color_viridis(discrete = FALSE)
  }
  
  if (flip_axes) {
    p <- p + coord_flip()
  }
  
  p
}

# ----------- af_create_x_multi_y_plot -----------------------------------------

af_create_x_multi_y_plot <- function(data, x_var = NULL, y_var_names, 
                                     use_facet = FALSE, facet_cols = 2, 
                                     smooth = FALSE, show_points = FALSE, flip_axes = FALSE, 
                                     agg_func = "None") {
  
  # Check if y_var_names is a character vector
  if (!is.character(y_var_names)) {
    stop("y_var_names must be a character vector of column names")
  }
  
  # Check if all y variables exist in the data
  missing_vars <- y_var_names[!y_var_names %in% colnames(data)]
  if (length(missing_vars) > 0) {
    stop(paste("The following y variables are not found in the data:", 
               paste(missing_vars, collapse = ", ")))
  }
  
  # Create x variable if NULL
  if (is.null(x_var)) {
    data$.index <- 1:nrow(data)
    x_var <- ".index"
  } else {
    # Ensure x_var exists in data
    if (!x_var %in% colnames(data)) {
      stop(paste("x variable", x_var, "not found in data"))
    }
    
    # Filter NA values for x_var
    data <- data %>% filter(!is.na(!!sym(x_var)))
  }
  
  # Filter NA values for all y variables
  for (y_var in y_var_names) {
    data <- data %>% filter(!is.na(!!sym(y_var)))
  }
  
  # Apply aggregation if specified
  if (agg_func != "None") {
    agg_func_lower <- tolower(agg_func)
    agg_func_to_use <- switch(agg_func_lower,
                              "mean" = mean,
                              "median" = median,
                              "min" = min,
                              "max" = max,
                              "sum" = sum,
                              "count" = length)
    
    # Group by x variable for aggregation
    agg_data <- data %>%
      group_by(!!sym(x_var))
    
    # Apply aggregation function to each y variable
    agg_data <- agg_data %>%
      summarise(across(all_of(y_var_names), agg_func_to_use), .groups = "drop")
    
    data <- agg_data
  }
  
  # Convert to long format for plotting multiple y variables
  plot_data <- data %>%
    select(all_of(c(x_var, y_var_names))) %>%
    pivot_longer(cols = all_of(y_var_names), 
                 names_to = "variable", 
                 values_to = "value")
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = !!sym(x_var), y = value, color = variable, group = variable))
  
  if (show_points) {
    p <- p + geom_point(alpha = 0.6)
  }
  
  # Only add connecting lines if not using smoothing
  if (!smooth) {
    p <- p + geom_line(alpha = 0.8)
  }
  
  # Add smoothing if requested
  if (smooth) {
    unique_x_count <- length(unique(data[[x_var]]))
    if (unique_x_count >= 4) {
      if (unique_x_count < 10) {
        p <- p + geom_smooth(se = FALSE, method = "lm", formula = y ~ x, na.rm = TRUE)
      } else {
        span <- min(0.75, max(0.25, 10 / unique_x_count))
        p <- p + geom_smooth(se = FALSE, method = "loess", formula = y ~ x, span = span, na.rm = TRUE)
      }
    }
  }
  
  # Add faceting if requested
  if (use_facet) {
    p <- p + facet_wrap(~ variable, ncol = facet_cols, labeller = label_value)
  }
  
  # Add labels
  x_label <- if(x_var == ".index") "Index" else x_var
  p <- p + labs(x = x_label, y = "Value")
  
  # Set colors
  p <- p + scale_color_viridis(discrete = TRUE, na.value = "grey50") +
    labs(color = "Variables")
  
  # Flip axes if requested
  if (flip_axes) {
    p <- p + coord_flip()
  }
  
  # Apply theme
  p <- p + theme_minimal()
  
  return(p)
}

# ----------- af_create_y_plot -------------------------------------------------

af_create_y_plot <- function(data, y_var, group_var = NULL, subgroup_var = NULL, plot_types = c("bar"), 
                             bins = 30, use_facet = FALSE, facet_cols = 2, agg_func = NULL) {
  
  # Convert grouping variables to factors
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
        p <- p + aes_string(fill = paste0("interaction(", group_var, ", ", subgroup_var, ")"),
                            group = paste0("interaction(", group_var, ", ", subgroup_var, ")"))
      } else {
        p <- p + aes_string(fill = subgroup_var, group = subgroup_var)
      }
    }
    
    # Add layers based on plot types
    for (type in plot_types) {
      switch(type,
             "histogram" = {
               if (is.null(group_var) && is.null(subgroup_var)) {
                 p <- p + geom_histogram(aes_string(x = y_var), bins = bins, color = "black", alpha = 0.7)
               } else {
                 p <- p + geom_histogram(aes_string(x = y_var), bins = bins, color = "black", alpha = 0.7, position = "identity")
               }
             },
             "histogram dodged" = {
               p <- p + geom_histogram(aes_string(x = y_var), bins = bins, color = "black", alpha = 0.7, position = position_dodge())
             },
             "boxplot" = {
               p <- p + geom_boxplot(aes_string(x = ifelse(is.null(group_var), "", group_var), y = y_var), color = "black", alpha = 0.7)
             },
             "violin" = {
               p <- p + geom_violin(aes_string(x = ifelse(is.null(group_var), "", group_var), y = y_var), color = "black", alpha = 0.7)
             },
             "density" = {
               if (is.null(group_var) && is.null(subgroup_var)) {
                 p <- p + geom_density(aes_string(x = y_var), color = "black", alpha = 0.7)
               } else {
                 p <- p + geom_density(aes_string(x = y_var), color = "black", alpha = 0.7)
               }
             },
             "bar" = {
               if (!is.null(group_var) || !is.null(subgroup_var)) {
                 p <- p + geom_col(aes_string(x = group_var, y = y_var), color = "black", alpha = 0.7, position = position_dodge())
               }
             }
      )
    }
    
  } else {
    # Handling factor y_var
    p <- ggplot(data) + aes_string(x = y_var)
    
    if (!is.null(group_var)) {
      p <- p + aes_string(fill = group_var, group = group_var)
    }
    if (!is.null(subgroup_var)) {
      p <- p + aes_string(fill = paste0("interaction(", group_var, ", ", subgroup_var, ")"),
                          group = paste0("interaction(", group_var, ", ", subgroup_var, ")"))
    }
    
    # Add layers based on plot types
    for (type in plot_types) {
      if (type == "bar") {
        p <- p + geom_bar(position = position_dodge(), color = "black", alpha = 0.7)
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
      p <- p + facet_wrap(vars(interaction(!!sym(group_var), !!sym(subgroup_var))), ncol = facet_cols)
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
    labs(x = y_var, 
         y = "frequency", 
         fill = legend_title) +
    theme(legend.position = "right") +
    scale_fill_viridis(discrete = TRUE, na.value = "grey50")
  
  return(p)
}

# ----------- af_plot functions ---------------------------------------------------

af_plot_qq <- function (df, var_name)
{
  ggplot(df, aes_string(sample=var_name)) +
    qqplotr::stat_qq_point() +
    qqplotr::stat_qq_line() +
    qqplotr::stat_qq_band(alpha=0.3) +
    labs(x = "Theoretical Quantiles",
         y = paste(var_name,"Sample Quantiles"))
}

af_plot_histogram <- function(df, y_name, nbins = 50) {
  total_population <- nrow(df)  # Calculate total population size
  
  if (is.numeric(df[[y_name]])) {
    ggplot(df, aes(x = .data[[y_name]])) +
      geom_histogram(alpha = 0.5, position = "identity", color = "black", bins = nbins) +
      annotate(
        "text",
        x = Inf, y = Inf,
        label = paste("Total N =", total_population),
        hjust = 1.1, vjust = 1.5,
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
        x = Inf, y = Inf,
        label = paste("Total N =", total_population),
        hjust = 1.1, vjust = 1.5,
        size = 4
      ) +
      labs(x = y_name, y = "Count")
  }
}

# Distribution: Histogram overlaid with kernel density curve
af_plot_distribution <- function(df, y_name, title = " ", nbins = 50)
{
  total_population <- nrow(df)
  
  if (is.numeric(df[,y_name])) {
    p <- ggplot(df, aes_string(x=y_name)) + 
      geom_histogram(aes(y=..density..), colour="black", fill="white", 
                     bins=nbins, na.rm = TRUE) +
      geom_density(alpha=.2, fill="#FF6666", na.rm = TRUE)+
      annotate(
        "text",
        x = Inf, y = Inf,
        label = paste("Total N =", total_population),
        hjust = 1.1, vjust = 1.5,
        size = 4
      )
  } else {
    p <- ggplot(df, aes_string(x=y_name)) + 
      geom_bar(alpha=0.5, position="identity", na.rm = TRUE)
  }
  
  p <- p + labs(title = title)
  
  return(p)
}

af_plot_density <- function(df, y_name) {
  
  n_points <- sum(!is.na(df[[y_name]]))
  
  p <- ggplot(df, aes(x = .data[[y_name]])) +
    geom_density(fill = "blue", alpha = 0.5) +
    labs(y = "Frequency") +
    annotate("text", x = Inf, y = Inf, label = paste("n =", n_points), 
             hjust = 1.1, vjust = 1.5, size = 3, color = "black")
  
  return(p)
}

af_plot_barchart <- function(df, y_name) {
  
  # Create bar charts for each variable
  n_points <- sum(!is.na(df[[y_name]]))
  
  p <- ggplot(df, aes(x = .data[[y_name]])) +
    geom_bar(fill = "blue", alpha = 0.5) +
    labs(y = "Count") +
    annotate("text", x = Inf, y = Inf, label = paste("n =", n_points), 
             hjust = 1.1, vjust = 1.5, size = 3, color = "black")
  
  return(p)
}

# Function to create correlation plot
af_plot_correlation <- function(df, vars = NULL, title = "Correlation Plot") {
  
  if(is.null(vars)) vars <- names(df)
  
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
  ggcorrplot::ggcorrplot(corr_matrix, 
                         method = "circle", 
                         type = "lower", 
                         lab = TRUE,              
                         lab_size = 3, 
                         colors = viridis::viridis(3, begin=0.4, end = 1),
                         title = title)
}

af_plot_correlation_per_group<- function(df, group_var, group_values = NULL, vars = NULL) {
  
  if (is.null(vars)) vars <- names(df)
  if (is.null(group_values)) group_values <- unique(df[[group_var]])
  
  # Initialize list for storing plots
  plot_list <- list()
  plot_counter <- 1
  
  for (g in group_values){
    dfw <- df %>% filter (.data[[group_var]] == g)
    title = paste(group_var, "=", g)
    p <- af_plot_correlation(dfw, vars, title)
    plot_list[[plot_counter]] <- p
    plot_counter <- plot_counter + 1
  }
  
  return(plot_list)
}

af_plot_distribution_per_group<- function(df, y_name, group_var, group_values = NULL) {
  
  if (is.null(group_values)) group_values <- unique(df[[group_var]])
  
  # Initialize list for storing plots
  dist_plots <- list()
  plot_counter <- 1
  
  for (g in group_values){
    dfw <- df %>% filter (.data[[group_var]] == g)
    p <- af_plot_distribution(dfw, y_name, title = paste(group_var, "=", g))
    dist_plots[[plot_counter]] <- p
    plot_counter <- plot_counter + 1
  }
  
  return(dist_plots)
}

# Define the function to arrange plots in a grid
af_combine_plots <- function(plot_list, ncol = 2) {
  # Combine all plots in a 2-column layout
  combined_plot <- gridExtra::grid.arrange(grobs = plot_list, ncol = ncol,
                                           widths = c(1,1), #Equal column width
                                           padding = unit(0.1, "cm") # Reduce space between plots
  )
  return(invisible(combined_plot))
}

af_ggsave <- function(filename, plot=last_plot(), 
                      width=180, height=90, units="mm",dpi=600, display=TRUE) {
  
  if (display) print(plot)
  
  ggsave(filename, plot = plot,
         width = width,height = height,units = units,dpi = dpi)
}


