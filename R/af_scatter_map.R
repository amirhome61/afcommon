#' Create a Labeled Scatter Plot Map
#'
#' @description Creates a scatter plot from a dataset where each point is labeled with its identity.
#' The function supports optional coloring by group and adding trend lines.
#'
#' @param df A data frame containing the variables to plot
#' @param identity_var_name Character string specifying the column name for point labels
#' @param x_var_name Character string specifying the column name for x-axis values
#' @param y_var_name Character string specifying the column name for y-axis values
#' @param color_var_name Character string specifying the column name for coloring points, or NULL for default black coloring
#' @param line_type Character string specifying the type of trend line to add: "linear", "smooth", or NULL for no line
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' # Example with coloring and a linear trend line
#' af_scatter_map(df, "country_code", "HDI", "total_extremism", "region", "linear")
#' 
#' # Example with no coloring and no trend line
#' af_scatter_map(df, "country_code", "HDI", "total_extremism", NULL, NULL)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_text geom_smooth theme_minimal labs scale_color_viridis_d
#' @importFrom rlang sym
af_scatter_map <- function(df, identity_var_name, x_var_name, y_var_name, 
                           color_var_name = NULL, line_type = NULL) {
  # Input validation
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }
  
  if (!identity_var_name %in% names(df)) {
    stop("identity_var_name column not found in data frame")
  }
  
  if (!x_var_name %in% names(df)) {
    stop("x_var_name column not found in data frame")
  }
  
  if (!y_var_name %in% names(df)) {
    stop("y_var_name column not found in data frame")
  }
  
  if (!is.null(color_var_name) && !color_var_name %in% names(df)) {
    stop("color_var_name column not found in data frame")
  }
  
  if (!is.null(line_type) && !line_type %in% c("linear", "smooth")) {
    stop("line_type must be NULL, 'linear', or 'smooth'")
  }
  
  # Ensure identity column is a factor or character
  if (!is.factor(df[[identity_var_name]]) && !is.character(df[[identity_var_name]])) {
    warning("Converting identity_var_name to character")
    df[[identity_var_name]] <- as.character(df[[identity_var_name]])
  }
  
  # Set up the base plot
  if (is.null(color_var_name)) {
    p <- ggplot(df, aes(x = !!sym(x_var_name), 
                        y = !!sym(y_var_name))) +
      geom_point(size = 3, color = "black")
  } else {
    # Ensure color variable is a factor or character
    if (!is.factor(df[[color_var_name]]) && !is.character(df[[color_var_name]])) {
      warning("Converting color_var_name to factor")
      df[[color_var_name]] <- as.factor(df[[color_var_name]])
    }
    
    p <- ggplot(df, aes(x = !!sym(x_var_name), 
                        y = !!sym(y_var_name),
                        color = !!sym(color_var_name))) +
      geom_point(size = 3) +
      scale_color_viridis_d(option = "plasma")
  }
  
  # Add identity labels
  p <- p + geom_text(aes(label = !!sym(identity_var_name)), 
                     hjust = -0.3, vjust = 0.5, size = 3)
  
  # Add trend line if specified
  if (!is.null(line_type)) {
    if (line_type == "linear") {
      p <- p + geom_smooth(method = "lm", formula = y ~ x, 
                           se = TRUE, color = "darkgrey")
    } else if (line_type == "smooth") {
      p <- p + geom_smooth(method = "loess", formula = y ~ x, 
                           se = TRUE, color = "darkgrey")
    }
  }
  
  # Finalize the plot
  p <- p + theme_minimal() +
    labs(x = x_var_name, y = y_var_name) +
    theme(legend.position = "right",
          panel.grid.minor = element_line(color = "lightgrey", linetype = "dashed"),
          panel.grid.major = element_line(color = "lightgrey"),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          axis.title = element_text(size = 12, face = "bold"))
  
  return(p)
}
