recov_stretch_plot <- function(true, infer, plot_lab, palette_name, color_index, plot_title = NULL, num_points = NULL) {
  # Load required libraries
  library(ggplot2)
  library(RColorBrewer)
  library(ggExtra)  # For marginal plots
  library(dplyr)
  
  # Get the colors from the specified palette
  plot_col <- brewer.pal(n = 8, name = palette_name)[color_index]
  
  # Create the data frame
  df <- data.frame(true, infer)
  
  # Optionally sample 'num_points' from the data
  if (!is.null(num_points)) {
    set.seed(123)  # For reproducibility
    df <- df %>%
      sample_n(min(num_points, n()))  # Sample a specified number of points
  }
  
  # Calculate axis limits
  axis_range <- range(c(true, infer))  # Combine both true and infer values
  axis_limits <- c(floor(axis_range[1]), ceiling(axis_range[2]))  # Ensure limits are rounded
  
  # Create the main scatter plot with crosses
  scatter_plot <- ggplot(df, aes(x = true, y = infer)) +
    geom_point(shape = 4, color = plot_col) +            # Crosses and assigned color
    geom_smooth(method = "lm", se = TRUE, formula = y ~ x, color = plot_col) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
    theme_minimal() +
    xlab(plot_lab[1]) +
    ylab(plot_lab[2]) +
    coord_fixed(ratio = 1) +                             # Ensure equal aspect ratio
    scale_x_continuous(limits = axis_limits) +           # Apply axis limits
    scale_y_continuous(limits = axis_limits) +           # Apply axis limits
    ggtitle(ifelse(is.null(plot_title), 
                   paste0("'", plot_lab[2], "' compared to '", plot_lab[1], "'"), 
                   plot_title))  # Use custom title if provided
  
  # Add marginal density plots
  final_plot <- ggMarginal(scatter_plot, 
                           type = "density", 
                           fill = "black",  # Overlay black for visibility
                           alpha = 0.2,      # Set transparency of the marginal plots
                           color = "black")  # Black outline for clarity
  
  return(final_plot)
}
