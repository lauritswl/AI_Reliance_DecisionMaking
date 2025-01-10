recov_stretch_plot <- function(true, infer, plot_lab, palette_name, color_index) {
  # Load required libraries
  # library(ggplot2)
  # library(RColorBrewer)
  
  # Get the colors from the specified palette
  plot_col <- brewer.pal(n = 8, name = palette_name)[color_index]
  
  # Create the plot
  df <- data.frame(true, infer)
  
  # Calculate axis limits
  axis_range <- range(c(true, infer))  # Combine both true and infer values
  axis_limits <- c(floor(axis_range[1]), ceiling(axis_range[2]))  # Ensure limits are rounded
  
  pl <- ggplot(df, aes(x = true, y = infer)) +
    geom_point(color = plot_col) +            # Assigning color from Brewer palette
    geom_smooth(method = "lm", se = TRUE, formula = y ~ x, color = plot_col) +
    theme_minimal() +
    xlab(plot_lab[1]) +
    ylab(plot_lab[2]) +
    ggtitle(paste0("'", plot_lab[2], "' compared to '", plot_lab[1], "'"))
  
  return(pl)
}
