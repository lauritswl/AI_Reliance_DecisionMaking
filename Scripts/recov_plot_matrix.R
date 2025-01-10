recov_plot_matrix <- function(true, infer, plot_lab, AI_Thetas) {
  # Load required libraries
  # library(ggplot2)
  
  # Check dimensions of input matrices
  if (!all(dim(true) == dim(infer))) {
    stop("The dimensions of 'true' and 'infer' must match.")
  }
  
  # Define a color gradient based on AI_Thetas
  gradient_colors <- scales::seq_gradient_pal("blue", "red", "Lab")(AI_Thetas)
  
  # Prepare data for plotting
  NAI <- ncol(true)  # Number of models
  df <- data.frame(
    true = as.vector(true),
    infer = as.vector(infer),
    model = rep(1:NAI, each = nrow(true)),  # Model index
    color = rep(gradient_colors, each = nrow(true))  # Gradient color for each model
  )
  
  # Create the plot
  axis_range <- range(c(true, infer))  # Combine all values for axis range
  axis_limits <- c(floor(axis_range[1]), ceiling(axis_range[2]))  # Rounded limits
  
  pl <- ggplot(df, aes(x = true, y = infer, color = factor(model))) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, formula = y ~ x, aes(group = model)) +
    scale_color_manual(values = gradient_colors, guide = "none") +  # Use gradient colors
    theme_minimal() +
    xlab(plot_lab[1]) +
    ylab(plot_lab[2]) +
    coord_fixed(ratio = 1) +  # Equal aspect ratio
    scale_x_continuous(limits = axis_limits) +
    scale_y_continuous(limits = axis_limits) +
    ggtitle(paste0("'", plot_lab[2], "' compared to '", plot_lab[1], "'")) +
    theme(legend.position = "none")  # Hide legend
  
  return(pl)
}

recov_plot_ggarrange <- function(true, infer, plot_lab, AI_Thetas) {
  # Load required libraries
  library(ggpubr)  # For ggarange
  
  # Check dimensions of input matrices
  if (!all(dim(true) == dim(infer))) {
    stop("The dimensions of 'true' and 'infer' must match.")
  }
  
  # Define a color gradient based on AI_Thetas
  gradient_colors <- scales::seq_gradient_pal("blue", "red", "Lab")(AI_Thetas)
  
  # Prepare individual plots for each model
  NAI <- ncol(true)  # Number of models
  plots <- vector("list", NAI)  # Initialize list to store plots
  
  for (model in 1:NAI) {
    # Create data frame for the current model
    df <- data.frame(true = true[, model], infer = infer[, model])
    
    # Calculate axis limits for consistency
    axis_range <- range(c(true, infer))  # Combine all values for axis range
    axis_limits <- c(floor(axis_range[1]), ceiling(axis_range[2]))  # Rounded limits
    
    # Create the plot for the current model
    plots[[model]] <- ggplot(df, aes(x = true, y = infer)) +
      geom_point(color = gradient_colors[model]) +
      geom_smooth(method = "lm", se = TRUE, formula = y ~ x, color = gradient_colors[model]) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black")+
      theme_minimal() +
      xlab(plot_lab[1]) +
      ylab(plot_lab[2]) +
      coord_fixed(ratio = 1) +  # Equal aspect ratio
      scale_x_continuous(limits = axis_limits) +
      scale_y_continuous(limits = axis_limits) +
      ggtitle(paste0("Model ", model, " (AI Theta = ", round(AI_Thetas[model], 2), ")"))
  }
  
  # Combine all plots using ggarrange
  combined_plot <- ggarrange(plotlist = plots, ncol = 2, nrow = ceiling(NAI / 2))
  
  return(combined_plot)
}
