recov_plot <- function(true, infer, plot_lab, palette_name, color_index, plot_title = NULL, num_points = NULL) {
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
  axis_range <- range(c(df$true, df$infer))  # Combine both true and infer values
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
    scale_x_continuous(limits = axis_limits) +
    scale_y_continuous(limits = axis_limits) +
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


recov_plot_vector_matrix <- function(true, infer, plot_lab, plot_title = NULL, num_points = 50) {
  # Load required libraries
  library(ggplot2)
  library(ggExtra)  # For marginal plots
  
  # Check dimensions for valid input
  if (length(true) != nrow(infer)) {
    stop("Length of 'true' must match the number of rows in 'infer'")
  }
  
  # Sample the first `num_points` data points from each model
  set.seed(123)  # Ensure reproducibility of the sampling
  sampled_indices <- sample(1:length(true), num_points, replace = FALSE)  # Randomly sample indices
  
  # Create a data frame in long format for ggplot
  df <- data.frame(true = rep(true[sampled_indices], ncol(infer)), 
                   infer = as.vector(infer[sampled_indices, ]), 
                   Model = rep(1:ncol(infer), each = num_points))  # Model column for grouping
  
  # Change factor levels to AI_1, AI_2, etc.
  df$Model <- factor(df$Model, labels = paste("AI", 1:ncol(infer), sep = "_"))
  
  # Calculate axis limits
  axis_range <- range(c(true, as.vector(infer)))  # Combine both true and infer values
  axis_limits <- c(floor(axis_range[1]), ceiling(axis_range[2]))  # Ensure limits are rounded
  
  # Create the main scatter plot with crosses
  scatter_plot <- ggplot(df, aes(x = true, y = infer, color = Model)) +
    geom_point(alpha = 0.2) +  # Points with transparency for better visualization
    geom_smooth(method = "lm", se = TRUE, formula = y ~ x, aes(color = Model), 
                alpha = 0.3) +  # Tinted confidence region with model color
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
    theme_minimal() +
    xlab(plot_lab[1]) +
    ylab(plot_lab[2]) +
    coord_fixed(ratio = 1) +                             # Ensure equal aspect ratio
    scale_x_continuous(limits = axis_limits) +
    scale_y_continuous(limits = axis_limits) +
    ggtitle(ifelse(is.null(plot_title), 
                   paste0("'", plot_lab[2], "' compared to '", plot_lab[1], "'"), 
                   plot_title)) +  # Use custom title if provided
    scale_color_manual(values = c("red", "blue", "green", "purple", "orange"))
  
  # Add marginal density plots
  final_plot <- ggMarginal(scatter_plot, 
                           type = "density", 
                           fill = "black",  # Overlay black for visibility
                           alpha = 0.2,      # Set transparency of the marginal plots
                           color = "black")  # Black outline for clarity
  
  return(final_plot)
}




recov_plot_long <- function(true, infer, plot_lab, plot_title = NULL, num_points = 1600) {
  # Load necessary libraries
  library(ggplot2)
  library(ggExtra)  # For adding marginal plots
  num_cols <- length(true[1,])
  # Step 1: Convert matrices into data frames
  true_df <- data.frame(true)  
  infer_df <- data.frame(infer)  
  
  # Step 2: Rename columns to represent the AI model names
  colnames(true_df) <- paste("AI", 1:num_cols, sep = "_")
  colnames(infer_df) <- paste("AI", 1:num_cols, sep = "_")
  
  # Step 3: Melt each data frame to long format
  true_long <- melt(true_df, variable.name = "Model", value.name = "True")
  infer_long <- melt(infer_df, variable.name = "Model", value.name = "Infer")
  
  # Step 4: Combine the long-format data frames
  df <- data.frame(Model = true_long$Model, True = true_long$True, Inferred = infer_long$Infer)
  
  
  
  
  # Optionally sample 'num_points' from each AI model
  set.seed(123)  # For reproducibility, set the seed (optional)
  df_sampled <- df %>%
    dplyr::group_by(Model) %>%
    dplyr::sample_n(min(num_points, n()))  # Sample a specified number of points per model
  
  # Calculate axis limits based on data range
  axis_range <- range(c(df_sampled$True, df_sampled$Inferred))  # Combine true and inferred values
  axis_limits <- c(floor(axis_range[1]), ceiling(axis_range[2]))  # Ensure rounded limits
  
  # Create the scatter plot with color grouping by Model
  scatter_plot <- ggplot(df_sampled, aes(x = True, y = Inferred, color = Model)) +
    geom_point(alpha = 0.2) +  # Points with transparency for better visualization
    geom_smooth(method = "lm", se = TRUE, formula = y ~ x, aes(color = Model), 
                alpha = 0.3) +  # Tinted confidence region with model color
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Diagonal reference line
    theme_minimal() +
    xlab(plot_lab[1]) +
    ylab(plot_lab[2]) +
    coord_fixed(ratio = 1) +  # Equal aspect ratio for both axes
    scale_x_continuous(limits = axis_limits) +
    scale_y_continuous(limits = axis_limits) +
    ggtitle(ifelse(is.null(plot_title), 
                   paste0("'", plot_lab[2], "' vs '", plot_lab[1], "'"), 
                   plot_title)) +  # Use custom title if provided
    scale_color_manual(values = c("red", "blue", "green", "purple", "orange"))  # Custom colors
  
  # Add marginal density plots
  final_plot <- ggMarginal(scatter_plot, 
                           type = "density", 
                           fill = "black",  # Overlay black for visibility
                           alpha = 0.2,      # Set transparency of the marginal plots
                           color = "black")  # Black outline for clarity
  
  return(final_plot)
}


recov_plot_long_split <- function(true, infer, plot_lab, plot_title = NULL, num_points = 1600, model_colors = c("red", "blue", "green", "purple", "orange")) {
  # Load necessary libraries
  library(ggplot2)
  library(ggExtra)  # For adding marginal plots
  library(gridExtra)  # For arranging multiple plots
  library(dplyr)
  library(reshape2)  # For melt function
  
  
  # Step 1: Convert matrices into data frames
  true_df <- data.frame(true)  
  infer_df <- data.frame(infer)  
  
  # Step 2: Rename columns to represent the AI model names
  num_cols <- length(true[1,])
  colnames(true_df) <- paste("AI", 1:num_cols, sep = "_")
  colnames(infer_df) <- paste("AI", 1:num_cols, sep = "_")
  
  # Step 3: Melt each data frame to long format
  true_long <- melt(true_df, variable.name = "Model", value.name = "True")
  infer_long <- melt(infer_df, variable.name = "Model", value.name = "Infer")
  
  # Step 4: Combine the long-format data frames
  df <- data.frame(Model = true_long$Model, True = true_long$True, Inferred = infer_long$Infer)
  
  # Optionally sample 'num_points' from each AI model
  set.seed(123)  # For reproducibility, set the seed (optional)
  df_sampled <- df %>%
    dplyr::group_by(Model) %>%
    dplyr::sample_n(min(num_points, n()))  # Sample a specified number of points per model
  
  # Calculate axis limits based on data range
  axis_range <- range(c(df_sampled$True, df_sampled$Inferred))  # Combine true and inferred values
  axis_limits <- c(floor(axis_range[1]), ceiling(axis_range[2]))  # Ensure rounded limits
  
  
  # Create individual scatter plots for each model
  plot_list <- lapply(unique(df_sampled$Model), function(model_name) {
    model_df <- df_sampled[df_sampled$Model == model_name, ]
    color <- model_colors[which(unique(df_sampled$Model) == model_name)]
    
    # Create the scatter plot for the current model
    scatter_plot <- ggplot(model_df, aes(x = True, y = Inferred)) +
      geom_point(alpha = 0.2, color = color) +  # Points with transparency for better visualization
      geom_smooth(method = "lm", se = TRUE, formula = y ~ x, 
                  alpha = 0.3, color = "black", fill = "black") +  # Confidence region in same color as points
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Diagonal reference line
      theme_minimal() +
      xlab(plot_lab[1]) +
      ylab(plot_lab[2]) +
      coord_fixed(ratio = 1) +  # Equal aspect ratio for both axes
      scale_x_continuous(limits = axis_limits) +
      scale_y_continuous(limits = axis_limits) +
      ggtitle(paste("Model:", model_name)) +
      scale_color_manual(values = c(color))  # Set color for points and smooth line
    
    # Add marginal plots with density for better visibility
    ggMarginal(scatter_plot, 
               type = "density", 
               fill = "black",  # Overlay black for visibility
               alpha = 0.2,      # Set transparency of the marginal plots
               color = "black")  # Black outline for clarity
  })
  
  # Combine all individual plots into one using gridExtra::grid.arrange
  final_plot <- do.call(grid.arrange, c(plot_list))  # Arrange the plots in 2 columns
  
  return(final_plot)
}


recov_plot_long_unsplit <- function(true, infer, plot_lab, palette_name, color_index, plot_title = NULL, num_points = NULL) {
  # Load required libraries
  library(ggplot2)
  library(RColorBrewer)
  library(ggExtra)  # For marginal plots
  library(dplyr)
  library(tidyr)    # For pivot_longer
  
  # Convert the matrices to data frames
  true_df <- data.frame(true)  
  infer_df <- data.frame(infer)  
  
  # Step 2: Rename columns to represent the AI model names
  num_cols <- length(true[1,])
  colnames(true_df) <- paste("AI", 1:num_cols, sep = "_")
  colnames(infer_df) <- paste("AI", 1:num_cols, sep = "_")
  
  # Step 3: Pivot each data frame to long format using pivot_longer from tidyr
  true_long <- true_df %>%
    pivot_longer(cols = everything(), names_to = "Model", values_to = "True")
  infer_long <- infer_df %>%
    pivot_longer(cols = everything(), names_to = "Model", values_to = "Infer")
  
  # Step 4: Combine the long-format data frames
  df <- data.frame(True = true_long$True, Inferred = infer_long$Infer)
  
  # Get the colors from the specified palette
  plot_col <- brewer.pal(n = 8, name = palette_name)[color_index]
  
  # Optionally sample 'num_points' from the data
  if (!is.null(num_points)) {
    set.seed(123)  # For reproducibility
    df <- df %>%
      sample_n(min(num_points, n()))  # Sample a specified number of points
  }
  
  # Calculate axis limits
  axis_range <- range(c(df$True, df$Inferred))  # Combine both true and infer values
  axis_limits <- c(floor(axis_range[1]), ceiling(axis_range[2]))  # Ensure limits are rounded
  
  # Create the main scatter plot with crosses
  scatter_plot <- ggplot(df, aes(x = True, y = Inferred)) +
    geom_point(color = plot_col, alpha = 0.2 )+            # Crosses and assigned color
    geom_smooth(method = "lm", se = TRUE, formula = y ~ x, color = "black") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
    theme_minimal() +
    xlab(plot_lab[1]) +
    ylab(plot_lab[2]) +
    coord_fixed(ratio = 1) +                             # Ensure equal aspect ratio
    scale_x_continuous(limits = axis_limits) +
    scale_y_continuous(limits = axis_limits) +
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





recov_plot_color <- function(true, infer, plot_lab, color, plot_title = NULL, num_points = NULL) {
  # Load required libraries
  library(ggplot2)
  library(RColorBrewer)
  library(ggExtra)  # For marginal plots
  library(dplyr)
  
  # Get the colors from the specified palette
  plot_col <- color
  
  # Create the data frame
  df <- data.frame(true, infer)
  
  # Optionally sample 'num_points' from the data
  if (!is.null(num_points)) {
    set.seed(123)  # For reproducibility
    df <- df %>%
      sample_n(min(num_points, n()))  # Sample a specified number of points
  }
  
  # Calculate axis limits
  axis_range <- range(c(df$true, df$infer))  # Combine both true and infer values
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
    scale_x_continuous(limits = axis_limits) +
    scale_y_continuous(limits = axis_limits) +
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



