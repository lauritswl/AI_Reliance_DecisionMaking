##### 3PLM model is defined as:   #####

## P(X_i[o] = z[o]) = \eta + (1-\eta)*\theta

# Corrects for chance level by introducing a guessing parameter

#####-----------------------------#####
#            1. Setup                 #
#####-----------------------------#####
### Install Packages: ###
pacman::p_load(tidyverse, truncnorm, R2jags, parallel, ggpubr, extraDistr, RColorBrewer)

### Import and define functions: ###
source("AI_Reliance_DecisionMaking/Scripts/Simulate_3PLM.R")
source("AI_Reliance_DecisionMaking/Scripts/recov_plot.R")

### Maximum of posterior density ###
MPD <- function(x) {
  d <- density(x)
  d$x[which.max(d$y)]}

### Sample posterior: ###
SampleDist <- function(x) {
  ### Maximum of posterior density ###
  sample(x = x, size = 1)
}


### Set a seed for replication ###
set.seed(88778)


#####-----------------------------#####
#      2. Simulate and wrangle        #
#####-----------------------------#####
### Define variables for simulation: ###


NN <- 5 # Levels of noise-filters to apply to the images
NL <- 16 # Different images for classification
NS <- 70 # Number of participants
NI <- NN*NL
NO <- NI*NS

### Simulate data with sourced function: ###
data <- sim_model(nsub = NS, nNoise = NN, L = NL)




### Create data list for JAGS ###
jags_data <- list(
  NO = NO,                             # Number of instances in advice off condition (NI * NS)
  NS = NS,                             # Number of participants    
  NI = NI,                             # Number of items (NN * NL)
  NN = NN,                             # Number of noise categories
  NL = NL,                             # Number of possible labels
  jid = data$jid,                      # Participant index for observation o
  iid = data$iid,                      # Item index for observation o
  z = data$z,                          # True label for observation o
  lbl = data$lbl                      # Human predictions for observation o
)

### Key Latent Parameters returned by model: ###
params<-c(
  "a",           # a[i]        ability participant i 
  "s",           # s[j]        discrimination parameter item j
  "d",           # d[j]        difficulty item j
  "eta",         # eta[j]      chance level of guessing item j
  "theta",       # theta[o]    human skill of predicting observation o
  "prob_z",      # prob_z[o]   prop of correct observation o
  "pst_lbl")     # pst_lbl[o]  sampled distribution from posterior lbl choice distribution


### Specify the path to your model file ###
model_file <- "AI_Reliance_DecisionMaking/Models/IRT_3PLM.txt"


#####-----------------------------#####
#     3. Compile and run JAGS         #
#####-----------------------------#####
# start timer
start_time <- proc.time()
print("Starting sampling:")
# run code
samples <- jags.parallel(
  data = jags_data, 
  inits=NULL,
  parameters.to.save = params,
  jags.seed = 1234,
  model.file = model_file, 
  n.chains= 3, n.iter=3000, n.burnin=1000, n.thin=1)

# stop timer and print time
total_time_string <- paste("Sampling done after:", (proc.time() - start_time)[3], "seconds." ) 
print(total_time_string)

#####-----------------------------#####
#      3. Plots and analysis          #
#####-----------------------------#####


### Save true parameters in arrays ###
index_participants <- seq(1, nrow(data), by = NI) #Index every 100th observation
true_a <-      data$a[index_participants] 
true_s <-      data$s[1:NI]
true_d <-      data$d[1:NI]
true_eta <-    data$eta[1:NI]
true_theta <-  data$theta
true_lbl <-    data$z
true_odds <-   data$prob_z


### Save Maximums of posterior densitites in arrays ###
Y <- samples$BUGSoutput$sims.list
infer_a <- apply(Y$a, 2, MPD)
infer_s <- apply(Y$s, 2, MPD)
infer_d <- apply(Y$d, 2, MPD)
infer_eta <- apply(Y$eta, 2, MPD)
infer_theta <- apply(Y$theta, 2, MPD)
infer_lbl <- apply(Y$pst_lbl, 2, SampleDist)
infer_odds <- apply(Y$prob_z, 2, MPD)


# Combine true and inferred parameters into a single data frame
sample_indices <- sample(seq_along(true_theta), round(sqrt(length(jid))))  # Random indices for sampling

sample_dataframe <- data.frame(
  Participant_Index = seq_len(length(sample_indices)),
  True_Ability = true_a[iid[sample_indices]],
  Inferred_Ability = infer_a[iid[sample_indices]],
  True_Discrimination = true_s[jid[sample_indices]],
  Inferred_Discrimination = infer_s[jid[sample_indices]],
  True_Difficulty = true_d[jid[sample_indices]],
  Inferred_Difficulty = infer_d[jid[sample_indices]],
  True_Eta = true_eta[jid[sample_indices]],
  Inferred_Eta = infer_eta[jid[sample_indices]],
  True_Theta = true_theta[sample_indices],
  Inferred_Theta = infer_theta[sample_indices],
  True_Label = true_lbl[sample_indices],
  Inferred_Label = infer_lbl[sample_indices],
  True_Odds = true_odds[sample_indices],
  Inferred_Odds = infer_odds[sample_indices]
)

### -------------------------------------------------------- ###
##                      Parameter recovery:                   ##
##            For Theta and underlying psychometrics          ##
### -------------------------------------------------------- ###

pl1 <- recov_plot(
  true = sample_dataframe$True_Ability,
  infer = sample_dataframe$Inferred_Ability,
  plot_lab = c('True Ability', 'Inferred Ability'),
  palette_name = 'Set1',
  color_index = 1
)
pl2 <- recov_plot(
  true = sample_dataframe$True_Discrimination,
  infer = sample_dataframe$Inferred_Discrimination,
  plot_lab = c('True Discrimination', 'Inferred Discrimination'),
  palette_name = 'Set1',
  color_index = 2
)
pl3 <- recov_plot(
  true = sample_dataframe$True_Difficulty,
  infer = sample_dataframe$Inferred_Difficulty,
  plot_lab = c('True Difficulty', 'Inferred Difficulty'),
  palette_name = 'Set1',
  color_index = 3
)
pl4 <- recov_plot(
  true = sample_dataframe$True_Theta,
  infer = sample_dataframe$Inferred_Theta,
  plot_lab = c('True Theta', 'Inferred Theta'),
  palette_name = 'Set1',
  color_index = 4
)
pl5 <- recov_plot(
  true = sample_dataframe$True_Eta,
  infer = sample_dataframe$Inferred_Eta,
  plot_lab = c('True Eta', 'Inferred Eta'),
  palette_name = 'Set1',
  color_index = 5
)
pl6 <- recov_plot(
  true = sample_dataframe$True_Odds,
  infer = sample_dataframe$Inferred_Odds,
  plot_lab = c('True Odds', 'Inferred Odds'),
  palette_name = 'Set1',
  color_index = 7
)

combined_recovery <- ggarrange(pl1, pl2, pl3, pl4, pl5, pl6, ncol = 3, nrow = 2)
ggsave(
  "AI_Reliance_DecisionMaking/Plots/3PLM/parameter_recovery_1500x1000_samples.png",
  plot = combined_recovery,
  width = 21,
  height = 14,
  units = "in",
  dpi = 300
)


### -------------------------------------------------------- ###
##                    Predictive Accuracy:                    ##
##            From sampled labels from posterior              ##
### -------------------------------------------------------- ###
### CREATE CORRECT PREDITIONS PLOT ###
# Create a confusion matrix
conf_matrix <- table(true_lbl, infer_lbl)
correct <- diag(conf_matrix)

# Create a data frame for plotting
correct_df <- data.frame(Label = factor(names(correct), levels = 1:NL), Count = as.numeric(as.numeric(correct)/table(true_lbl)))

# Plot correct predictions
custom_blues <- colorRampPalette(c("lightblue", "#00468b"))(NL)
correct_plt <- ggplot(correct_df, aes(x = Label, y = Count, fill = Label)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = custom_blues, name = "Category") +
  labs(
    title = "Correct Predictions",
    x = "Category",
    y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### CREATE INCORRECT PREDICTIONS PLOT ###
# Subset the confusion matrix to exclude diagonal (incorrect predictions)
incorrect_matrix <- conf_matrix
diag(incorrect_matrix) <- -1

# Convert the incorrect matrix to a data frame for plotting
incorrect_df <- as.data.frame(as.table(incorrect_matrix))

# Remove zero entries for clarity
incorrect_df <- incorrect_df[incorrect_df$Freq >= 0, ]

# Plot incorrect predictions as a heatmap
incorrect_plt <- ggplot(incorrect_df, aes(x = infer_lbl, y = true_lbl, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Count") +
  labs(
    title = "Incorrect Predictions",
    x = "Inferred Label",
    y = "True Label"
  ) +
  theme_minimal()



# Create theoretical accuracy as a function of binned odds
accuracy_by_odds <- data.frame(
  Inferred_Odds = infer_theta,
  Predicted_Correct = (infer_lbl == true_lbl),
  Actual_Theta = true_theta) %>%
  mutate(Binned_Odds = cut(Inferred_Odds, breaks = seq(0, 1, by = 0.05), include.lowest = TRUE)) %>%
  group_by(Binned_Odds) %>%
  summarize(
    Accuracy = mean(Predicted_Correct) * 100, 
    Count = n(),
    True_Theta = mean(Actual_Theta)*100
  )

theoretical_accuracy <- data.frame(
  Binned_Odds = levels(accuracy_by_odds$Binned_Odds),  # Same bins as accuracy_by_odds
  Theoretical_Accuracy = seq(5, 95, length.out = length(levels(accuracy_by_odds$Binned_Odds))) # Replace with your function or values
)
percentage_correct <- ggplot(accuracy_by_odds, aes(x = Binned_Odds)) +
  geom_bar(aes(y = Accuracy), stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_point(data = theoretical_accuracy, aes(x = Binned_Odds, y = Theoretical_Accuracy, color = "Theoretical Accuracy"), 
             size = 2) +
  geom_line(data = theoretical_accuracy, aes(x = Binned_Odds, y = Theoretical_Accuracy, group = 1, color = "Theoretical Accuracy"), 
            linetype = "dashed") +
  geom_point(data = accuracy_by_odds, aes(x = Binned_Odds, y = True_Theta, color = "True Theta"), 
             size = 2) +
  geom_line(data = accuracy_by_odds, aes(x = Binned_Odds, y = True_Theta, group = 1, color = "True Theta"), 
            linetype = "dashed") +
  labs(
    title = "Percentage of Correct Predictions by Binned Inferred Odds",
    x = "Binned Inferred Theta",
    y = "Percentage Correct"
  ) +
  scale_color_manual(
    values = c("Theoretical Accuracy" = "red", "True Theta" = "orange")
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.title = element_blank(), legend.position = "top")


### PLOT ALL THREE PLOTS ###
accuracy <- ggarrange(plotlist = list(correct_plt, incorrect_plt, percentage_correct), nrow = 1)
ggsave("AI_Reliance_DecisionMaking/Plots/3PLM/accuracy_21x7.png", plot = accuracy, width = 21, height = 7, units = "in", dpi = 300)





### -------------------------------------------------------- ###
##                      Parameter recovery:                   ##
##            Correlation between theta, eta and prob         ##
### -------------------------------------------------------- ###


source("AI_Reliance_DecisionMaking/Scripts/recov_stretch_plot.R")
 

pl7 <- recov_stretch_plot(
  true = sample_dataframe$True_Eta,
  infer = sample_dataframe$True_Theta,
  plot_lab = c('True Eta', 'True Theta'),
  palette_name = 'Set1',
  color_index = 1
)
pl8 <- recov_stretch_plot(
  true = sample_dataframe$True_Theta,
  infer = sample_dataframe$True_Odds,
  plot_lab = c('True Theta', 'True Odds'),
  palette_name = 'Set1',
  color_index = 2
)
pl9 <- recov_stretch_plot(
  true = sample_dataframe$True_Eta,
  infer = sample_dataframe$True_Odds,
  plot_lab = c('True Eta', 'True Odds'),
  palette_name = 'Set1',
  color_index = 3
)

# Pairwise relationships
pl10 <- recov_stretch_plot(
  true = sample_dataframe$Inferred_Theta,
  infer = sample_dataframe$Inferred_Eta,
  plot_lab = c('Inferred Theta', 'Inferred Eta'),
  palette_name = 'Set1',
  color_index = 4
)

pl11 <- recov_stretch_plot(
  true = sample_dataframe$Inferred_Theta,
  infer = sample_dataframe$Inferred_Odds,
  plot_lab = c('Inferred Theta', 'Inferred Odds'),
  palette_name = 'Set1',
  color_index = 5
)
pl12 <- recov_stretch_plot(
  true = sample_dataframe$Inferred_Eta,
  infer = sample_dataframe$Inferred_Odds,
  plot_lab = c('Inferred Eta', 'Inferred Odds'),
  palette_name = 'Set1',
  color_index = 7
)


combined_relation <- ggarrange(pl7, pl8, pl9, pl10, pl11, pl12, ncol = 3, nrow = 2)
ggsave(
  "AI_Reliance_DecisionMaking/Plots/3PLM/odds_theta_eta_relation_1500x1000_samples.png",
  plot = combined_relation,
  width = 21,
  height = 14,
  units = "in",
  dpi = 300
)


save_posterior <- saveRDS(object = samples, file = "AI_Reliance_DecisionMaking/Models/IRT_3PLM_output.Rdata")
















