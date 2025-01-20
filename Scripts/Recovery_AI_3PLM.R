#take argument from bash
#arg <- commandArgs(TRUE)
#make it an integer
#seed <- as.integer(arg)
seed <- 7788
set.seed(seed)


print(seed)
##### 3PLM model is defined as:   #####

## P(X_i[o] = lbl[o]) = \theta[o] + (1-\theta[o])*\alpha[o]

# Corrects for chance level by introducing a guessing parameter

#####-----------------------------#####
#            1. Setup                 #
#####-----------------------------#####
### Install Packages: ###
pacman::p_load(tidyverse, truncnorm, R2jags, parallel, ggpubr, ggExtra, extraDistr, RColorBrewer)

### Import and define functions: ###
source("AI_Reliance_DecisionMaking/Scripts/Simulate_AI_3PLM.R")
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


#####-----------------------------#####
#      2. Simulate and wrangle        #
#####-----------------------------#####
### Define variables for simulation: ###


NN <- 5 # Levels of noise-filters to apply to the images
NL <- 8 # Different images for classification
NS <- 40 # Number of participants
NI <- NN*NL
NO <- NI*NS
NAI <- 5
### Simulate data with sourced function: ###
data <- sim_model(nsub = NS, nNoise = NN, L = NL, nAI = NAI)


jid = data$jid
iid = data$iid
AI_IDX <- 3
### Create data list for JAGS ###
jags_data <- list(
  NO = NO,                             # Number of instances in advice off condition (NI * NS)
  NS = NS,                             # Number of participants    
  NI = NI,                             # Number of items (NN * NL)
  NN = NN,                             # Number of noise categories
  NL = NL,                             # Number of possible labels
  NAI = NAI,                           # Number of AI models, indexed by a
  jid = data$jid,                      # Participant index for observation o
  iid = data$iid,                      # Item index for observation o
  z = data$z,                          # True label for observation o
  ai_hum_lbl = as.matrix(data[(12+(NAI*1)):(11+2*NAI)]) # data[11+(NAI*1)+AI_IDX][,1],  
  )


### Key Latent Parameters returned by model: ###
params<-c(
  "a",           # a[i]        ability participant i 
  "alpha",       # alpha[i]    AI reliance participant i
  "alpha_sd",    # proxy StdDev of alpha
  "s",           # s[j]        discrimination parameter item j
  "d",           # d[j]        difficulty item j
  "theta",       # theta[o]    human skill of predicting observation o
  "ai_theta",    # ai_theta[o, ai] of ai
  "probs_correct",    # p_correct[o, ai] prob correct for model o
  "pst_ai_lbl",   # pst_ai_lbl[o,ai] sampled distribution from posterior ai assistance lbl choice distribution
  "ai_theta_mean",
  "ai_theta_std")




### Specif<- the path to your model file ###
# model_file <- "AI_Reliance_DecisionMaking/Models/IRT_AI_3PLM.txt"
# model_file <- "AI_Reliance_DecisionMaking/Models/IRT_SingleAI_3PLM.txt"
# model_file <- "AI_Reliance_DecisionMaking/Models/Simplified_IRT_3PLM.txt"
model_file <- "AI_Reliance_DecisionMaking/Models/Simplified_IRT_3PLM_noHum.txt"
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
Y <- samples$BUGSoutput$sims.list
save_posterior <- saveRDS(object = samples, file = "AI_Reliance_DecisionMaking/Models/Output_Simplified_5_hirar_onlyHUMAI.Rdata")
print("Saved the plot")
break
#####-----------------------------#####
#      3. Plots and analysis          #
#####-----------------------------#####


if (F){
  samples <- readRDS("AI_Reliance_DecisionMaking/Models/Output_Simplified_5_hirar_onlyHUMAI.Rdata")
  Y <- samples$BUGSoutput$sims.list
}


### Save true parameters in arrays ###
index_participants <- seq(1, nrow(data), by = NI) #Index every 100th observation
true_a <-      data$a[index_participants] 
true_alpha <-  data$alpha
true_s <-      data$s[1:NI]
true_d <-      data$d[1:NI]
true_ai_theta <- as.matrix(data[(12+3*NAI):(11+4*NAI)])
true_theta <-  data$theta
true_lbl <-    data$z
true_humai_odds <- true_theta*(1-true_alpha) +true_alpha*true_ai_theta
true_humai_lbl <- as.matrix(data[(12+NAI):(11+2*NAI)])





### Save Maximums of posterior densitites in arrays ###
Y <- samples$BUGSoutput$sims.list
infer_a <- apply(Y$a, 2, MPD)
#infer_alpha <- apply(Y$alpha, 2, MPD)
infer_s <- apply(Y$s, 2, MPD)
infer_d <- apply(Y$d, 2, MPD)
#infer_ai_theta <- apply(Y$ai_theta, 2, MPD)
infer_theta <-  pmin(pmax(apply(Y$theta, 2, MPD), 0.000001), 1)
infer_lbl <- apply(Y$pst_ai_lbl, 2, SampleDist)
infer_alpha_sd <- apply(Y$alpha_sd, 2, MPD)
# infer_humai_odds <-  infer_theta*(1-infer_alpha)+infer_alpha*infer_ai_theta #apply(Y$, c(2, 3), MPD)
# infer_humai_lbl <- apply(Y$pst_ai_lbl, c(2, 3), SampleDist)

infer_mean_ai_theta <- infer_a <- apply(Y$ai_theta_mean, 2, MPD)
infer_std_ai_theta <- infer_a <- apply(Y$ai_theta_std, 2, MPD)

infer_alpha <-  sapply(1:dim(Y$alpha)[3], function(layer) {apply(Y$alpha[,,layer], 2, MPD)})
infer_ai_theta <- sapply(1:dim(Y$ai_theta)[3], function(layer) {apply(Y$ai_theta[,,layer], 2, MPD)})
infer_humai_odds <-  infer_theta*(1-infer_alpha)+infer_alpha*infer_ai_theta
infer_humai_lbl <- sapply(1:dim(Y$pst_ai_lbl)[3], function(layer) {apply(Y$pst_ai_lbl[,,layer], 2, SampleDist)})


# Combine true and inferred parameters into a single data frame
sample_indices <- sample(seq_along(true_theta), round(sqrt(length(jid))))  # Random indices for sampling
AI_Thetas <- as.numeric(unlist(strsplit(data$ai_theta_string[1], ",")))


### -------------------------------------------------------- ###
##                      Parameter recovery:                   ##
##            For Theta and underlying psychometrics          ##
### -------------------------------------------------------- ###

pl1 <- recov_plot(
  true = true_a,
  infer =infer_a,
  plot_lab = c('True', 'Inferred'),
  palette_name = 'Set1',
  color_index = 1,
  plot_title = "All 40 Ability recoveries"
)
pl2 <- recov_plot(
  true = true_s,
  infer = infer_s,
  plot_lab = c('True', 'Inferred'),
  palette_name = 'Set1',
  color_index = 2,
  plot_title = "All 40 Discrimination recoveries"
)
pl3 <- recov_plot(
  true = true_d,
  infer = infer_d,
  plot_lab = c('True', 'Inferred'),
  palette_name = 'Set1',
  color_index = 3,
  plot_title = "All 40 Difficulty recoveries "
)
pl4 <- recov_plot(
  true = true_theta,
  infer = infer_theta,
  plot_lab = c('True', 'Inferred'),
  palette_name = 'Set1',
  color_index = 4,
  plot_title = "2500 samples of Theta",
  num_points = 2500
)
pl5 <- recov_plot_vector_matrix(
  true = true_alpha,
  infer = infer_alpha,
  plot_lab = c('True', 'Inferred'),
  plot_title = "500 Alpha samples for each model",
  num_points = 500
)

pl6 <- recov_plot_long(
  true = true_ai_theta, 
  infer = infer_ai_theta, 
  plot_lab = c('True AI Theta', 'Inferred AI Theta'),
  plot_title = "500 AI Theta samples for each model",
  num_points = 500
)



combined_recovery <- ggarrange(pl1, pl2, pl3, pl4, pl5, pl6, ncol = 3, nrow = 2)

ggsave(
  "AI_Reliance_DecisionMaking/Plots/AI_3PLM/5simple_parameter_recovery_1500x1000_samples.png",
  plot = combined_recovery,
  width = 21,
  height = 14,
  units = "in",
  dpi = 300
)
print("Saved Recovery_Plot: as 5simple_parameter")

ai_theta_split <- recov_plot_long_split(
  true = true_ai_theta, 
  infer = infer_ai_theta, 
  plot_lab = c('True AI Theta', 'Inferred AI Theta'),
  plot_title = "AI Theta Comparison",
  num_points = 500,
  model_colors = c("red", "blue", "green", "purple", "orange")
)
# Create AI Theta


source("AI_Reliance_DecisionMaking/Scripts/recov_plot_matrix.R")

plt_odds_1 <- recov_plot_long_unsplit(
  true = true_humai_odds, 
  infer = infer_humai_odds, 
  plot_lab = c('True AI Odds', 'Inferred AI Odds'),
  palette_name = 'Set1',
  color_index = 7,
  plot_title = "2500 samples of AI Odds recovery",
  num_points = 2500
)

alpha_recovery <- recov_plot_long_split(
  true =  matrix(rep(true_alpha, 5), ncol = 5, byrow = FALSE), 
  infer = infer_alpha, 
  plot_lab = c('True AI Odds', 'Inferred AI Odds'),
  plot_title = "500 samples of AI Odds recovery",
  num_points = 500,
  model_colors = c("red", "blue", "green", "purple", "orange")
)

recov_plot_color(
  true = true_alpha,
  infer = infer_alpha[,5],
  plot_lab = c('True', 'Inferred'),
  color = "orange",
  plot_title = " ",
  num_points = 500
)






######### BY AI MODEL:   ###########
### Save Maximums of posterior densitites in arrays ###
Y <- samples$BUGSoutput$sims.list
infer_alpha <- apply(Y$alpha, c(2, 3), MPD)
infer_ai_theta <- apply(Y$ai_theta, c(2, 3), MPD)
infer_theta <- apply(Y$theta, c(2), MPD)
infer_humai_odds <-  infer_theta*(1-infer_alpha)+infer_alpha*infer_ai_theta #apply(Y$, c(2, 3), MPD)
infer_humai_lbl <- apply(Y$pst_ai_lbl, c(2, 3), SampleDist)







# Reshape data for ggplot
df <- data.frame(true_theta, true_ai_theta)
colnames(df) <- c("infer_theta", paste0("AI_", 1:5)) # Rename columns
pacman::p_load(reshape2) # For reshaping the data
df_long <- melt(df, id.vars = "infer_theta", 
                variable.name = "Model", 
                value.name = "AI_Theta") # Convert to long format

# Plot with ggplot
ggplot(df_long, aes(x = infer_theta, y = AI_Theta, color = Model)) +
  geom_point(alpha = 0.2) +  # Add data points
  geom_smooth(method = "lm", se = TRUE) +  # Add regression lines
  labs(title = "Linear Models of actual AI Theta against Human Theta",
       x = "True Human Theta", y = "True AI Theta") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange")) +
  theme(legend.title = element_blank())



