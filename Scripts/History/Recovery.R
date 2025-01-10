#####-----------------------------#####
#            1. Setup                 #
#####-----------------------------#####

# Simulate data for parameter recovery:

## Packages
pacman::p_load(tidyverse, truncnorm, R2jags, parallel, ggpubr, extraDistr)
# Importing simulation function and recovery plot
source("AI_Reliance_DecisionMaking/Scripts/Simulate.R")
source("AI_Reliance_DecisionMaking/Scripts/recov_plot.R")
# defining a function for calculating the maximum of the posterior density (not exactly the same as the mode)
MPD <- function(x) {
  d <- density(x)
  d$x[which.max(d$y)]
}
# Set a Seed
set.seed(8877)



# Generate data:
return_object <- sim_model(nsub = 100, nimg = 100, L = 16)
data <- return_object[["data"]]
parameters <- return_object[["parameters"]]


# Data
#   NO           Number of instances in advice off condition
#   NS           Number of participants    
#   NI           Number of images
#   L            Number of labels           
#   jid(o)       Item index for observation o
#   iid(o)       Participant index for observation o
#   z(o)         True label for obervervation o
#   lbl(o)       Label in the no-advice condition (Human Prediction)
#   r(o)         Confidence rating for observation o in advice off condition


# Create data list for JAGS
jags_data <- list(
  NO = nrow(data),                     # Number of observations
  NS = length(unique(data$uid)),       # Number of participants
  NI = length(unique(data$pid)),       # Number of items
  L = data$L[1], # Number of possible labels
  jid = data$uid,                      # Participant indices
  iid = data$pid,                      # Item indices
  z = data$true_label,                 # True labels
  lbl = data$y,                        # Human predictions
  r = data$conf_lvl_numeric            # Numeric confidence levels
)

# Key Latent Parameters
#    a[i]        ability participant i 
#    s[j]        discrimination parameter item j
#    d[j]        difficulty item j
#    v[i,1:2]    cutpoints participant i
#    theta[o]    prop correct for observation o
params<-c("a", "s", "d", "v1", "v2", "theta", "theta_noise", "r")

# Specify the path to your model file
# model_file <- "AI_Reliance_DecisionMaking/Models/IRT_model.txt"
model_file <- "AI_Reliance_DecisionMaking/Models/dummy_IRT.txt"


  ##### Run GIBS sampler: #####
# start timer
start_time <- proc.time()
print("Starting timer:")
# run code
samples <- jags(
  data = jags_data, 
  inits=NULL,
  parameters.to.save = params,
  jags.seed = 1234,
  model.file = model_file, 
  n.chains= 3, n.iter=3000, n.burnin=1000, n.thin=1)

# stop timer and print time
print((proc.time() - start_time)[3])



# Get ready to plot recovery
## Source recov_plot function



## prepare arrays:
### true arrays
index100 <- seq(1, nrow(parameters), by = 100) #Index every 100th observation
true_a <- parameters$a[index100] 
true_s <- parameters$s[1:100]
true_d <- parameters$s[1:100]
true_v1 <- parameters$v1[index100]
true_v2 <- parameters$v2[index100]
true_theta <- parameters$theta
true_r <- parameters$r

### infered arrarys
Y <- samples$BUGSoutput$sims.list
infer_a <- apply(Y$a, 2, MPD)
infer_s <- apply(Y$d, 2, MPD)
infer_d <- apply(Y$s, 2, MPD)
infer_v1 <- apply(Y$v[, , 1], 2, MPD)
infer_v2 <- apply(Y$v[, , 2], 2, MPD)
infer_theta <- apply(Y$theta, 2, MPD)
infer_r <- apply(Y$r, 2, MPD)

pl1 <- recov_plot(true = true_a, infer = infer_a, plot_lab = c('True Ability', 'Infered Ability'), plot_col = 'Least Squares Fit')
pl2 <- recov_plot(true = true_s, infer = infer_s, plot_lab = c('True Discimination', 'Infered Discrimination'), plot_col = 'Least Squares Fit')
pl3 <- recov_plot(true = true_d, infer = infer_d, plot_lab = c('True Difficulty', 'Infered Difficulty'), plot_col = 'Least Squares Fit')
pl4 <- recov_plot(true = true_theta, infer = infer_theta, plot_lab = c('True Theta', 'Infered Theta'), plot_col = 'Least Squares Fit')



sample_indices <- sample(seq_along(true_theta), 100)  # Random indices for sampling
pl4 <- recov_plot(
  true_theta[sample_indices], 
  infer_theta[sample_indices], 
  plot_lab = c('True Theta', 'Inferred Theta'), 
  plot_col = 'Least Squares Fit'
)


ggarrange(pl1, pl2, pl3, pl4)



diff_s <- abs(true_s-infer_s)
diff_d <- abs(true_d-infer_d)
pl1 <- recov_plot(
  diff_d, 
  infer_s, 
  plot_lab = c('Diff Difficulty', 'Infer Discrim'), 
  plot_col = 'Least Squares Fit'
)
pl2 <- recov_plot(
  infer_d, 
  diff_s, 
  plot_lab = c('Infer Difficulty', 'Diff Discrim'), 
  plot_col = 'Least Squares Fit'
)
pl3 <- recov_plot(
  infer_d, 
  infer_s, 
  plot_lab = c('Infer Difficulty', 'Infer Discrim'), 
  plot_col = 'Least Squares Fit'
)
pl4 <- recov_plot(
  diff_d, 
  diff_s, 
  plot_lab = c('Diff Difficulty', 'Diff Discrim'), 
  plot_col = 'Least Squares Fit'
)

ggarrange(pl1, pl2, pl3, pl4)



# Take a closer look at inferred parameters related to item
recov_plot(
  infer_d, 
  infer_s, 
  plot_lab = c('Infer Difficulty', 'Infer Discrim'), 
  plot_col = 'Least Squares Fit'
)


recov_plot(
  as.numeric(true_r), 
  infer_r, 
  plot_lab = c('True', 'Infer'), 
  plot_col = 'Least Squares Fit'
)

