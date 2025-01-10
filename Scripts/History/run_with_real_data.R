## Packages
pacman::p_load(tidyverse, truncnorm, R2jags, parallel, ggpubr, extraDistr)
data <- read_csv(file = "AI_Reliance_DecisionMaking/Data/data_sequential_paradigm.csv")
subset <- data %>% 
  filter(model_on == 0) %>% 
  select("pid" = task_number, "uid" = participant_id, z = image_category, "lbl" = participant_classification, "r0" = confidence) %>% 
  mutate(
    across(c(z, lbl), as.factor), 
    r0 = factor(r0, levels = c("low", "medium", "high"), ordered = TRUE))
subset <- subset %>% 
  mutate("N0" = rep(count(subset)),
         "N1" = rep(count(data)-count(subset)),
         "NS" = rep(length(unique(data$participant_id))),
         "NI" = rep(length(unique(data$image_id))),
         "L" = rep(length(unique(data$image_category))))

# Define JAGS data list
jags_data <- list(
  N0 = subset$N0[1],
  N1 = subset$N1[1],
  NS = subset$NS[1],
  NI = subset$NI[1],
  L = subset$L[1],
  pid1 = subset$pid,
  uid1 = subset$uid,
  z = as.integer(subset$z),  # True labels for observations
  lbl = as.integer(subset$lbl),  # Labels in the no-advice condition
  r0 = as.integer(subset$r0),  # Confidence ratings for observations in advice off condition
  N = nrow(subset)
)

# Find file:
model_file <- "AI_Reliance_DecisionMaking/Models/jags_NOADV1_transl.txt"

# Parameters to save
params <- c("a", "s", "d", "r1", "r_pred")
# Run file:
samples <- jags.parallel(
  data = jags_data, 
  inits=NULL,
  parameters.to.save = params,
  jags.seed = 1234,
  model.file = model_file, 
  n.chains= 3, n.iter=3000, n.burnin=1000, n.thin=1, n.cluster=4)