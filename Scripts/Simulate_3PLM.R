sim_model <- function(nsub, nNoise, L){
  nimg = nNoise * L
  ntrials = nsub * nimg
  jid <- rep(1:nimg, times = nsub)   #  Image index for observation i
  iid <- rep(1:nsub, each = nimg)      #  Participant index for observation i 
  labels <- 1:L #labels 
  
  #####--------------------------------------#####
  #####   1. Generate subject parameters    #####
  #####--------------------------------------#####
  i <- list()
  # Add elements to the list
  i$a <- rnorm(n = nsub, mean = 0.5, sd = 1) # Individual image recognition ability
  
  #####--------------------------------------#####
  #####   2.  Generate item parameters       #####
  #####--------------------------------------#####
  j <- list()
  j$z <- sample(labels, size = nimg, replace = TRUE)
  j$noise_level <- sample(rep(1:nNoise, each = L))
  j$d <- 4 - 8*(j$noise_level/(nNoise+1)) + runif(n = nimg, min = -1, 1) # Image difficulty
  j$s <- truncnorm::rtruncnorm(n = nimg, mean = 1, sd = 1, a = 0) # Discrimination parameter
   # Guessing ability varies from 0 to double the chance level
  
  
  #####--------------------------------------#####
  #####   3. Generate trial parameters       #####
  #####--------------------------------------#####
  ij <- list()
  ij$logodds <- j$s[jid]*(i$a[iid] - j$d[jid]) # Create all combinations of logodds
  ij$theta <- 1 / (1 + exp(-ij$logodds))  # Find theta (probability correct)
  ij$eta <- rbeta(n= ntrials, shape1 = (1/L)*20, shape2 = 20-(1/L)*20)
  ij$prob_z <-  ij$eta +  (1- ij$eta)*ij$theta
  ### Record choices: ###
  ij$z <- j$z[jid]
  ij$not_z <- sapply(ij$z, function(z_val) sample(setdiff(labels, z_val), size = 1))
  ij$label <- ifelse(runif(n = ntrials) < ij$prob_z, # Prob of picking correct
                     ij$z, # Correct label
                     ij$not_z) # Incorrect label

  #####--------------------------------------#####
  #####          4. Return Results           #####
  #####--------------------------------------#####
  
  ### Saving Key Latent Parameters and Predictions for recovery ###
  parameters <- tibble(iid = iid, jid = jid, a = i$a[iid], s = j$s[jid], d = j$d[jid], theta = ij$theta, eta = ij$eta, prob_z = ij$prob_z, lbl = ij$label, z = ij$z)
  return(parameters)
}