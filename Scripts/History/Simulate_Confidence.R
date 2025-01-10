sim_model <- function(nsub, nimg, L){
  ntrials = nsub * nimg
  jid <- rep(1:nimg, times = nsub)   #  Image index for observation i
  iid <- rep(1:nsub, each = nimg)      #  Participant index for observation i 
  labels <- 1:L #labels 
  
  #####--------------------------------------#####
  #####     Generate subject parameters      #####
  #####--------------------------------------#####
  i <- list()
  # Add elements to the list
  i$a <- rnorm(n = nsub, mean = 0, sd = 1) # Individual image recognition ability
  i$v <- t(apply(matrix(runif(2 * nsub), ncol = 2), 1, sort)) # Confidence intervals
  i$tau <- runif(n = nsub, min = 0, max = 15) # Precision
  i$sigma <- 1 / i$tau # Fluidity of confidence levels
  
  
  ### Generate parameters only dependent on the image:
  j <- list()
  j$d <- runif(n = nimg, min = 0, max = 1) # Image difficulty
  j$s <- truncnorm::rtruncnorm(n = nimg, mean = 1, sd = 1, a = 0) # Discrimination parameter
  j$z <- sample(labels, size = nimg, replace = TRUE)
  j$not_z <- sapply(j$z, function(z_val) sample(setdiff(labels, z_val), size = 1))
  
  ### Generate parameters only dependent on the ij:
  # Initialize an empty list
  ij <- list()
  
  # Find theta (probability correct)
  ij$logodds <- j$s[jid] * i$a[iid] - j$d[jid] # Create all combinations of logodds
  ij$theta <- 1 / (1 + exp(-ij$logodds)) 
  
  
  # Find Human Confidence
  ij$sigma <- i$sigma[iid] # Rewrite sigma, i.e. fluidity of confidence levels, to ij indexing
  ij$confidence <- rnorm(n = ntrials, mean = ij$theta, sd = ij$sigma) # Generate confidence
  ij$v <- i$v[iid,]
  ij$conf_lvl <- ifelse(ij$confidence <= ij$v[, 1], "low",
                        ifelse(ij$confidence <= ij$v[, 2], "medium", "high"))
  ij$conf_lvl <- factor(ij$conf_lvl, levels = c("low", "medium", "high"))
  
  # Define label distribution
  
  ij$label <- ifelse(runif(n = ntrials) < ij$theta, # Prob of picking correct
                     j$z[jid], # Correct label
                     j$not_z[jid]) # Incorrect label
  ij$true_label <- j$z[jid]
  
  
  
  # Saving Key Latent Parameters for recovery check 
  ij$a <- i$a[iid]
  ij$s <- j$s[jid]
  ij$d <- j$d[jid]
  parameters <- tibble(id = iid, item = jid, a = ij$a, s = ij$s, d = ij$d, v1 = ij$v[,1], v2 = ij$v[,2], theta = ij$theta, r = ij$conf_lvl)
  
  
  # Save in dataframe:
  data <- tibble(id = iid, item = jid, conf_lvl = ij$conf_lvl, y = ij$label, true_label = ij$true_label, L = L)
  # Convert confidence levels to numeric
  data$conf_lvl_numeric <- as.numeric(factor(data$conf_lvl, levels = c("low", "medium", "high")))
  # Map participant IDs and item IDs to sequential indices
  data$uid <- as.numeric(as.factor(data$id))    # Convert participant IDs to sequential indices
  data$pid <- as.numeric(as.factor(data$item))  # Convert item IDs to sequential indices
  
  return_list <- list("data" = data, "parameters" = parameters)
  
  
  return(return_list)
}