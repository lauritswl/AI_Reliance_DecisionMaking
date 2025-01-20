sim_model <- function(nsub, nNoise, nAI,L, ai_theta_input = 0.8){
  set.seed(98)
  # FOR TESTING:
  #nNoise <- 5 # Levels of noise-filters to apply to the images
  #L <- 16 # Different images for classification
  #nsub <- 70 # Number of participants
  #nAI <- 5
  
  nimg = nNoise * L
  ntrials = nsub * nimg
  jid <- rep(1:nimg, times = nsub)   #  Image index for observation i
  iid <- rep(1:nsub, each = nimg)      #  Participant index for observation i 
  labels <- 1:L #labels
  
  #####--------------------------------------#####
  #####   1. Generate subject parameters     #####
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
  j$eta <- rbeta(n= nimg, shape1 = (1/L)*20, shape2 = 20-(1/L)*20) # Guessing ability varies from 0 to double the chance level
  
  
  #####--------------------------------------#####
  #####   3.  Generate AI parameters         #####
  #####--------------------------------------#####  
  k <- list()
  k$ai_theta <- seq(from = 0.2, to = 0.95, length.out = nAI)
  if (nAI == 1){
    k$ai_theta <- ai_theta_input
  }
  k$ai_theta_string <- rep(paste(sprintf("%.2f", k$ai_theta), collapse = ", "), times = ntrials)

  
  #####--------------------------------------#####
  #####   4. Generate trial parameters       #####
  #####--------------------------------------#####
  ij <- list()
  ij$logodds <- j$s[jid] * i$a[iid] - j$d[jid] # Create all combinations of logodds
  ij$theta <- 1 / (1 + exp(-ij$logodds))  # Find theta (probability correct)
  ### Record choices: ###
  ij$z <- j$z[jid]
  ij$not_z <- sapply(ij$z, function(z_val) sample(setdiff(labels, z_val), size = 1))
  ij$ai_not_z <- sapply(ij$z, function(z_val) sample(setdiff(labels, z_val), size = 1))
  ij$human_with_ai_not_z <- sapply(ij$z, function(z_val) sample(setdiff(labels, z_val), size = 1))
  ij$label <- ifelse(runif(n = ntrials) < ij$theta, # Prob of picking correct
                     ij$z, # Correct label
                     ij$not_z) # Incorrect label

  #####--------------------------------------#####
  #####      5. Generate ai choices          #####
  #####--------------------------------------#####

  
  # Compute a matrix of probabilities for all AI agents and trials
  # Repeat k$ai_theta into a matrix with ntrials rows and nAI columns
  ai_theta_matrix <- matrix(rep(k$ai_theta, each = ntrials), nrow = ntrials, ncol = nAI, byrow = FALSE)
  # Example matrix
  
  # Add truncated noise
  noise_sd <- 0.1  # Standard deviation of the noise
  ai_theta_matrix <- ai_theta_matrix + matrix(rnorm(ntrials * nAI, mean = 0, sd = noise_sd), 
                                           nrow = ntrials, ncol = nAI)
  
  # Truncate to [0, 1]
  ai_theta_matrix <- pmax(pmin(ai_theta_matrix, 1), 0)
  # Generate a random uniform matrix for comparison
  random_uniform <- matrix(runif(ntrials * nAI), nrow = ntrials, ncol = nAI)
  
  # Generate a matrix of responses: Correct label or incorrect label
  ai_responses <- ifelse(random_uniform < ai_theta_matrix, 
                         matrix(rep(ij$z, nAI), nrow = ntrials),      # Correct label
                         matrix(rep(ij$ai_not_z, nAI), nrow = ntrials)) # Incorrect label
  
  # Convert the matrix to a dataframe
  ai_trials <- as.data.frame(ai_responses)
  ai_theta_matrix_df <- as.data.frame(ai_theta_matrix)
  # Rename columns to reflect AI agent IDs
  colnames(ai_trials) <- paste0("ai_", 1:nAI)
  colnames(ai_theta_matrix_df)<- paste0("ai_", 1:nAI)
  # Add the trial ID column
  ai_trials <- cbind(trial_id = 1:ntrials, ai_trials)
  
  #####--------------------------------------#####
  #####  6. Generate ai-aided human choices  #####
  #####--------------------------------------#####

  # Initialize a matrix to store human decisions with AI aid
  human_with_ai <- matrix(NA, nrow = ntrials, ncol = nAI)
  human_with_ai_odds <-  matrix(NA, nrow = ntrials, ncol = nAI)
  
  ij$theta_matrix <- matrix(rep(ij$theta, each = nAI), nrow = length(ij$theta), ncol = nAI, byrow = FALSE)
  ai_theta_matrix <- as.matrix(ai_theta_matrix_df[1:(nAI)])
  alpha_output <- mapply(function(ai, theta) rbeta(1, shape1 = 10 * ai, shape2 = 10 * theta), 
                         ai = as.vector(ai_theta_matrix), 
                         theta = as.vector(ij$theta_matrix))
  alpha_matrix <- matrix(
    data = alpha_output,
    nrow = nrow(ai_theta_matrix))
  # Loop over AI agents
  for (k_idx in 1:nAI) {
    # AI choices for this agent
    c_jk <- ai_trials[[paste0("ai_", k_idx)]]
    # Compute probabilities for all four cases
    p_y <- ifelse(
      ij$label == ij$z  & c_jk == ij$z,                                              # If human and ai guessed correctly
      (1-alpha_matrix[,k_idx])+alpha_matrix[,k_idx],                                 # no switch or switch
      ifelse(
        ij$label == ij$z  & c_jk != ij$z,                                            # If human guessed correctly but ai guessed incorrectly
        (1-alpha_matrix[,k_idx]),                                                    # no switch
        ifelse(                                                                        # Else check new statement
          ij$label != ij$z & c_jk == ij$z,                                             # If human guessed wrong, but ai guessed correctly
          (1-alpha_matrix[,k_idx]),                                                    # Then switch
          0                                                                            # If both are wrong, then there is no chance of being correct.
        )
      )
    )
    # Sample a decision based on the computed probabilities
    human_with_ai[, k_idx] <- ifelse(runif(n = ntrials)<p_y, ij$z, ij$human_with_ai_not_z)
    human_with_ai_odds[ ,k_idx] <- p_y
  }
  
  # Convert human odds decisions with AI aid to a dataframe
  human_with_ai_odds_df <- as.data.frame(human_with_ai_odds)
  colnames(human_with_ai_odds_df) <- paste0("human_with_ai_odds_", 1:nAI)
  # Convert human decisions with AI aid to a dataframe
  human_with_ai_df <- as.data.frame(human_with_ai)
  colnames(human_with_ai_df) <- paste0("human_with_ai_", 1:nAI)
  
  # Name alpha
  alpha_df <- as.data.frame(alpha_matrix)
  colnames(alpha_df) <- paste0("alpha_ai_", 1:nAI)
  
  # Name theta
  colnames(ai_theta_matrix_df) <- paste0("Theta_AI_", 1:nAI)
  

  #####--------------------------------------#####
  #####          7. Return Results           #####
  #####--------------------------------------#####
  
  ### Saving Key Latent Parameters and Predictions for recovery ###
  parameters <- tibble(iid = iid, jid = jid, a = i$a[iid], alpha = alpha_matrix[,k_idx], s = j$s[jid], d = j$d[jid], theta = ij$theta, eta = j$eta[jid],lbl = ij$label, z = ij$z, prob_z = ij$prob_z, ai_theta_string = k$ai_theta_string)
  parameters <- cbind(parameters, ai_trials[2:(nAI+1)], human_with_ai_df[1:nAI], human_with_ai_odds_df[1:nAI], ai_theta_matrix_df[1:nAI], alpha_df[1:nAI])
  return(parameters)
}