library(foreach)
library(doParallel)

# Set up parallel processing
n.cores <- length(parallelly::availableWorkers()) - 1
my.cluster <- parallel::makeCluster(n.cores, type = "PSOCK")
registerDoParallel(cl = my.cluster)

# Load external functions if needed
source(paste0(getwd(), "/PC's/PC1 - Alto Voltaje/attempt 5/functions.R"))

# Number of simulations
n_sim <- 100

# Simulation loop
time_vector <- foreach(i = 1:n_sim, .combine = 'c') %dopar% {
  tictoc::tic()
  game <- initialize_game(num_cards = 6, number_of_players = 1, min_discard_pile = 1) |> 
    shuffle_discard_pile() |> 
    simulate_game(player_id = "1")
  tictoc::toc()$toc
}

# Stop cluster and compute mean time
stopCluster(my.cluster)
mean_time <- mean(time_vector)
mean_time