library(tictoc)
source(paste0(getwd(), "/PC's/PC1 - Alto Voltaje/attempt 5/functions.R"))
sequential_simulation <- function(n_sim) {
  time_vector <- numeric(n_sim)
  for (i in 1:n_sim) {
    tic()
    game <- initialize_game(num_cards = 6, number_of_players = 1, min_discard_pile = 1) |>
      shuffle_discard_pile() |>
      simulate_game(player_id = "1")
    compute_time <- toc()
    time_vector[i] <- compute_time$toc  # Store the elapsed time in the ith position of time_vector
  }
  return(time_vector)  # Return the time vector with n_sim elements
}
parallel_simulation <- function(n_sim) {
  library(foreach)
  library(doParallel)
  library(parallelly)
  library(tictoc)
  source(paste0(getwd(), "/PC's/PC1 - Alto Voltaje/attempt 5/functions.R"))
  
  n_cores <- availableCores() - 1
  my_cluster <- makeCluster(n_cores, type = "PSOCK")
  registerDoParallel(my_cluster)
  
  # Export required functions to parallel workers
  clusterExport(my_cluster, c("initialize_game", "shuffle_discard_pile", "simulate_game", "play_card", "create_card_deck", "deal_cards", "simulate_game"))
  
  time_vector <- foreach(i = 1:n_sim, .combine = 'c') %dopar% {
    tictoc::tic()
    game <- initialize_game(num_cards = 6, number_of_players = 1, min_discard_pile = 1) |> 
      shuffle_discard_pile() |> 
      simulate_game(player_id = "1")
    tictoc::toc()$toc
  }
  
  stopCluster(my_cluster)
  return(time_vector)
}
n_sim <- 10  # Adjust as needed

library(microbenchmark)
benchmark_results <- microbenchmark(
  sequential = sequential_simulation(n_sim),
  parallel = parallel_simulation(n_sim),
  times = 10  # Number of times to repeat each measurement
)

# Print the benchmark results
print(benchmark_results)

