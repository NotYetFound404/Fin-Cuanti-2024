parallel_simulation <- function(n_sim) {
  library(foreach)
  library(doParallel)
  library(parallelly)
  library(tictoc)
  source(paste0(getwd(), "/PC's/PC1 - Alto Voltaje/Version Final - 1 (vectorizado)/optimized_functions.R"))
  
  #parallelization setup
  n_cores <- availableCores() - 1
  my_cluster <- makeCluster(n_cores, type = "PSOCK")
  registerDoParallel(my_cluster)
  
  # Export required functions to parallel workers
  clusterExport(my_cluster, c("initialize_game", "shuffle_discard_pile", "simulate_game", "play_card", "create_card_deck", "deal_cards", "simulate_game"))
  
  time_vector <- foreach(i = 1:n_sim, .combine = 'c') %dopar% {
    tictoc::tic()
    game <- initialize_game(num_cards = 73, number_of_players = 1, min_discard_pile = 1) |> 
      shuffle_discard_pile() |> 
      simulate_game(player_id = "1")
    tictoc::toc()$toc
  }
  
  stopCluster(my_cluster)
  return(time_vector)
}
n_sim <- 1e6
tictoc::tic()
re <- parallel_simulation(n_sim = n_sim)
total_time <- tictoc::toc()
print(paste0("Total compute time: ", total_time$callback_msg))
print(paste0("Average copmute time per simulation: ", mean(re)))
library(ggplot2)
p <- ggplot(data = data.frame(time=re), aes(x = time)) +
  geom_histogram() +  # Adjust binwidth as needed
  labs(x = "Time (seconds)", y = "Frequency", title = "Computation Time Histogram")
p
