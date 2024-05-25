library(tictoc)
library(furrr)

# Set up parallel processing backend
plan(multisession)  # or plan(multicore) for multicore processing

source(paste0(getwd(), "/PC's/PC1 - Alto Voltaje/attempt 4/functions.R"))

n_sim <- 10000
time_vector <- numeric(n_sim)  # Preallocate the vector

simulate_game_and_time <- function() {
  tic()
  game <- initialize_game(num_cards = 6, number_of_players = 1, min_discard_pile = 1) |>
    shuffle_discard_pile() |>
    simulate_game(player_id = "1")
  toc()$toc
}

# Use future_map_dbl from furrr for parallel execution
time_vector <- future_map_dbl(seq_len(n_sim), \(i) simulate_game_and_time())

# Calculate the mean time
mean_time <- mean(time_vector)
mean_time
