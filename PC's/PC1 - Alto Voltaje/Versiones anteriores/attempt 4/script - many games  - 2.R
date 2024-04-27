library(tictoc)
source(paste0(getwd(), "/PC's/PC1 - Alto Voltaje/attempt 4/functions.R"))

n_sim <- 100
time_vector <- numeric(n_sim)  # Preallocate the vector

simulate_game_and_time <- function() {
  tic()
  game <- initialize_game(num_cards = 6, number_of_players = 1, min_discard_pile = 1) |>
    shuffle_discard_pile() |>
    simulate_game(player_id = "1")
  toc()$toc
}

time_vector <- vapply(seq_len(n_sim), \(i) simulate_game_and_time(), numeric(1))

mean(time_vector)
