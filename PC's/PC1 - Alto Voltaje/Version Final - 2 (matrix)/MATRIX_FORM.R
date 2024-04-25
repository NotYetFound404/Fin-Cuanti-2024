library(purrr)
source(paste0(getwd(), "/PC's/PC1 - Alto Voltaje/Version Final - 2 (matrix)/optimized_functions.R"))
set.seed(123)
#Simulate n games
n = 10
games_list <- map(1:n, ~ initialize_game(num_cards = 73, number_of_players = 1, min_discard_pile = 1) |>
                    shuffle_discard_pile())
#Play the games with vectorization
played_games <- map(games_list, ~ simulate_game(.x, "1"))

played_games[[1]]$time_taken

# Extract time taken for each game
time_taken_vector <- map_dbl(played_games, pluck, "time_taken$callback_msg")
