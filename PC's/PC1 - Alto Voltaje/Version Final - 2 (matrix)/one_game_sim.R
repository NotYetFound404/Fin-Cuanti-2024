library(purrr)
source(paste0(getwd(), "/PC's/PC1 - Alto Voltaje/Version Final - 2 (matrix)/optimized_functions.R"))
set.seed(123)

game_starts <- initialize_game(num_cards = 73, number_of_players = 1, min_discard_pile = 1) |> shuffle_discard_pile()
simules1game <-  game_starts |> simulate_game(player_id = "1")

#Simulate n games
#n = 1
#games_list <- map(1:n, ~ initialize_game())

#make sure both list are identical
games_list <- list()
games_list[[1]] <- game_starts
identical(game_starts, games_list[[1]])

#generate a vectorized played games
played_games <- map(games_list, ~ simulate_game(.x, "1"))
identical(simules1game, played_games[[1]])
#it is true
