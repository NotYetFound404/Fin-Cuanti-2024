#precalculated (this is faster)
library(tictoc)
source(paste0(getwd(), "/PC's/PC1 - Alto Voltaje/attempt 4/functions.R"))
game <- initialize_game(num_cards = 73, number_of_players = 1, min_discard_pile = 1) |> shuffle_discard_pile()
# step1 <- play_card(game_state = game, player_id = "1")
# step1
tic()
playing_game <- simulate_game(game_state = game, player_id = "1")
toc()
playing_game


#................
# library(tictoc)
# source(paste0(getwd(), "/PC's/PC1 - Alto Voltaje/attempt 4/functions.R"))
# game <- initialize_game(num_cards = 6, number_of_players = 1, min_discard_pile = 1) |> shuffle_discard_pile()
# playing_game <- simulate_game(game_state = game, player_id = "1")
# playing_game