source(paste0(getwd(), "/PC's/PC1 - Alto Voltaje/Version Final - 1 (vectorizado)/optimized_functions.R"))
game <- initialize_game(num_cards = 73, number_of_players = 1, min_discard_pile = 1) |> 
  shuffle_discard_pile() |> 
  simulate_game(player_id = "1")

game$Discard_Pile
game$Players
game$card_to_play

