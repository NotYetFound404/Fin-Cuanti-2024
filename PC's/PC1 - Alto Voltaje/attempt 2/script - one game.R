source(paste0(getwd(), "/PC's/PC1 - Alto Voltaje/attempt 2/functions.R"))
game_before_shuffling <- fn_anon(n_cards_on_deck  = 73,number_of_players = 1,initial_discard_pile = 1)
game_after_shuffling <- suffle_discard_pile(df_list = game_before_shuffling)
game <- det_function(game_setup = game_after_shuffling, player_id = "1")

