source(paste0(getwd(), "/PC's/PC1 - Alto Voltaje/script.R"))
game_before_shuffling <- fn_anon(n_cards_on_deck  = 10,number_of_players = 1,initial_discard_pile = 1)
game_after_shuffling <- suffle_discard_pile(df_list = game_before_shuffling)
player_number <- "1"
randomName <- fn_anon3(player_number = "1",card_original_setup = game_after_shuffling,debug_mode = F)

#The number of turns
randomName$number_of_turns
# FALTA AGREGAR CUANTO TIEMPO SE TARDO EN COMPUTAR USANDO TICTOC

