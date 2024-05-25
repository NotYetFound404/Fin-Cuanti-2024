source(paste0(getwd(), "/PC's/PC1 - Alto Voltaje/attempt 2/At2 script.R"))
game_before_shuffling <- fn_anon(n_cards_on_deck  = 6,number_of_players = 1,initial_discard_pile = 1)
game_after_shuffling <- suffle_discard_pile(df_list = game_before_shuffling)
fn_anon2(game_setup = game_after_shuffling, player_id = "1")
#........................................
game_setup = game_after_shuffling
player_id = "1"






randomName <- fn_anon3(player_id = "1",game_setup = game_after_shuffling,debug_mode = T)



#The number of turns
randomName$number_of_turns
# FALTA AGREGAR CUANTO TIEMPO SE TARDO EN COMPUTAR USANDO TICTOC


b$cards_played
#.............
detailed_function <- function(game_setup, player_id) {
  library(tictoc)
  # Start the timer
  tic()
  
  # Initialize variables
  cards_played <- 0
  
  # Perform the first step
  step1 <- fn_anon2(game_setup = game_setup, player_id = player_id)
  
  # Increment cards_played if a card is played
  if (step1$card_to_play != "There is no possible play") {
    cards_played <- cards_played + 1
  }
  
  # Repeat until there is no possible play
  repeat {
    # Check if the card to play is "There is no possible play"
    if (step1$card_to_play == "There is no possible play") {
      # Exit the loop if there's no possible play
      break
    } else {
      # Perform the next step
      step1 <- fn_anon2(game_setup = step1, player_id = player_id)
      
      # Increment cards_played if a card is played
      if (step1$card_to_play != "There is no possible play") {
        cards_played <- cards_played + 1
      }
    }
  }
  
  # Stop the timer
  toc()
  
  # Return a list with cards_played and the final game state (step1)
  return(list(cards_played = cards_played, game_state = step1))
}
quick_function(game_setup = game_setup, player_id = player_id)
a <- detailed_function(game_setup = game_setup, player_id = player_id)

#------------
# fn_anon3 <- function(player_id,game_setup, debug_mode = F){
#   #Stores the state of the players deck and the discard pile trough time
#   steps_list <- list()
#   # Initial values for my_hand_input_df and discard_pile_input_df
#   my_hand_df <- game_setup$Players[[player_id]]
#   discard_pile_df <- game_setup$Discard_Pile
#   
#   #record how many cards the player has played
#   player_has_played_n_cards <- 0
#   repeat {
#     
#     
#     # Call fn_anon2 with current inputs
#     step_output <- fn_anon2(game_setup = game_setup,player_id = player_id)
#     #check if there was a card to play
#     if(step_output$card_to_play == "There is a card to play"){
#       
#       
#       if(debug_mode == T){
#         print(paste0("It is Player N°{", player_id, "} turn"))  
#       }
#       
#       # Increment the number of cards played
#       player_has_played_n_cards <- player_has_played_n_cards + 1
#       #State how many cards has the player played so far
#       if(debug_mode == T){
#         print(paste0("Player N°{",player_id,"} has played {", player_has_played_n_cards, "} many times so far"))
#       }
#       #.................
#       # Store the step's output in the list
#       steps_list$ith_state[[player_has_played_n_cards]] <- step_output
#       # Update my_hand_df and discard_pile_df for the next iteration
#       my_hand_df <- step_output$Players[[player_id]]
#       discard_pile_df <- step_output$Discard_Pile
#       #Store how many times the player has played
#       steps_list[["number_of_turns"]] <- player_has_played_n_cards
#     } else{
#       
#       if(debug_mode == T){
#         print(paste0("Player N°{", player_id, "} has stopped playing"))
#         print(paste0("Player N°{", player_id, "}, played {", player_has_played_n_cards, "} cards in total"))
#       }
#       #Store how many times the player has played
#       steps_list[["number_of_turns"]] <- player_has_played_n_cards
#       break
#     }
#   }
#   return(steps_list)  
# }