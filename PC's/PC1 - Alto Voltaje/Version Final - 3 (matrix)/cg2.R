check_deadlock <- function(game_state, player_id = "1") {
  # Extract the discard pile and player's hand from the game state
  discard_pile <- game_state$Discard_Pile
  players_hands <- game_state$Players[[player_id]]
  
  # Extract the values of the discard pile card
  discard_pile_values <- c(discard_pile$values_positive, discard_pile$values_negative)
  
  # Extract the values from my hand
  my_hands_values <- c(players_hands$values_positive, players_hands$values_negative)
  
  # Return TRUE if there are no playable cards, FALSE if there are playable cards
  return(!any(my_hands_values %in% discard_pile_values))
}
#----------------
set.seed(2017)
game <- initialize_game(num_cards = 15, number_of_players = 1, min_discard_pile = 1) |> 
  shuffle_discard_pile()
game$Discard_Pile


library(dplyr)

rm(step1)
# Perform the first step
step1 <- play_card(game_state = game, player_id = "1")


simulate_and_empty_hand_2 <- function(game_state, player_id) {
  check_deadlock <- function(game_state, player_id = "1") {
    # Extract the discard pile and player's hand from the game state
    discard_pile <- game_state$Discard_Pile
    players_hands <- game_state$Players[[player_id]]
    
    # Extract the values of the discard pile card
    discard_pile_values <- c(discard_pile$values_positive, discard_pile$values_negative)
    
    # Extract the values from my hand
    my_hands_values <- c(players_hands$values_positive, players_hands$values_negative)
    
    # Return TRUE if there are no playable cards, FALSE if there are playable cards
    return(!any(my_hands_values %in% discard_pile_values))
  }
  
  library(tictoc)
  tic(quiet = TRUE)
  # Perform the first step
  step1 <- play_card(game_state = game_state, player_id = player_id)
  
  # Perform the next steps
  while (nrow(step1$Players[[player_id]]) > 0) {
    # Check for deadlock before making adjustments
    if (check_deadlock(game_state = step1)) {
      print("theres a deadlock")
      break
    }
    
    # Shuffle the discard pile
    if (step1$card_to_play == "There is no possible play") {
      step1 <- shuffle_discard_pile(step1)
    }
    
    step1 <- play_card(game_state = step1, player_id = player_id)
    print(nrow(step1$Players[[player_id]]))
    
  }
  
  step1[["time_taken"]] <- toc(quiet = TRUE)
  return(step1)
}
aa <- simulate_and_empty_hand_2(game_state = game,player_id = "1")

aa$time_taken
rm(aa)














#....................
# aa <- simulate_game_v2(game_state = game, player_id = "1")
# while (nrow(step1$Players[["1"]]) > 0) {
#   # Check for deadlock before making adjustments
#   if (check_deadlock(game_state = step1)){
#     #print("theres a deadlock")
#     break
#   }
#   # Adjust the discard pile
#   if (step1$card_to_play == "There is no possible play"){
#     step1$Discard_Pile$z_index <- c(1, step1$Discard_Pile$z_index[-1] + 1)
#     
#   }
#     step1 <- play_card(game_state = step1, player_id = "1")
# 
#     #order the discard pile
#     step1$Discard_Pile <-  arrange(step1$Discard_Pile, desc(z_index))
# }
# step1



# # Repeat until the player's hand has zero rows or there's a deadlock
# while (counter < 20) {
#   # Check the nrows is cero 
#   if (nrow(step1$Players[["1"]]) == 0){
#     print("Empty hand")
#     break
#   }
#   
#   print(step1$card_to_play)
#   # Play the hand until there are no possible plays
#   while (step1$card_to_play != "There is no possible play") {
#     step1 <- play_card(game_state = step1, player_id = "1")
#     #print(step1$card_to_play)
#   }
#   
#   # Adjust the discard pile
#   step1$Discard_Pile$z_index <- c(1, step1$Discard_Pile$z_index[-1] + 1)
#   step1 <- play_card(game_state = game, player_id = "1")
#   
#   counter = counter +1
# 
# }
# step1





#......................
# # Perform the first step
# step1 <- play_card(game_state = game, player_id = "1")
# 
# # Play the hand until there are no possible plays
# while (step1$card_to_play != "There is no possible play") {
#   step1 <- play_card(game_state = step1, player_id = "1")
# }
# # Adjust the discard pile
# step1$Discard_Pile$z_index <- c(1, step1$Discard_Pile$z_index[-1] + 1)
# 
# # Play again after adjusting the discard pile
# while (step1$card_to_play != "There is no possible play") {
#   step1 <- play_card(game_state = step1, player_id = "1")
# }
# 
# nrow(step1$Players[["1"]])
# 
# check_deadlock(game_state = step1)
#......................

# # Perform the first step
# step1 <- play_card(game_state = game, player_id = "1")
# step1$Discard_Pile
# 
# #1. play my hand until there is no possible play
# repeat {
#   # Check if the card to play is "There is no possible play"
#   if (step1$card_to_play == "There is no possible play") {
#     
#     print(check_deadlock(step1,player_id = "1"))
#     # Exit the loop if there's no possible play
#     break
#   } else {
#     # Perform the next step
#     step1 <- play_card(game_state = step1, player_id = player_id)
#   }
# }
# #2. Whenever there is no possible play adjuste the discard pile
# #grab the next card on the discard pile and store the previous one in the last position
# step1$Discard_Pile$z_index <- c(1, step1$Discard_Pile$z_index[-1] +1)
# 
# #3. Play again 
# repeat {
#   # Check if the card to play is "There is no possible play"
#   if (step1$card_to_play == "There is no possible play") {
#     
#     print(check_deadlock(step1,player_id = "1"))
#     # Exit the loop if there's no possible play
#     break
#   } else {
#     # Perform the next step
#     step1 <- play_card(game_state = step1, player_id = player_id)
#   }
# }
# #repeat?
# step1






