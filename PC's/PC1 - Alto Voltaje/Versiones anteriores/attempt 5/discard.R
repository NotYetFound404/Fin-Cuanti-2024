play_card_og <- function(game_state, player_id = "1") {
  library(dplyr)
  # Extract the discard pile and players' hands from game_state
  discard_pile <- game_state$Discard_Pile
  players_hands <- game_state$Players[[player_id]]
  
  # Grab the card on the highest z-index in the discard pile
  top_card <- game_state$Discard_Pile[which.max(game_state$Discard_Pile$z_index), ]
  
  # Find playable cards based on the top card on the discard pile
  top_card_values <- c(top_card$values_positive, top_card$values_negative)
  playable_cards <- players_hands |>
    filter(values_positive %in% top_card_values | values_negative %in% top_card_values)
  
  if (nrow(playable_cards) > 0) {
    # Select the first playable card
    card_to_play <- playable_cards[1, ]
    
    # Remove the played card from the player's hand
    players_hands_after <- players_hands |> 
      filter(card_id != card_to_play$card_id)
    
    # Add the selected card to the discard pile
    card_to_discard_pile <- card_to_play |> mutate(z_index = top_card$z_index +1)
    discard_pile_after <- bind_rows(card_to_discard_pile, discard_pile)
    
    # Update game_state with the modified discard pile and player's hand
    game_state$Discard_Pile <- discard_pile_after
    game_state$Players[[player_id]] <- players_hands_after
    
    output <- list(game_state = game_state, "card_to_play" = "There is a card to play")
  } else {
    output <- list(game_state = game_state, "card_to_play" = "There is no possible play")
  }
  
  return(output)
}
play_card_v2 <- function(game_state, player_id = "1") {
  library(dplyr)
  # Extract the discard pile and players' hands from game_state
  discard_pile <- game_state$Discard_Pile
  players_hands <- game_state$Players[[player_id]]
  
  # Grab the card on the highest z-index in the discard pile
  top_card <- game_state$Discard_Pile[which.max(game_state$Discard_Pile$z_index), ]
  
  # Find playable cards based on the top card on the discard pile
  top_card_values <- c(top_card$values_positive, top_card$values_negative)
  
  # Check if players_hands is not NULL before filtering
  if (!is.null(players_hands)) {
    playable_cards <- players_hands |>
      filter(values_positive %in% top_card_values | values_negative %in% top_card_values)
  } else {
    playable_cards <- NULL
  }
  
  if (!is.null(playable_cards) && nrow(playable_cards) > 0) {
    # Select the first playable card
    card_to_play <- playable_cards[1, ]
    
    # Remove the played card from the player's hand
    players_hands_after <- players_hands |> 
      filter(card_id != card_to_play$card_id)
    
    # Add the selected card to the discard pile
    card_to_discard_pile <- card_to_play |> mutate(z_index = top_card$z_index + 1)
    discard_pile_after <- bind_rows(card_to_discard_pile, discard_pile)
    
    # Update game_state with the modified discard pile and player's hand
    game_state$Discard_Pile <- discard_pile_after
    game_state$Players[[player_id]] <- players_hands_after
    
    output <- list(game_state = game_state, "card_to_play" = "There is a card to play")
  } else {
    output <- list(game_state = game_state, "card_to_play" = "There is no possible play")
  }
  
  return(output)
}