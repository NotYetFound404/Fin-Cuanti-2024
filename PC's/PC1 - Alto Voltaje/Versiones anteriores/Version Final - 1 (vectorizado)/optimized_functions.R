create_card_deck  <- function(num_cards = 73){
  main_numbers <- round(runif(n = num_cards,min = 1,max = 10))
  modifiers <- round(runif(num_cards, min = 1, max = 3))
  output <- data.frame(main_numbers = main_numbers, modifiers = modifiers)
  return(output)
}
deal_cards <- function(num_players  = 2, num_cards  = 73, min_discard_pile  = 1){
  total_cards_excluding_discard <- num_cards  - min_discard_pile 
  cards_per_player <- total_cards_excluding_discard %/% num_players 
  remaining_cards_for_discard <- num_cards  - cards_per_player*num_players 
  return(c(rep(0, times = remaining_cards_for_discard),
           rep(1:num_players , each = cards_per_player)))
}
initialize_game <- function(num_cards = 73, number_of_players = 2, min_discard_pile = 1){
  # Generate 73 cards 
  df <- create_card_deck (num_cards = num_cards)
  #Calculate the cards values before-hand
  df$values_positive <- df$main_numbers + df$modifiers
  df$values_negative <- df$main_numbers - df$modifiers
  df$values_positive <- ifelse(df$values_positive > 10, 1, df$values_positive)
  df$values_negative <- ifelse(df$values_negative < 0, 8, df$values_negative)
  
  # Add a unique identifier column
  df$card_id <- seq_len(nrow(df))
  # Deal the cards(Decide which cards are assigned to whom)
  card_holder <- deal_cards(num_players = number_of_players, num_cards = num_cards, min_discard_pile  = min_discard_pile)
  # Group the dataframe by card_holder and split into list
  df_split <- split(df, card_holder)
  # Create output list with discard pile and players' hands
  output <- list(
    "Discard_Pile" = df_split[[1]],  # Using [[0]] for the discard pile (card_holder = 0)
    "Players" = df_split[-1]  # Rest of the groups correspond to players
  )
  return(output)
}
shuffle_discard_pile  <- function(game_state){
  game_state$Discard_Pile <- sample(game_state$Discard_Pile, replace = FALSE)
  game_state$Discard_Pile$z_index <- seq_len(nrow(game_state$Discard_Pile))
  return(game_state)
}
play_card <- function(game_state, player_id = "1") {
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
    game_state$card_to_play <- "There a card to play"
    output <- game_state
  } else {
    game_state$card_to_play <- "There is no possible play"
    output <- game_state
  }
  return(output)
}
simulate_game <- function(game_state, player_id){
  # Perform the first step
  step1 <- play_card(game_state = game_state, player_id = player_id)
  # Repeat until there is no possible play
  repeat {
    # Check if the card to play is "There is no possible play"
    if (step1$card_to_play == "There is no possible play") {
      # Exit the loop if there's no possible play
      break
    } else {
      # Perform the next step
      step1 <- play_card(game_state = step1, player_id = player_id)
    }
  }
  return(step1)
}
