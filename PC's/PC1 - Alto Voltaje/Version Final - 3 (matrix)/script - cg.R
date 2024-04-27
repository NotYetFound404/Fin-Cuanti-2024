#-----------------
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
  #Adds z-index to keep track of which card is on top
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
  library(tictoc)
  tic(quiet = T)
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
  step1[["time_taken"]] <- toc(quiet = T)
  return(step1)
}
simulate_and_empty_hand <- function(game_state, player_id) {
  library(tictoc)
  tic(quiet = TRUE)
  
  while (TRUE) {
    if (is.null(game_state$Players[[player_id]]) || nrow(game_state$Players[[player_id]]) == 0) {
      break  # Exit the loop if the player's hand is empty
    }
    
    game_state <- play_card(game_state = game_state, player_id = player_id)
    
    if (game_state$card_to_play == "There is no possible play") {
      game_state <- shuffle_discard_pile(game_state)
    }
  }
  
  game_state[["time_taken"]] <- toc(quiet = TRUE)
  return(game_state)
}
#-----------------
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
simulate_game_v2 <- function(game_state, player_id){
  library(tictoc)
  tic(quiet = T)
  # Perform the first step
  step1 <- play_card(game_state = game_state, player_id = player_id)
  # Repeat until there is no possible play
  
  # Initialize the flag for deadlock
  deadlock_flag <- FALSE
  
  repeat {
    # Check if the card to play is "There is no possible play"
    if (step1$card_to_play == "There is no possible play") {
      #Check if there could be a deadlock
      if(check_deadlock(game_state = step1, player_id = player_id)){
        #If there is a deadlock and there is no play stop playing
        # If there is a deadlock and there is no play, set the deadlock flag
        deadlock_flag <- TRUE
        #stop playing
        break
      }
      #In case there is no deadlock the game state will reshuffle the discard pile
      
      step1 <- shuffle_discard_pile(step1)
      #Eventually the game will empty the players hand
      
    } else {
      # Perform the next step
      step1 <- play_card(game_state = step1, player_id = player_id)
    }
  }
  step1[["time_taken"]] <- toc(quiet = T)
  # Add the information about deadlock to the game state
  step1$was_there_deadlock <- deadlock_flag
  return(step1)
}

simulate_game_v3 <- function(game_state, player_id){
  library(tictoc)
  tic(quiet = TRUE)
  
  # Perform the first step
  step1 <- play_card(game_state = game_state, player_id = player_id)
  
  # Initialize the flag for deadlock
  deadlock_flag <- FALSE
  
  # Repeat until there is no possible play
  repeat {
    # Check if player's hand is empty
    if (is.null(step1$Players[[player_id]]) || nrow(step1$Players[[player_id]]) == 0) {
      message("Player's hand is empty.")
      break  # Exit the loop if player's hand is empty
    }
    
    
    # Check if it is possible to play a card
    if (step1$card_to_play == "There a card to play") {
      # Play a card
      step1 <- play_card(game_state = step1, player_id = player_id)
      next  # Skip the rest of the loop iteration
    }
    
    # Check if there could be a deadlock
    if (check_deadlock(game_state = step1, player_id = player_id)) {
      # Set the deadlock flag and stop playing
      deadlock_flag <- TRUE
      break
    }
    
    # In case there is no deadlock, reshuffle the discard pile
    step1 <- shuffle_discard_pile(step1)
  }
  
  step1[["time_taken"]] <- toc(quiet = TRUE)
  
  # Add the information about deadlock to the game state
  step1$was_there_deadlock <- deadlock_flag
  
  return(step1)
}
#----------


set.seed(2023)
game <- initialize_game(num_cards = 40, number_of_players = 1, min_discard_pile = 1) |>
      shuffle_discard_pile()

no_reshuffle <- game |> simulate_game(player_id = "1")


length(no_reshuffle$Players[["1"]])

check_deadlock(no_reshuffle)
reshuf1 <- shuffle_discard_pile(no_reshuffle) |> play_card()
length(reshuf1$Players[["1"]])



Yes_reshuffle <- game |> simulate_game_v2(player_id = "1")
Yes_reshuffle_2 <- game |> simulate_game_v3(player_id = "1") #this one takes less time



#Funcion que incluye reshuffle

game_state = game
player_id = "1"
play_game_with_shuffle <- function(game_state, player_id = "1") {
  players_hands <- game_state$Players[[player_id]]
  
  while (!is.null(players_hands) && nrow(players_hands) > 0) {
    game_state <- play_card(game_state, player_id)
    players_hands <- game_state$Players[[player_id]]
    
    if (is.null(players_hands) || nrow(players_hands) == 0) {
      message("Player's hand is empty.")
      break
    }
    
    # Check if there are playable cards in the player's hand
    top_card <- game_state$Discard_Pile[which.max(game_state$Discard_Pile$z_index), ]
    top_card_values <- c(top_card$values_positive, top_card$values_negative)
    playable_cards <- players_hands |> filter(values_positive %in% top_card_values | values_negative %in% top_card_values)
    
    if (is.null(playable_cards) || nrow(playable_cards) == 0) {
      # Reshuffle the discard pile and continue playing
      game_state <- shuffle_discard_pile(game_state)  # Assuming shuffle is a function to shuffle the discard pile
      message("No playable cards. Reshuffling discard pile.")
    }
  }
  
  return(game_state)
}
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
check_deadlock(game_state = game)

#


