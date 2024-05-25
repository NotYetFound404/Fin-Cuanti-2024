#-------------------------
draw_n_cards <- function(n = 73){
  main_numbers <- round(runif(n = n,min = 1,max = 10))
  modifiers <- round(runif(n, min = 1, max = 3))
  output <- data.frame(main_numbers = main_numbers, modifiers = modifiers)
  return(output)
}
#-------------------------
draw_n_cards_for_m_players <- function(player_number = 2, n_cards_on_deck = 73, discard_pile_minimum = 1){
  total_cards_excluding_discard <- n_cards_on_deck - discard_pile_minimum
  cards_per_player <- total_cards_excluding_discard %/% player_number
  remaining_cards_for_discard <- n_cards_on_deck - cards_per_player*player_number
  Vec <- c(rep(0, times = remaining_cards_for_discard),
           rep(1:player_number, each = cards_per_player))
  return(Vec)
}

draw_n_cards_for_m_players(player_number = 2, n_cards_on_deck = 5, discard_pile_minimum = 1)

#-------------------------
fn_anon <- function(n_cards_on_deck = 73, number_of_players = 2, initial_discard_pile = 1){
  # Generate 73 cards 
  df <- draw_n_cards(n = n_cards_on_deck)
  # Decide which cards are assigned to whom
  card_holder <- draw_n_cards_for_m_players(player_number = number_of_players, n_cards_on_deck = n_cards_on_deck, discard_pile_minimum = initial_discard_pile)
  # Group the dataframe by card_holder and split into list
  df_split <- split(df, card_holder)
  # Create output list with discard pile and players' hands
  output <- list(
    "Discard_Pile" = df_split[[1]],  # Using [[0]] for the discard pile (card_holder = 0)
    "Players" = df_split[-1]  # Rest of the groups correspond to players
  )
  return(output)
}
fn_anon_mod <- function(n_cards_on_deck = 73, number_of_players = 2, initial_discard_pile = 1){
  # Generate 73 cards 
  df <- draw_n_cards(n = n_cards_on_deck)
  #Calculate the cards values before-hand
  df$values_positive <- df$main_numbers + df$modifiers
  df$values_negative <- df$main_numbers - df$modifiers
  df$values_positive <- ifelse(df$values_positive > 10, 1, df$values_positive)
  df$values_negative <- ifelse(df$values_negative < 0, 8, df$values_negative)
  
  # Add a unique identifier column
  df$card_id <- seq_len(nrow(df))
  
  
  # Decide which cards are assigned to whom
  card_holder <- draw_n_cards_for_m_players(player_number = number_of_players, n_cards_on_deck = n_cards_on_deck, discard_pile_minimum = initial_discard_pile)
  # Group the dataframe by card_holder and split into list
  df_split <- split(df, card_holder)
  # Create output list with discard pile and players' hands
  output <- list(
    "Discard_Pile" = df_split[[1]],  # Using [[0]] for the discard pile (card_holder = 0)
    "Players" = df_split[-1]  # Rest of the groups correspond to players
  )
  return(output)
}

#-------------------------
suffle_discard_pile <- function(df_list){
  df_list$Discard_Pile <- sample(df_list$Discard_Pile, replace = FALSE)
  df_list$Discard_Pile$z_index <- seq_len(nrow(df_list$Discard_Pile))
  return(df_list)
}
fn_anon2 <- function(game_setup, player_id = "1"){
  # Grab the card on the highest z-index in the discard pile
  top_card <- game_setup$Discard_Pile[which.max(game_setup$Discard_Pile$z_index), ]
  
  # Calculate values for my hand and the top card
  my_hand <- game_setup$Players[[player_id]] |> calc_value()
  top_card <- top_card |> calc_value()
  
  # Identify playable cards
  cards_to_match <- c(top_card$Value_positive, top_card$Value_negative)
  playable_cards <- my_hand %>%
    filter(Value_positive %in% cards_to_match | Value_negative %in% cards_to_match)
  
  if(nrow(playable_cards) > 0){
    # Select the first playable card
    card_to_play <- playable_cards[1, ]
    
    # Remove played card and perform cleanup
    my_hand_after <- my_hand[!my_hand$main_numbers %in% card_to_play$main_numbers, ]
    my_hand_after <- rm_calc(my_hand_after)
    card_to_play <- rm_calc(card_to_play)
    
    # Adjust the z-index and add the card to the discard pile
    card_to_play$z_index <- top_card$z_index + 1
    discard_pile_after <- bind_rows(card_to_play, game_setup$Discard_Pile)
    
    # Update game_setup with new state
    output <- game_setup
    output$Discard_Pile <- discard_pile_after
    output$Players[[player_id]] <- my_hand_after
    output$card_to_play <- "There is a card to play"
    
  } else {
    print("There is no possible play")
    output <- game_setup
    output$card_to_play <- "There is no possible play"
  }
  
  return(output)
}
#------------
fn_anon2_mod <- function(game_setup, player_id = "1") {
  # Extract the discard pile and players' hands from game_setup
  discard_pile <- game_setup$Discard_Pile
  players_hands <- game_setup$Players[[player_id]]
  
  # Grab the card on the highest z-index in the discard pile
  top_card <- game_setup$Discard_Pile[which.max(game_setup$Discard_Pile$z_index), ]
  
  # Find playable cards based on the top card on the discard pile
  top_card_values <- c(top_card$Value_positive, top_card$Value_negative)
  #top_card_values <- c(discard_pile$values_positive, discard_pile$values_negative)
  playable_cards <- players_hands %>%
    filter(values_positive %in% top_card_values | values_negative %in% top_card_values)
  
  if (nrow(playable_cards) > 0) {
    # Select the first playable card
    card_to_play <- playable_cards[1, ]
    
    # Remove the played card from the player's hand
    players_hands_after <- players_hands %>%
      anti_join(card_to_play, by = c("main_numbers", "modifiers"))
    
    # Add the selected card to the discard pile
    discard_pile_after <- bind_rows(card_to_play, discard_pile) %>%
      mutate(z_index = max(discard_pile$z_index) + 1)
    
    # Update game_setup with the modified discard pile and player's hand
    game_setup$Discard_Pile <- discard_pile_after
    game_setup$Players[[player_id]] <- players_hands_after
    
    output <- list(game_setup, "card_to_play" = "There is a card to play")
  } else {
    output <- list(game_setup, "card_to_play" = "There is no possible play")
  }
  
  return(output)
}

#-------------------
calc_value <- function(df){
  output <- df |> dplyr::mutate(
    modifier_positive = modifiers,
    modifier_negative = -modifiers,
    Value_positive = main_numbers + modifier_positive,
    Value_negative = main_numbers + modifier_negative) |> dplyr::mutate(
      #Case when there is a positive overflow
      Value_positive = ifelse(Value_positive > 10, 1,Value_positive),
      #Case when there is a negative overflow
      Value_negative = ifelse(Value_positive < 0, 8,Value_negative)
    )
  return(output)
}
rm_calc <- function(df){
  output <- df |> dplyr::select(
    -modifier_positive,
    -modifier_negative,
    -Value_positive,
    -Value_negative)
  return(output)
}
fn_anon2_og <- function(game_setup, player_id = "1"){
  #1. Grab the card on the highest z-index in the discard pile
  discard_pile <- game_setup$Discard_Pile
  top_Card_on_discard_pile <- discard_pile[which.max(game_setup$Discard_Pile$z_index), ]
  #2. Look at my hand
  my_hand_input_df <- game_setup$Players[[player_id]]
  my_hand <- my_hand_input_df |> calc_value()
  #3. Calculate the values on the top
  top_Card_on_discard_pile <- top_Card_on_discard_pile |> calc_value()
  #4. Store playable cards
  #4.1 Posible Cards to match belong to card on top
  cards_to_match <- c(top_Card_on_discard_pile$Value_positive,
                      top_Card_on_discard_pile$Value_negative)
  #4.2 Cards in my deck that match either value of the card on top's main number +/- its modifier
  playable_cards <- my_hand |> dplyr::filter(
    Value_positive %in% cards_to_match | Value_negative %in% cards_to_match)
  #4.3 Select the card that will be played
  #Selection criteria = the first that comes up if there are any
  if(nrow(playable_cards) > 0){
    #select card to play
    card_to_play <- dplyr::first(playable_cards)
    # Remove played card
    my_hand_after <- dplyr::anti_join(my_hand, card_to_play, by = c("main_numbers", "modifiers"))
    # Remove calculation on my hand
    my_hand_after <- rm_calc(df = my_hand_after)
    #rm calculation on card_to play
    card_to_play <- rm_calc(card_to_play)
    # Add the selected card to the discard pile
    card_to_play <- card_to_play |> dplyr::mutate(
      z_index = top_Card_on_discard_pile$z_index + 1 #Adjust the z-index
    )
    discard_pile_after <- dplyr::bind_rows(card_to_play, discard_pile)
    
    #Return a list
    output <- game_setup
    output[["Discard_Pile"]] <-  discard_pile_after
    output$Players[[player_id]] <- my_hand_after
    output[["card_to_play"]] <-  "There is a card to play"
    
  } else{
    card_to_play <- NULL
    print("There is no possible play")
    output <- game_setup
    output[["card_to_play"]] <-  "There is no possible play"
  }
  return(output)
}
#-------------------------
detailed_function <- function(game_setup, player_id) {
  library(tictoc)
  # Start the timer
  tic()
  
  # Initialize variables
  cards_played <- 0
  
  # Store the initial state
  initial_state <- game_setup
  
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
  
  # Stop the timer and store the elapsed time
  time_elapsed <- toc()
  
  # Return a list with initial state, final state, cards_played, and time_elapsed
  return(list(initial_state = initial_state, final_state = step1, cards_played = cards_played, time_elapsed = time_elapsed))
}
#-------------------------
quick_function <- function(game_setup, player_id){
  # Perform the first step
  step1 <- fn_anon2(game_setup = game_after_shuffling, player_id = player_id)
  # Repeat until there is no possible play
  repeat {
    # Check if the card to play is "There is no possible play"
    if (step1$card_to_play == "There is no possible play") {
      # Exit the loop if there's no possible play
      break
    } else {
      # Perform the next step
      step1 <- fn_anon2(game_setup = step1, player_id = player_id)
    }
  }
  return(step1)
}
#------------
quick_function_mod <- function(game_setup, player_id){
  # Perform the first step
  step1 <- fn_anon2_mod(game_setup = game_after_shuffling, player_id = player_id)
  # Repeat until there is no possible play
  repeat {
    # Check if the card to play is "There is no possible play"
    if (step1$card_to_play == "There is no possible play") {
      # Exit the loop if there's no possible play
      break
    } else {
      # Perform the next step
      step1 <- fn_anon2_mod(game_setup = step1, player_id = player_id)
    }
  }
  return(step1)
}