draw_n_cards <- function(n = 73){
  #1. equal probability of drawing a card with numbers 1 trough 10
  
  main_numbers <- round(runif(n = n,min = 1,max = 10))
  main_numbers
  
  #2. equal probability of drawing a card with absolute modifier values of1 trough 3
  modifiers <- round(runif(n, min = 1, max = 3))
  
  #hay que hacer que sea valor absoluto y cuando calcule el valor añadir las columnas +1 y -1 para ambos valores
  
  #3. Output Dataframe
  output <- data.frame(main_numbers = main_numbers, modifiers = modifiers)
  return(output)
}
draw_n_cards_for_m_players <- function(player_number = 2, n_cards_on_deck = 73, discard_pile_minimum = 1){
  
  #There's always one card on the discard pile
  cards_to_discard_pile <- discard_pile_minimum
  #After accounting for the card on the discard pile, theres a new number of available cards
  available_cards <- n_cards_on_deck - cards_to_discard_pile
  #Each player will be have the following cards
  cards_for_each_player <- round(available_cards/player_number)
  #When there's an uneven number of platers,more cards will be added to the discard pile
  cards_to_discard_pile <- cards_to_discard_pile + (available_cards %% player_number)
  
  #Output= Vector that contains the number of cards for each player and for the discard pile
  #0: to the discard pile
  #1 to n: number of players
  Vec <- c(rep(0, each = cards_to_discard_pile),
           rep(1:player_number, each = cards_for_each_player))
  return(Vec)
}
fn_anon <- function(n_cards_on_deck = 73, number_of_players = 2, initial_discard_pile = 1){
  #Generate 73 cards 
  df <- draw_n_cards(n = n_cards_on_deck)
  #Decide which cards are to whom
  card_holder <- draw_n_cards_for_m_players(player_number = number_of_players,n_cards_on_deck = n_cards_on_deck,discard_pile_minimum = initial_discard_pile)
  #Assign the cards to each card holder 
  df <- df |> dplyr::mutate(
    card_holder = card_holder
  )
  #Turn df into a list
  df_list <- split(df, df$card_holder)
  return(df_list)
}
suffle_discard_pile <- function(df_list, debug_mode = F){
  if(debug_mode == T){
    #Debug mode adds the index in the original discard pile (it should be different when shuffled)
    #Original data: uses the "0" because this represents the discard pile
    original <- df_list[["0"]] |> dplyr::mutate(index = seq_along(original$card_holder))
    original_shuffled <- original |> dplyr::sample_n(size = nrow(original),replace = F)
    
    #Dont change the discard pile, add the original discard pile with index and with shuffling
    df_list[["original"]] <- original
    df_list[["original_shuffled"]] <- original_shuffled
    
  } else{
    original <- df_list[["0"]]
    original_shuffled <- original |> dplyr::sample_n(size = nrow(original),replace = F)
    #Add a z-index to determine which card is on top (higher z-index means it is closer to the top, lower is lower)
    original_shuffled <- original_shuffled |> dplyr::mutate(z_index = seq_along(original_shuffled$card_holder))
    
    #Only change the discard pile
    df_list[["0"]] <- original_shuffled
  }
  return(df_list)
}
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
fn_anon2 <- function(player_id = "1", my_hand_input_df = game_after_shuffling[["1"]],discard_pile_input_df = game_after_shuffling[["0"]]){
  #1. Grab the card on the highest z-index in the discard pile
  discard_pile <- discard_pile_input_df 
  top_Card_on_discard_pile <- discard_pile[which.max(game_after_shuffling[["0"]]$z_index), ]
  #2. Look at my hand
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
    output <- list("0" = discard_pile_after)
    output[[player_id]] <-  my_hand_after
    output[["card_to_play"]] <-  "There is a card to play"
    
  } else{
    card_to_play <- NULL
    print("There is no possible play")
    
    #Return a list
    output <- list("0" = discard_pile_input_df)
    output[[player_id]] <-  my_hand_input_df
    output[["card_to_play"]] <-  "There is no possible play"
  }
  return(output)
}
fn_anon3 <- function(player_number,card_original_setup, debug_mode = F){
  #Stores the state of the players deck and the discard pile trough time
  steps_list <- list()
  # Initial values for my_hand_input_df and discard_pile_input_df
  my_hand_df <- card_original_setup[[player_number]]
  discard_pile_df <- card_original_setup[["0"]]
  
  #record how many cards the player has played
  player_has_played_n_cards <- 0
  repeat {
    
    
    # Call fn_anon2 with current inputs
    step_output <- fn_anon2(my_hand_input_df = my_hand_df, discard_pile_input_df = discard_pile_df)
    #check if there was a card to play
    if(step_output$card_to_play == "There is a card to play"){
      
      
      if(debug_mode == T){
        print(paste0("It is Player N°{", player_number, "} turn"))  
      }
      
      # Increment the number of cards played
      player_has_played_n_cards <- player_has_played_n_cards + 1
      #State how many cards has the player played so far
      if(debug_mode == T){
      print(paste0("Player N°{",player_number,"} has played {", player_has_played_n_cards, "} many times so far"))
      }
      #.................
      # Store the step's output in the list
      steps_list$ith_state[[player_has_played_n_cards]] <- step_output
      # Update my_hand_df and discard_pile_df for the next iteration
      my_hand_df <- step_output[["1"]]
      discard_pile_df <- step_output[["0"]]
      #Store how many times the player has played
      steps_list[["number_of_turns"]] <- player_has_played_n_cards
    } else{
      
      if(debug_mode == T){
      print(paste0("Player N°{", player_number, "} has stopped playing"))
      print(paste0("Player N°{", player_number, "}, played {", player_has_played_n_cards, "} cards in total"))
      }
      #Store how many times the player has played
      steps_list[["number_of_turns"]] <- player_has_played_n_cards
      break
    }
  }
  return(steps_list)  
}