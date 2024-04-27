library(bench)
library(dplyr)
#test 1--------------
# # Define the number of repetitions for the benchmark
# num_reps <- 1e4
# n <- 73
# 
# # Benchmark the sample operation
# test <- bench::mark(
#   sample(1:10, n, replace = TRUE),
#   round(runif(n = n, min = 1, max = 10)),
#   iterations = num_reps,
#   check = FALSE
# )
# test
# #According to the test, round(runif is better)
#test 2--------------
#Which data structure is faster? Is it datatable, tible or dataframe ?
# num_reps <- 1e4
# n <- 73
# draw_n_cards_df <- function(n = 73){
#   #1. equal probability of drawing a card with numbers 1 trough 10
#   main_numbers <- round(runif(n = n,min = 1,max = 10))
#   #2. equal probability of drawing a card with absolute modifier values of1 trough 3
#   modifiers <- round(runif(n, min = 1, max = 3))
#   #3. Output Dataframe
#   output <- data.frame(main_numbers = main_numbers, modifiers = modifiers)
#   return(output)
# }
# draw_n_cards_tb <- function(n = 73){
#   library(dplyr)
#   #1. equal probability of drawing a card with numbers 1 trough 10
#   main_numbers <- round(runif(n = n,min = 1,max = 10))
#   #2. equal probability of drawing a card with absolute modifier values of1 trough 3
#   modifiers <- round(runif(n, min = 1, max = 3))
#   #3. Output Dataframe
#   output <- tibble(main_numbers = main_numbers, modifiers = modifiers)
#   return(output)
# }
# draw_n_cards_dt <- function(n = 73){
#   library(data.table)
#   #1. equal probability of drawing a card with numbers 1 trough 10
#   main_numbers <- round(runif(n = n,min = 1,max = 10))
#   #2. equal probability of drawing a card with absolute modifier values of1 trough 3
#   modifiers <- round(runif(n, min = 1, max = 3))
#   #3. Output Dataframe
#   output <- data.table(main_numbers = main_numbers, modifiers = modifiers)
#   return(output)
# }
# test <- bench::mark(
#   draw_n_cards_df(n = n),
#   draw_n_cards_tb(n = n),
#   draw_n_cards_dt(n = n),
#   iterations = num_reps,
#   check = FALSE
# )
# test
#Using df is the fastest
#test3------------
# num_reps <- 1e4
# n <- 73
# draw_n_cards_1 <- function(n = 73){
#   main_numbers <- round(runif(n = n,min = 1,max = 10))
#   modifiers <- round(runif(n, min = 1, max = 3))
#   output <- data.frame(main_numbers = main_numbers, modifiers = modifiers)
#   return(output)
# }
# draw_n_cards_2 <- function(n = 73){return(data.frame(main_numbers = round(runif(n = n,min = 1,max = 10)), modifiers = round(runif(n, min = 1, max = 3))))}
# # Benchmark the sample operation
# test <- bench::mark(
#   draw_n_cards_1(n),
#   draw_n_cards_2(n),
#   iterations = num_reps,
#   check = FALSE
# )
#draw_n_cards_1 is better and more readable
#test 4------------
# Define the number of repetitions for the benchmark
# num_reps <- 3e4
# player_number = 2
# n_cards_on_deck = 73
# discard_pile_minimum = 1
# draw_n_cards_for_m_players <- function(player_number = 2, n_cards_on_deck = 73, discard_pile_minimum = 1){
#   
#   #There's always one card on the discard pile
#   cards_to_discard_pile <- discard_pile_minimum
#   #After accounting for the card on the discard pile, theres a new number of available cards
#   available_cards <- n_cards_on_deck - cards_to_discard_pile
#   #Each player will be have the following cards
#   cards_for_each_player <- floor(available_cards/player_number)
#   #When there's an uneven number of platers,more cards will be added to the discard pile
#   cards_to_discard_pile <- cards_to_discard_pile + (available_cards %% player_number)
#   
#   #Output= Vector that contains the number of cards for each player and for the discard pile
#   #0: to the discard pile
#   #1 to n: number of players
#   Vec <- c(rep(0, each = cards_to_discard_pile),
#            rep(1:player_number, each = cards_for_each_player))
#   return(Vec)
# }
# draw_n_cards_for_m_players_2 <- function(player_number = 2, n_cards_on_deck = 73, discard_pile_minimum = 1){
#   # Calculate the total number of cards excluding the discard pile
#   total_cards_excluding_discard <- n_cards_on_deck - discard_pile_minimum
#   
#   # Calculate the number of cards for each player and the remaining cards for the discard pile
#   cards_per_player <- total_cards_excluding_discard %/% player_number
#   remaining_cards_for_discard <- total_cards_excluding_discard %% player_number + discard_pile_minimum
#   
#   # Generate the vector indicating the number of cards for each player and the discard pile
#   Vec <- c(rep(0, times = remaining_cards_for_discard),
#            rep(1:player_number, each = cards_per_player))
#   
#   return(Vec)
# }
# draw_n_cards_for_m_players_3 <- function(player_number = 2, n_cards_on_deck = 73, discard_pile_minimum = 1){
#   
#   # Calculate the total number of cards excluding the discard pile
#   total_cards_excluding_discard <- n_cards_on_deck - discard_pile_minimum
#   
#   # Calculate the number of cards for each player and the remaining cards for the discard pile
#   cards_per_player <- total_cards_excluding_discard %/% player_number
#   remaining_cards_for_discard <- n_cards_on_deck - cards_per_player*player_number
#   
#   # Generate the vector indicating the number of cards for each player and the discard pile
#   Vec <- c(rep(0, times = remaining_cards_for_discard),
#            rep(1:player_number, each = cards_per_player))
#   return(Vec)
# }
# draw_n_cards_for_m_players_4 <- function(player_number = 2, n_cards_on_deck = 73, discard_pile_minimum = 1){
#   # Calculate the total number of cards excluding the discard pile
#   total_cards_excluding_discard <- n_cards_on_deck - discard_pile_minimum
#   
#   # Calculate the number of cards for each player and the remaining cards for the discard pile
#   cards_per_player <- total_cards_excluding_discard %/% player_number
#   remaining_cards_for_discard <- total_cards_excluding_discard %% player_number
#   
#   # Generate the vector indicating the number of cards for each player and the discard pile
#   Vec <- c(rep(0, times = discard_pile_minimum),
#            rep(1:player_number, each = cards_per_player))
#   
#   # Add remaining cards to the discard pile
#   Vec[1:remaining_cards_for_discard] <- 0
#   
#   return(Vec)
# }
# 
# test <- bench::mark(
#   draw_n_cards_for_m_players(player_number = player_number,n_cards_on_deck =n_cards_on_deck, discard_pile_minimum = discard_pile_minimum),
#   draw_n_cards_for_m_players_2(player_number = player_number,n_cards_on_deck =n_cards_on_deck, discard_pile_minimum = discard_pile_minimum),
#   draw_n_cards_for_m_players_3(player_number = player_number,n_cards_on_deck =n_cards_on_deck, discard_pile_minimum = discard_pile_minimum),
#   draw_n_cards_for_m_players_4(player_number = player_number,n_cards_on_deck =n_cards_on_deck, discard_pile_minimum = discard_pile_minimum),
#   iterations = num_reps,
#   check = FALSE
# )
# test
#the third version is the fastest, We will select this one

#test 5----------
# Define the number of repetitions for the benchmark
# num_reps <- 1e3
# n <- 73
# fn_anon <- function(n_cards_on_deck = 73, number_of_players = 2, initial_discard_pile = 1){
#   #Generate 73 cards 
#   df <- draw_n_cards(n = n_cards_on_deck)
#   #Decide which cards are to whom
#   card_holder <- draw_n_cards_for_m_players(player_number = number_of_players,n_cards_on_deck = n_cards_on_deck,discard_pile_minimum = initial_discard_pile)
#   #Assign the cards to each card holder 
#   df <- df |> dplyr::mutate(
#     card_holder = card_holder,
#     type = ifelse(card_holder == 0, "Discard_Pile", "Player")
#   )
#   
#   #Split discard pile and players
#   df_discard_pile <- df |> dplyr::filter(type == "Discard_Pile")
#   df_players <- df |> dplyr::filter(type != "Discard_Pile")
#   #Split the players hand into a list
#   df_players_list <- split(df_players, df_players$card_holder)
#   #output: join the discard pile and the players list (that contains a df = this players hand)
#   output <- list("Discard_Pile" = df_discard_pile,
#                  "Players" = df_players_list) 
#   return(output)
# }
# fn_anon_1 <- function(n_cards_on_deck = 73, number_of_players = 2, initial_discard_pile = 1){
#   # Generate 73 cards 
#   df <- draw_n_cards(n = n_cards_on_deck)
#   
#   # Decide which cards are assigned to whom
#   card_holder <- draw_n_cards_for_m_players(player_number = number_of_players, n_cards_on_deck = n_cards_on_deck, discard_pile_minimum = initial_discard_pile)
#   
#   # Combine mutations and filter in one step
#   df <- df %>%
#     mutate(
#       card_holder = card_holder,
#       type = ifelse(card_holder == 0, "Discard_Pile", "Player")
#     )
#   
#   # Split the dataframe into discard pile and players
#   df_split <- df %>%
#     filter(type != "Discard_Pile") %>%
#     group_split(card_holder)
#   
#   # Create output list with discard pile and players' hands
#   output <- list(
#     "Discard_Pile" = df %>%
#       filter(type == "Discard_Pile"),
#     "Players" = df_split
#   )
#   
#   return(output)
# }
# fn_anon_2 <- function(n_cards_on_deck = 73, number_of_players = 2, initial_discard_pile = 1){
#   # Generate 73 cards 
#   df <- draw_n_cards(n = n_cards_on_deck)
#   
#   # Decide which cards are assigned to whom
#   card_holder <- draw_n_cards_for_m_players(player_number = number_of_players, n_cards_on_deck = n_cards_on_deck, discard_pile_minimum = initial_discard_pile)
#   
#   # Group the dataframe by card_holder and split into list
#   df <- cbind(df,card_holder)
#   df_split <- df %>%
#     group_by(card_holder) %>%
#     group_split()
#   
#   # Create output list with discard pile and players' hands
#   output <- list(
#     "Discard_Pile" = df_split[[1]],  # First group corresponds to discard pile (card_holder = 0)
#     "Players" = df_split[-1]  # Rest of the groups correspond to players
#   )
#   
#   return(output)
# }
# fn_anon_3 <- function(n_cards_on_deck = 73, number_of_players = 2, initial_discard_pile = 1){
#   # Generate 73 cards 
#   df <- draw_n_cards(n = n_cards_on_deck)
#   
#   # Decide which cards are assigned to whom
#   card_holder <- draw_n_cards_for_m_players(player_number = number_of_players, n_cards_on_deck = n_cards_on_deck, discard_pile_minimum = initial_discard_pile)
#   
#   # Group the dataframe by card_holder and split into list
#   df_split <- split(df, card_holder)
#   
#   # Create output list with discard pile and players' hands
#   output <- list(
#     "Discard_Pile" = df_split[[1]],  # Using [[0]] for the discard pile (card_holder = 0)
#     "Players" = df_split[-1]  # Rest of the groups correspond to players
#   )
#   
#   return(output)
# }
# n_cards_on_deck = 73
# player_number = 2
# initial_discard_pile = 1
# # Benchmark the sample operation
# test <- bench::mark(
#   fn_anon(number_of_players = player_number,n_cards_on_deck =n_cards_on_deck, initial_discard_pile = initial_discard_pile),
#   fn_anon_1(number_of_players = player_number,n_cards_on_deck =n_cards_on_deck, initial_discard_pile = initial_discard_pile),
#   fn_anon_2(number_of_players = player_number,n_cards_on_deck =n_cards_on_deck, initial_discard_pile = initial_discard_pile),
#   fn_anon_3(number_of_players = player_number,n_cards_on_deck =n_cards_on_deck, initial_discard_pile = initial_discard_pile),
#   iterations = num_reps,
#   check = FALSE
# )
#fn_anon_3 is by far the fastest one
#test 6---------
# Define the number of repetitions for the benchmark
# num_reps <- 1e4
# n <- 73
# df_list = game_before_shuffling
# suffle_discard_pile <- function(df_list){
#   original <- df_list$Discard_Pile
#   original_shuffled <- sample(original, replace = FALSE)
#   original_shuffled <- data.frame(original_shuffled, z_index = seq_along(original_shuffled$main_numbers))
#   df_list$Discard_Pile <- original_shuffled
#   return(df_list)
# }
# suffle_discard_pile_2 <- function(df_list){
#   # Shuffle the discard pile and add a z-index column
#   df_list$Discard_Pile <- sample(df_list$Discard_Pile, replace = FALSE)
#   df_list$Discard_Pile$z_index <- seq_len(nrow(df_list$Discard_Pile))
#   return(df_list)
# }
# suffle_discard_pile_3 <- function(df_list){
#   # Shuffle the discard pile and add a z-index column
#   df_list$Discard_Pile <- df_list$Discard_Pile %>%
#     sample_frac(1) %>%
#     mutate(z_index = row_number())
#   
#   return(df_list)
#   return(df_list)
# }
# 
# test <- bench::mark(
#   suffle_discard_pile(df_list = df_list),
#   suffle_discard_pile_2(df_list = df_list),
#   suffle_discard_pile_3(df_list = df_list),
#   iterations = num_reps,
#   check = FALSE
# )
# test
#the second implementation is the fastest
#test 7---------
# # Define the number of repetitions for the benchmark
# num_reps <- 1e4
# n <- 73
# game_setup = game_after_shuffling
# 
# # Benchmark the sample operation
# test <- bench::mark(
#   game_setup$Discard_Pile[which.max(game_setup$Discard_Pile$z_index), ],
#   slice_max(game_setup$Discard_Pile, order_by = z_index, n = 1),
#   iterations = num_reps,
#   check = FALSE
# )
# test

#test 7---------
# Define the number of repetitions for the benchmark
# num_reps <- 1000
# n <- 73
# game_setup = game_after_shuffling
# player_id = "1"
# 
# 
# calc_value <- function(df){
#   output <- df |> dplyr::mutate(
#     modifier_positive = modifiers,
#     modifier_negative = -modifiers,
#     Value_positive = main_numbers + modifier_positive,
#     Value_negative = main_numbers + modifier_negative) |> dplyr::mutate(
#       #Case when there is a positive overflow
#       Value_positive = ifelse(Value_positive > 10, 1,Value_positive),
#       #Case when there is a negative overflow
#       Value_negative = ifelse(Value_positive < 0, 8,Value_negative)
#     )
#   return(output)
# }
# rm_calc <- function(df){
#   output <- df |> dplyr::select(
#     -modifier_positive,
#     -modifier_negative,
#     -Value_positive,
#     -Value_negative)
#   return(output)
# }
# fn_anon2_og <- function(game_setup, player_id = "1"){
#   #1. Grab the card on the highest z-index in the discard pile
#   discard_pile <- game_setup$Discard_Pile
#   top_Card_on_discard_pile <- discard_pile[which.max(game_setup$Discard_Pile$z_index), ]
#   #2. Look at my hand
#   my_hand_input_df <- game_setup$Players[[player_id]]
#   my_hand <- my_hand_input_df |> calc_value()
#   #3. Calculate the values on the top
#   top_Card_on_discard_pile <- top_Card_on_discard_pile |> calc_value()
#   #4. Store playable cards
#   #4.1 Posible Cards to match belong to card on top
#   cards_to_match <- c(top_Card_on_discard_pile$Value_positive,
#                       top_Card_on_discard_pile$Value_negative)
#   #4.2 Cards in my deck that match either value of the card on top's main number +/- its modifier
#   playable_cards <- my_hand |> dplyr::filter(
#     Value_positive %in% cards_to_match | Value_negative %in% cards_to_match)
#   #4.3 Select the card that will be played
#   #Selection criteria = the first that comes up if there are any
#   if(nrow(playable_cards) > 0){
#     #select card to play
#     card_to_play <- dplyr::first(playable_cards)
#     # Remove played card
#     my_hand_after <- dplyr::anti_join(my_hand, card_to_play, by = c("main_numbers", "modifiers"))
#     # Remove calculation on my hand
#     my_hand_after <- rm_calc(df = my_hand_after)
#     #rm calculation on card_to play
#     card_to_play <- rm_calc(card_to_play)
#     # Add the selected card to the discard pile
#     card_to_play <- card_to_play |> dplyr::mutate(
#       z_index = top_Card_on_discard_pile$z_index + 1 #Adjust the z-index
#     )
#     discard_pile_after <- dplyr::bind_rows(card_to_play, discard_pile)
#     
#     #Return a list
#     output <- game_setup
#     output[["Discard_Pile"]] <-  discard_pile_after
#     output$Players[[player_id]] <- my_hand_after
#     output[["card_to_play"]] <-  "There is a card to play"
#     
#   } else{
#     card_to_play <- NULL
#     print("There is no possible play")
#     output <- game_setup
#     output[["card_to_play"]] <-  "There is no possible play"
#   }
#   return(output)
# }
# fn_anon2 <- function(game_setup, player_id = "1"){
#   # Grab the card on the highest z-index in the discard pile
#   top_card <- game_setup$Discard_Pile[which.max(game_setup$Discard_Pile$z_index), ]
#   
#   # Calculate values for my hand and the top card
#   my_hand <- game_setup$Players[[player_id]] |> calc_value()
#   top_card <- top_card |> calc_value()
#   
#   # Identify playable cards
#   cards_to_match <- c(top_card$Value_positive, top_card$Value_negative)
#   playable_cards <- my_hand %>%
#     filter(Value_positive %in% cards_to_match | Value_negative %in% cards_to_match)
#   
#   if(nrow(playable_cards) > 0){
#     # Select the first playable card
#     card_to_play <- playable_cards[1, ]
#     
#     # Remove played card and perform cleanup
#     my_hand_after <- my_hand[!my_hand$main_numbers %in% card_to_play$main_numbers, ]
#     my_hand_after <- rm_calc(my_hand_after)
#     card_to_play <- rm_calc(card_to_play)
#     
#     # Adjust the z-index and add the card to the discard pile
#     card_to_play$z_index <- top_card$z_index + 1
#     discard_pile_after <- bind_rows(card_to_play, game_setup$Discard_Pile)
#     
#     # Update game_setup with new state
#     output <- game_setup
#     output$Discard_Pile <- discard_pile_after
#     output$Players[[player_id]] <- my_hand_after
#     output$card_to_play <- "There is a card to play"
#     
#   } else {
#     print("There is no possible play")
#     output <- game_setup
#     output$card_to_play <- "There is no possible play"
#   }
#   
#   return(output)
# }
# 
# fn_anon2_2 <- function(game_setup, player_id = "1"){
#   # Grab the card on the highest z-index in the discard pile
#   top_card <- game_setup$Discard_Pile[which.max(game_setup$Discard_Pile$z_index), ]
#   
#   # Calculate values for my hand and the top card
#   my_hand <- game_setup$Players[[player_id]] |> calc_value()
#   top_card <- top_card |> calc_value()
#   
#   # Identify playable cards
#   cards_to_match <- c(top_card$Value_positive, top_card$Value_negative)
#   playable_cards <- my_hand %>%
#     filter(Value_positive %in% cards_to_match | Value_negative %in% cards_to_match)
#   
#   if(nrow(playable_cards) > 0){
#     # Select the first playable card
#     card_to_play <- playable_cards[1, ]
#     
#     # Remove played card and perform cleanup
#     my_hand_after <- my_hand[!my_hand$main_numbers %in% card_to_play$main_numbers, ]
#     my_hand_after <- rm_calc(my_hand_after)
#     card_to_play <- rm_calc(card_to_play)
#     
#     # Adjust the z-index and add the card to the discard pile
#     card_to_play$z_index <- top_card$z_index + 1
#     discard_pile_after <- bind_rows(card_to_play, game_setup$Discard_Pile)
#     
#     # Update game_setup with new state
#     output <- game_setup
#     output$Discard_Pile <- discard_pile_after
#     output$Players[[player_id]] <- my_hand_after
#     output$card_to_play <- "There is a card to play"
#     
#   } else {
#     print("There is no possible play")
#     output <- game_setup
#     output$card_to_play <- "There is no possible play"
#   }
#   
#   return(output)
# }
# 
# test <- bench::mark(
#   iterations = num_reps,
#   fn_anon2_og(game_setup = game_after_shuffling, player_id = "1"),
#   fn_anon2(game_setup = game_after_shuffling, player_id = "1"),
#   check = FALSE
# )
# test
#fn_anon2 is prefered to the og version
# Test 8--------------
# source(paste0(getwd(), "/PC's/PC1 - Alto Voltaje/attempt 3/functions.R"))
# library(bench)
# # Define the number of repetitions for the benchmark
# num_reps <- 100
# # Benchmark the original version of fn_anon2
# bench_original <- bench::mark(
#   game_before_shuffling <- fn_anon(n_cards_on_deck = 73, number_of_players = 1, initial_discard_pile = 1),
#   game_after_shuffling <- suffle_discard_pile(df_list = game_before_shuffling),
#   step1_original = fn_anon2(game_setup = game_after_shuffling, player_id = "1"),
#   iterations = num_reps,
#   check = FALSE
# )
# # Benchmark the precalculated version of fn_anon2_mod
# bench_precalculated <- bench::mark(
#   game_before_shuffling <- fn_anon_mod(n_cards_on_deck = 73, number_of_players = 1, initial_discard_pile = 1),
#   game_after_shuffling <- suffle_discard_pile(df_list = game_before_shuffling),
#   step1_precalculated = fn_anon2_mod(game_setup = game_after_shuffling, player_id = "1"),
#   iterations = num_reps,
#   check = FALSE
# )
# 
# sum(bench_original$median)
# sum(bench_precalculated$median)
# #the precalculated one is way faster
# sum(bench_original$min)
# sum(bench_precalculated$min)
