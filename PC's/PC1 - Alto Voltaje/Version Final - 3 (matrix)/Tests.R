
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

#test 5----------
# Define the number of repetitions for the benchmark


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
