#--------------------
# # Define the number of iterations
# num_iterations <- 10
# # Initialize a vector to store the time taken for each run
# time_taken <- numeric(num_iterations)
# # Loop through the iterations
# for (i in 1:num_iterations) {
#   # Your code here (replace this with your actual code)
#   source(paste0(getwd(), "/PC's/PC1 - Alto Voltaje/attempt 3/functions.R"))
#   game_before_shuffling <- fn_anon(n_cards_on_deck = 73, number_of_players = 1, initial_discard_pile = 1)
#   game_after_shuffling <- suffle_discard_pile(df_list = game_before_shuffling)
#   game <- detailed_function(game_setup = game_after_shuffling, player_id = "1")
#   
#   # Stop the timer and store the elapsed time in the vector
#   time_taken[i] <- game$time_elapsed$toc
# }
# 
# # Calculate the average time taken
# average_time <- mean(time_taken)
# average_time
#---------------------
# source(paste0(getwd(), "/PC's/PC1 - Alto Voltaje/attempt 3/functions.R"))
# game_before_shuffling <- fn_anon(n_cards_on_deck = 73, number_of_players = 1, initial_discard_pile = 1)
# game_after_shuffling <- suffle_discard_pile(df_list = game_before_shuffling)
# #step 1 (version original).....
# step1 <- fn_anon2(game_setup = game_after_shuffling, player_id = "1")
#----------
#precalculated (this is faster)
source(paste0(getwd(), "/PC's/PC1 - Alto Voltaje/attempt 3/functions.R"))
game_before_shuffling <- fn_anon_mod(n_cards_on_deck = 73, number_of_players = 1, initial_discard_pile = 1)
game_after_shuffling <- suffle_discard_pile(df_list = game_before_shuffling)
#step1 <- fn_anon2_mod(game_setup = game_after_shuffling, player_id = "1")
tic()
game <- quick_function_mod(game_setup = game_after_shuffling, player_id = "1")
toc()






























