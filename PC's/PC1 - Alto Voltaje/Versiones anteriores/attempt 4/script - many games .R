library(tictoc)
source(paste0(getwd(), "/PC's/PC1 - Alto Voltaje/attempt 4/functions.R"))

n_sim = 100
time_vector = numeric(n_sim)
for(i in 1:n_sim){
  #beging recording
  tic()
  #simulate the game
  game <- initialize_game(num_cards = 6, number_of_players = 1, min_discard_pile = 1) |> shuffle_discard_pile() |> simulate_game(player_id = "1")
  #record how much time it takes
  compute_time <- toc()
  time_vector <- c(time_vector,compute_time$toc)
}
time_vector_og <- time_vector
mean(time_vector)

