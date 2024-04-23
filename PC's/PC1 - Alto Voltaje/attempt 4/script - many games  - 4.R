#-----------
#Optimizing R code:
#Source: https://www.blasbenito.com/post/02_parallelizing_loops_with_r/
# n.cores <- length(parallelly::availableWorkers()) - 1
# #create the cluster
# my.cluster <- parallel::makeCluster(
#   n.cores, 
#   type = "PSOCK" #FORK is better but for UNIX
# )
# #check cluster definition (optional)
# print(my.cluster)
# #register it to be used by %dopar%
# doParallel::registerDoParallel(cl = my.cluster)
# #check if it is registered (optional)
# foreach::getDoParRegistered()
# library(doParallel)
# #example
# x <- foreach(
#   i = 1:10, 
#   .combine = 'c'
# ) %dopar% {
#   sqrt(i)
# }
# x
# parallel::stopCluster(cl = my.cluster)
#-------------
library(foreach)
library(doParallel)

# Set up parallel processing
n.cores <- length(parallelly::availableWorkers()) - 1
my.cluster <- parallel::makeCluster(n.cores, type = "PSOCK")
registerDoParallel(cl = my.cluster)

# Load external functions if needed
source(paste0(getwd(), "/PC's/PC1 - Alto Voltaje/attempt 4/functions.R"))

# Number of simulations
n_sim <- 100

# Simulation loop
time_vector <- foreach(i = 1:n_sim, .combine = 'c') %dopar% {
  tictoc::tic()
  game <- initialize_game(num_cards = 6, number_of_players = 1, min_discard_pile = 1) |> 
    shuffle_discard_pile() |> 
    simulate_game(player_id = "1")
  tictoc::toc()$toc
}

# Stop cluster and compute mean time
stopCluster(my.cluster)
mean_time <- mean(time_vector)
mean_time