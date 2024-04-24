library(bench)
library(tictoc)
source(paste0(getwd(), "/PC's/PC1 - Alto Voltaje/Version Final - 1 (vectorizado)/optimized_functions.R"))

game_state <- initialize_game(num_cards = 3, number_of_players = 1, min_discard_pile = 1) |> 
  shuffle_discard_pile() 
player_id = "1"

a <- simulate_game(game_state,player_id)
game_state
a


updated_simulate_game <- function(game_state, player_id) {
  while (TRUE) {
    step <- play_card(game_state = game_state, player_id = player_id)
    if (step$card_to_play == "There is no possible play") {
      return(step)
      break
    }
    return(step)
  }
  
}


tic()
p <- simulate_game(game_state, player_id)
toc()

tic()
q <- updated_simulate_game(game_state, player_id)
toc()

game_state
p
q

test <- bench::mark(
  simulate_game(game_state, player_id),
  updated_simulate_game(game_state, player_id),
  iterations = 10
)

print(test)
