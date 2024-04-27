##1##

# Función para generar las cartas del juego
data <- function(num_cards = 73) {
  # Crear una secuencia de números principales de 1 a 10
  main_numbers <- sample(1:10, num_cards, replace = TRUE)
  # Crear una secuencia de números modificadores de 1 a 3
  modifiers <- sample(1:3, num_cards, replace = TRUE)
  # Crear el dataframe con las variables main number y modifier
  card_df <- data.frame(main_number = main_numbers, modifier = modifiers)
  return(card_df)
}

# Ejemplo de uso
card_data <- data()

##2##
# Función para calcular la distribución de cartas entre los jugadores y la pila de descarte

crear_reparto_cartas <- function(num_jugadores = 2) {
  # Calcular el número total de cartas
  num_cartas_total <- 73
  # Calcular el número de cartas en la pila de descarte (mínimo = 1)
  num_cartas_descarte <- max(1, num_cartas_total %% num_jugadores)
  # Calcular el número de cartas en las manos de cada jugador
  num_cartas_por_jugador <- (num_cartas_total - num_cartas_descarte) %/% num_jugadores
  # Crear un vector con el número de cartas para cada jugador y la pila de descarte
  reparto_cartas <- c(rep(num_cartas_por_jugador, num_jugadores), num_cartas_descarte)
  return(reparto_cartas)
}

# Ejemplo de uso
reparto <- crear_reparto_cartas()
reparto

##3##
# Función para crear el estado inicial del juego

create_game_state <- function(card_data, reparto) {
  # Barajar el dataframe de cartas
  shuffled_cards <- card_data[sample(nrow(card_data)), ]
  
  # Crear una lista para contener las manos de los jugadores
  players_hands <- list()
  
  # Repartir las cartas a los jugadores
  start_index <- 1
  for (i in 1:(length(reparto) - 1)) {
    end_index <- start_index + reparto[i] - 1
    players_hands[[paste0("Player", i)]] <- shuffled_cards[start_index:end_index, ]
    start_index <- end_index + 1
  }
  
  # Crear un dataframe para la pila de descarte
  discard_pile <- shuffled_cards[start_index:nrow(shuffled_cards), ]
  
  # Crear una lista para contener el estado del juego
  game_state <- list(Players = players_hands, "Discard Pile" = discard_pile)
  
  return(game_state)
}

# Ejemplo de uso
card_data <- data()
reparto <- crear_reparto_cartas()

estado_juego <- create_game_state(card_data, reparto)
estado_juego

##4##
# Función para barajar la pila de descarte


shuffle_discard_pile <- function(estado_juego) {
  # Extraer la pila de descarte del estado del juego
  discard_pile <- estado_juego$"Discard Pile"
  
  # Barajar la pila de descarte
  shuffled_discard_pile <- discard_pile[sample(nrow(discard_pile)), ]
  
  # Actualizar la pila de descarte en el estado del juego
  estado_juego$"Discard Pile" <- shuffled_discard_pile
  
  return(estado_juego)
}

##5##
# Función para que un jugador realice una jugada

make_play <- function(player_hand, discard_pile) {
  # Verificar si hay una carta jugable en la mano del jugador
  playable_card <- NULL
  for (i in 1:nrow(player_hand)) {
    if (player_hand[i, "main_number"] == discard_pile[nrow(discard_pile), "main_number"] |
        player_hand[i, "modifier"] == discard_pile[nrow(discard_pile), "modifier"]) {
      playable_card <- i
      break
    }
  }
  
  # Si hay una carta jugable, moverla a la pila de descarte y actualizar la mano del jugador
  if (!is.null(playable_card)) {
    played_card <- player_hand[playable_card, ]
    discard_pile <- rbind(discard_pile, played_card)
    player_hand <- player_hand[-playable_card, , drop = FALSE]
    return(list(player_hand = player_hand, discard_pile = discard_pile))
  } else {
    # Si ningún jugador tiene una carta jugable, barajar la pila de descarte
    if (nrow(discard_pile) > 1) {
      discard_pile <- discard_pile[sample(nrow(discard_pile)), ]
      message("No hay ninguna carta jugable en la mano de ningún jugador. La pila de descarte se ha barajado.")
    } else {
      message("No hay ninguna carta jugable en la mano de ningún jugador y la pila de descarte tiene solo una carta.")
    }
    return(NULL)
  }
}

##6##
# Función para simular un juego de un solo jugador y rastrear el tiempo transcurrido

simulate_one_player_game <- function(card_data, reparto) {
  # Crear el estado inicial del juego
  game_state <- create_game_state(card_data, reparto)
  
  # Obtener la mano del jugador
  player_hand <- game_state$Players$Player1
  
  # Contador para el tiempo transcurrido
  start_time <- Sys.time()
  
  # Bucle para realizar jugadas hasta que la mano del jugador esté vacía
  while (nrow(player_hand) > 0) {
    # Hacer una jugada
    result <- make_play(player_hand, game_state$Discard Pile)
    
    # Si no hay jugada posible, romper el bucle
    if (is.null(result)) {
      break
    }
    
    # Actualizar la mano del jugador y la pila de descarte
    player_hand <- result$player_hand
    game_state$Discard Pile <- result$discard_pile
  }
  
  # Calcular el tiempo transcurrido
  end_time <- Sys.time()
  elapsed_time <- end_time - start_time
  
  # Devolver el tiempo transcurrido
  return(elapsed_time)
}

# Ejemplo de uso
tiempo_transcurrido <- simulate_one_player_game(card_data, reparto)
tiempo_transcurrido

##7##
# Función para ejecutar la simulación un número especificado de veces

# Instalar y cargar el paquete tictoc si aún no está instalado
if (!requireNamespace("tictoc", quietly = TRUE)) {
  install.packages("tictoc")
}
library(tictoc)

# Función para simular un juego de un solo jugador y rastrear el tiempo transcurrido
simulate_one_player_game <- function(card_data, reparto) {
  # Crear el estado inicial del juego
  game_state <- create_game_state(card_data, reparto)
  
  # Obtener la mano del jugador
  player_hand <- game_state$Players$Player1
  
  # Contador para el tiempo transcurrido
  start_time <- Sys.time()
  
  # Bucle para realizar jugadas hasta que la mano del jugador esté vacía
  while (nrow(player_hand) > 0) {
    # Hacer una jugada
    result <- make_play(player_hand, game_state$Discard Pile)
    
    # Si no hay jugada posible, romper el bucle
    if (is.null(result)) {
      break
    }
    
    # Actualizar la mano del jugador y la pila de descarte
    player_hand <- result$player_hand
    game_state$Discard Pile <- result$discard_pile
  }
  
  # Calcular el tiempo transcurrido
  end_time <- Sys.time()
  elapsed_time <- end_time - start_time
  
  # Devolver el tiempo transcurrido
  return(elapsed_time)
}

# Función para ejecutar la simulación un número especificado de veces
run_simulation <- function(num_simulations) {
  # Vector para almacenar los tiempos de cada simulación
  simulation_times <- numeric(num_simulations)
  
  # Bucle para ejecutar la simulación el número especificado de veces
  for (i in 1:num_simulations) {
    # Iniciar el temporizador
    tic()
    
    # Ejecutar la simulación
    simulation_times[i] <- simulate_one_player_game(data(), crear_reparto_cartas())
    
    # Detener el temporizador y mostrar el tiempo transcurrido
    toc()
  }
  
  # Calcular el tiempo promedio de todas las simulaciones
  avg_time <- mean(simulation_times)
  
  # Mostrar el tiempo promedio
  cat("Average time for", num_simulations, "simulations:", avg_time, "seconds\n")
  
  # Devolver el tiempo promedio
  return(avg_time)
}

# Ejecutar la simulación un millón de veces
average_time <- run_simulation(1000000)