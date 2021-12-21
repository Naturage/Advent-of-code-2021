source(here::here("common functions.R"))

day <- 21

# Data input
input <- get_input(day) %>% 
  str_extract_all("\\d") %>% 
  unlist %>% 
  .[c(2,4)] %>% 
  as.numeric

#Pt 1
scores <- c(0,0)
current_roll <- 0
current_positions <- input 
turn <- 0

while(max(scores) < 1000){
  turn <- turn %% 2 + 1
  tiles_to_move <- (3 * current_roll + 6) %% 10
  current_roll <- current_roll + 3
  current_positions[turn] <- ((current_positions[turn] + tiles_to_move -1) %% 10) +1
  scores[turn] <- scores[turn] + current_positions[turn]
}

(current_roll * min(scores))

# Pt 2

# A matrix of every universe.
# Every turn creates 27 universes: in 1 player moves by 3, 3 by 4, 6 by 5, 7 by 6, 6 by 7, 3 by 8, 1 by 9.
# Need to record: whose turn is it next, current positions, current scores, # of universes.

wins <- c(0,0)

multiverse <- tibble(universes = 1,
                     next_turn = 1, 
                     score_p1 = 0, 
                     score_p2 = 0,
                     position_p1 = input[1],
                     position_p2 = input[2])

take_quantum_turn <- function(multiverse){
  
  wins[1] <<- wins[1] + (multiverse %>% 
    filter(score_p1 >= 21) %>% 
    summarise(universes = sum(universes)) %>% 
    as.numeric())
  
  wins[2] <<- wins[2] + (multiverse %>% 
    filter(score_p2 >= 21) %>% 
    summarise(universes = sum(universes)) %>% 
    as.numeric())
  
  multiverse <- multiverse %>% 
    filter(score_p1 < 21 & score_p2 < 21) %>%
    rowwise() %>%
    mutate(universes= (c(1,3,6,7,6,3,1) * universes) %>% list,
           position_p1 = ifelse(next_turn == 1,
                                ((c(3:9) + position_p1 - 1) %% 10 + 1) %>% list,
                                rep(position_p1,7) %>% list),
           position_p2 = ifelse(next_turn == 2,
                                ((c(3:9) + position_p2 - 1) %% 10 + 1) %>% list,
                                rep(position_p2,7) %>% list)) %>%
    unnest(c(universes, position_p1, position_p2)) %>%
    mutate(score_p1 = ifelse(next_turn == 1, score_p1 + position_p1, score_p1),
           score_p2 = ifelse(next_turn == 2, score_p2 + position_p2, score_p2),
           next_turn = 3 - next_turn) %>%
    group_by(next_turn, score_p1, score_p2, position_p1, position_p2) %>% 
    summarise(universes = sum(universes), .groups = "drop")
  
  return(multiverse)
}

while (nrow(multiverse) > 0){
  multiverse <- take_quantum_turn(multiverse)
}

options(scipen = 999)
(max(wins))