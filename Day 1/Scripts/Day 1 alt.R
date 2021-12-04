source(here::here("common functions.R"))

day <- 1

# Data input
input <- get_input(day) %>% as.numeric()

diff_jump_n <- function(i, jump, data = input){
  ifelse(data[i+jump] - data[i] > 0, 1, 0)
}

# Pt 1
(1:(length(input)-1) %>% sapply(diff_jump_n, 1) %>% sum())

# Pt 2
(1:(length(input)-3) %>% sapply(diff_jump_n, 3) %>% sum())