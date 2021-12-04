source(here::here("common functions.R"))

day <- 1

# Data input
input <- get_input(day) %>% as.numeric()

# Pt 1
(diff(input) %>% sapply(function(x){ifelse(x > 0, 1, 0)}) %>% sum())

# Pt 2
trios <- 1:(length(input)-2) %>% sapply(function(i,x){x[i] + x[i+1] + x[i+2]}, x = input)

(diff(trios) %>% sapply(function(x){ifelse(x > 0, 1, 0)}) %>% sum())

