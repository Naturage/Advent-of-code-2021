source(here::here("common functions.R"))

day <- 24

options(scipen = 999)

# Data input

# We could do this appropriately. Or we could do this smartly.
# I take it "You'll need to figure out what MONAD does some other way" is an invitation to look at the input and fix it myself.
# As it happens it is a set of 18 operations, repeated 14 times, with 3 integer inputs (one of which being whatever we send in)
# and a boolean flag. Everything else there is a fluff.

lines_of_interest <- rep(0:13, each = 3) * 18 + c(5,6,16)

input <- get_input(day) %>% 
  .[lines_of_interest] %>% 
  str_match("div z (1|26)|add x (-?\\d+)|add y (-?\\d+)") %>% 
  .[,2:4] %>% 
  .[!is.na(.)] %>%
  as.integer() %>%
  matrix(ncol = 3)

round_to_zero <- function(x){
  ifelse(x > 0,floor(x),ceiling(x))
}

# Do it digit by digit and save each possible z value with biggest/smallest number that produced it
monad_options <- function(possibilities, digit){
  possibilities %>% 
    crossing(tibble(number = c(1:9))) %>%
    mutate(x = z %% 26 + input[digit,2]) %>%
    mutate(z = round_to_zero(z/input[digit,1])) %>%
    mutate(z = ifelse(x == number,
                      z,
                      26*z + number + input[digit,3])) %>%
    mutate(largest_number  = 10 * largest_number  + number,
           smallest_number = 10 * smallest_number + number) %>%
    group_by(z) %>%
    summarise(largest_number = max(largest_number), smallest_number = min(smallest_number), n = sum(n))
}

possibilities <- tibble(z = 0, largest_number = 0, smallest_number = 0, n = 1)

for (digit in 1:14){
  possibilities <- monad_options(possibilities, digit)
}

(possibilities %>% filter(z == 0) %>% pull(largest_number))
(possibilities %>% filter(z == 0) %>% pull(smallest_number))