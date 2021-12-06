source(here::here("common functions.R"))

day <- 6

# Data input
input <- get_input(day) %>% strsplit(split = ",") %>% unlist() %>% as.numeric()

input_agg <- table(input) %>% as.vector() %>% unname()
# Note all inputs are in 1-5, so need to adjust a bit.
input_agg <- c(0, input_agg, 0, 0, 0)

advance_day <- function(input){
  out <- c(input[-1], input[1])
  out[7] <- out[7] + input[1]
  return(out)
} 

# Pt 1

in_p1 <- input_agg

for (i in 1:80){
  in_p1 <- advance_day(in_p1)
}

(sum(in_p1))

# Pt 2

in_p2 <- input_agg

for (i in 1:256){
  in_p2 <- advance_day(in_p2)
}

options(scipen=999)
(sum(in_p2))