source(here::here("common functions.R"))

day <- 3

# Data input
input <- get_input(day) 

command_length <- nchar(input[[1]])

input <- input %>% 
  as.data.frame() %>% 
  separate(1, into = paste0("bit_",1:command_length), sep = 1:(command_length-1), convert = TRUE)

find_bit <- function(x, common = TRUE, tiebreaker = 1){
  temp <- x %>% mean()
  if (temp == 0.5){
    return(tiebreaker)
  } else if (common == TRUE){
    return(round(temp))
  } else {
    return(1 - round(temp))
  }
}

col_to_deci <- function(df){
  df %>% 
    mutate_all(as.character) %>% 
    unite(ans, 1:command_length, sep = "") %>% 
    as.character() %>% 
    strtoi(base = 2)
}

# Pt 1
popular_bits <- input %>% 
  summarise_all(find_bit)

popular_number <- col_to_deci(popular_bits)

(popular_number * (2^command_length - 1 - popular_number))

# Pt 2
oxygen <- input

for (i in 1:command_length){
  oxygen <- oxygen %>% filter(.[[i]] == find_bit(.[[i]]))
  if (nrow(oxygen) == 1){
    break
  }
}

oxygen_reading <- col_to_deci(oxygen)

co2 <- input

for (i in 1:command_length){
  co2 <- co2 %>% filter(.[[i]] == find_bit(.[[i]], common = FALSE, tiebreaker = 0))
  if (nrow(co2) == 1){
    break
  }
}

co2_reading <- col_to_deci(co2)

(oxygen_reading * co2_reading)