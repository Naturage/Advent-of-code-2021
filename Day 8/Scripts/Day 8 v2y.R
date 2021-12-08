source(here::here("common functions.R"))

day <- 8

# Data input
input <- get_input(day) %>% strsplit(split = " ")

possibilities <- input %>% lapply(function(x){x[1:10]})
output <- input %>% lapply(function(x){x[12:15]})

turn_to_vec <- function(input){
  return(input %>% strsplit(split = "") %>% unlist())
}

resort_vec <- function(input){
  return(input %>% strsplit(split = "") %>% sapply(sort) %>% sapply(paste, collapse = ""))
}

# Pt 1
(output %>% unlist() %>% .[nchar(.) %in% c(2,3,4,7)] %>% length())

# Pt 2: this is where the fun begins.

digit_solver <- function(digit_options){
  digit_options <- digit_options %>% unlist()
  
  one            <- digit_options %>% .[nchar(.) == 2]
  seven          <- digit_options %>% .[nchar(.) == 3]
  four           <- digit_options %>% .[nchar(.) == 4]
  two_three_five <- digit_options %>% .[nchar(.) == 5]
  six_nine_zero  <- digit_options %>% .[nchar(.) == 6]
  eight          <- digit_options %>% .[nchar(.) == 7]
  
  # 2 has 2 segments not found in 4; others only 1.
  two   <- two_three_five[sapply(two_three_five, function(x){setdiff(turn_to_vec(four), turn_to_vec(x)) %>% length()}) == 2] 
  # 3 has both of 1's segments; others only 1.
  three <- two_three_five[sapply(two_three_five, function(x){setdiff(turn_to_vec(one ), turn_to_vec(x)) %>% length()}) == 0]
  # 5's the last one.
  five  <- two_three_five[!two_three_five %in% c(three,two)]
  
  # 6 has only one of 1's segments, others have both.
  six   <- six_nine_zero[sapply(six_nine_zero, function(x){setdiff(turn_to_vec(one ), turn_to_vec(x)) %>% length()}) == 1]
  # 9 has all of 4 in it.
  nine  <- six_nine_zero[sapply(six_nine_zero, function(x){setdiff(turn_to_vec(four), turn_to_vec(x)) %>% length()}) == 0]
  # 0's the last one.
  zero  <- six_nine_zero[!six_nine_zero %in% c(six,nine)]
  
  output <- c(zero, one, two, three, four, five, six, seven, eight, nine)
  
  return(output)
}

return_decode <- function(solved_digits, four_digit_code){
  solved_digits <- solved_digits %>% unlist() %>% resort_vec()
  four_digit_code <- four_digit_code %>% unlist() %>% resort_vec()
  
  out <- lapply(four_digit_code,function(x){which(solved_digits == x)} - 1) %>% str_flatten() %>% as.numeric()
  
  return(out)
}

found_digits <- lapply(possibilities, digit_solver)
out <- mapply(return_decode, found_digits, output)

(sum(out))