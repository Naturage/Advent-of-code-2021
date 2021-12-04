source(here::here("common functions.R"))

day <- 4

# Data input
input <- get_input(day) 

numbers_called <- input[[1]] %>% strsplit(split = ",") %>% unlist() %>% as.numeric()

bingo_cards <- input[-1] %>% strsplit(split = " +")

num_bingo_cards <- length(bingo_cards)/6

reset_cards <- function(){
  bingo_card  <<- list()
  matched     <<- list()
  matched_row <<- list()
  matched_col <<- list()
  winner      <<- list()
  
  for (i in 1:num_bingo_cards){
    bingo_card[[i]]  <<- bingo_cards[(6*i-4):(6*i)] %>% unlist() %>% as.numeric() %>% na.omit()
    matched[[i]]     <<- rep(FALSE, 25)
    matched_row[[i]] <<- rep(0, 5)
    matched_col[[i]] <<- rep(0, 5)
    winner[[i]]      <<- FALSE
  }
}

number_draw <- function(card_id, number){
  spot_on_card <- match(number, bingo_card[[card_id]])
  if(!is.na(spot_on_card)){
    matched[[card_id]][[spot_on_card]] <<- TRUE
    row <- ceiling(spot_on_card/5)
    col <- (spot_on_card - 1) %% 5 + 1
    matched_row[[card_id]][[row]] <<- matched_row[[card_id]][[row]] + 1
    matched_col[[card_id]][[col]] <<- matched_col[[card_id]][[col]] + 1
    if (matched_row[[card_id]][[row]] == 5 | matched_col[[card_id]][[col]] == 5){
      winner[[card_id]] <<- TRUE
    }
  }
}

# Part 1

reset_cards()

for (i in numbers_called){
  sapply(1:num_bingo_cards, number_draw, number = i)
  if (sum(winner %>% unlist()) == 1){
    winning_number <- i
    winning_card_id <- which(winner == TRUE)
    break
  }
}

(bingo_card[[winning_card_id]][which(matched[[winning_card_id]] == FALSE)] %>% sum() * winning_number)

# Part 2

reset_cards()

for (i in numbers_called){
  sapply(1:num_bingo_cards, number_draw, number = i)
  if (sum(winner %>% unlist()) == num_bingo_cards - 1){
    winning_card_id <- which(winner == FALSE)
  } else if (sum(winner %>% unlist()) == num_bingo_cards){
    winning_number <- i
    break
  }
}

(bingo_card[[winning_card_id]][which(matched[[winning_card_id]] == FALSE)] %>% sum() * winning_number)