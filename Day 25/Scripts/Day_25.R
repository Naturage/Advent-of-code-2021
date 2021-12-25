source(here::here("common functions.R"))

day <- 25

# Data input

input <- get_input(day) %>% strsplit(split = "")
input <- input %>% unlist() %>% matrix(nrow = length(input), byrow = TRUE)

turn <- 0

repeat{
  turn <- turn + 1
  
  moving_right <- which(input == ">", arr.ind = TRUE) %>% 
    as_tibble() %>% 
    mutate(col_to_check = col %% ncol(input) + 1) %>%
    rowwise() %>%
    mutate(target_cell = input[row,col_to_check]) %>%
    ungroup() %>%
    filter(target_cell == ".") %>%
    mutate(un_array_index_from = (col         -1) * nrow(input) + row,
           un_array_index_to   = (col_to_check-1) * nrow(input) + row)
  
  input[moving_right$un_array_index_from] <- "."
  input[moving_right$un_array_index_to  ] <- ">"
  
  moving_down <- which(input == "v", arr.ind = TRUE) %>% 
    as_tibble() %>% 
    mutate(row_to_check = row %% nrow(input) + 1) %>%
    rowwise() %>%
    mutate(target_cell = input[row_to_check,col]) %>%
    ungroup() %>%
    filter(target_cell == ".") %>%
    mutate(un_array_index_from = (col-1) * nrow(input) + row         ,
           un_array_index_to   = (col-1) * nrow(input) + row_to_check)
  
  input[moving_down$un_array_index_from] <- "."
  input[moving_down$un_array_index_to  ] <- "v"
}

(turn)
