source(here::here("common functions.R"))

day <- 9

# Data input
input <- get_input(day) %>% strsplit(split = "") %>% lapply(as.numeric) 
input <- input %>% unlist() %>% matrix(nrow = length(input), byrow = TRUE)

neighbours <- function(row, col, input = globalenv()$input){
  out <- c()
  if (row > 1){
    out <- c(out, input[row - 1, col])
  }
  if (row < nrow(input)){
    out <- c(out, input[row + 1, col])
  }
  if (col > 1){
    out <- c(out, input[row, col - 1])
  }
  if (col < ncol(input)){
    out <- c(out, input[row, col + 1])
  }
  return(out)
}

# Pt 1
local_minimum <- c()
local_minimum_row <- c()
local_minimum_col <- c()

for (row in 1:nrow(input)){
  for (col in 1:ncol(input)){
    if (input[row, col] < min(neighbours(row, col))){
      local_minimum <- c(local_minimum, input[row, col])
      local_minimum_row <- c(local_minimum_row, row)
      local_minimum_col <- c(local_minimum_col, col)
    }
  }
}

((local_minimum + 1) %>% sum())

# Pt 2
basin <- rep(0, nrow(input) * ncol(input)) %>% matrix(nrow = nrow(input), byrow = TRUE)

for (row in 1:nrow(basin)){
  for (col in 1:ncol(basin)){
    if (input[row, col] >= 9){
      basin[row, col] <- -1
    }
  }
} 

for (low_pt in 1:length(local_minimum)){
  basin[local_minimum_row[low_pt], local_minimum_col[low_pt]] <- low_pt
}

repeat{
  for (row in 1:nrow(basin)){
    for (col in 1:ncol(basin)){
      if (basin[row, col] == 0 & max(neighbours(row, col, input = basin)) > 0){
        basin[row, col] <- max(neighbours(row, col, input = basin))
      }
    }
  }
  if (sum(basin == 0) == 0){
    break
  }
}

(table(basin[basin > 0]) %>% sort() %>% tail(3) %>% prod())