source(here::here("common functions.R"))

day <- 11

# Data input
input <- get_input(day) %>% strsplit(split = "") %>% lapply(as.numeric)
input <- input %>% unlist() %>% matrix(nrow = length(input), byrow = TRUE)

input_backup <- input

flare_up_neighbours <- function(row, col, input = globalenv()$input){
  for (i in max(1,row-1):min(nrow(input),row+1)){
    for(j in max(1,col-1):min(ncol(input),col+1)){
      input[i, j] <<- input[i, j] + 1
    }
  }
  
  input[row, col] <<- -9
}

reset_flarings <- function(){
  count_flares <- length(input[input < 0])
  input[input < 0] <<- 0
  return(count_flares)
}

# Pt 1
input <- input_backup

flarings_total <- 0

for (n_day in 1:100){
  input <- input + 1
  while (length(input[input > 9] > 0)){
    cells <- which(input > 9)
    cols <- ceiling(cells/10)
    rows <- cells - 10 * (cols - 1)
    mapply(flare_up_neighbours, rows, cols)
  }
  flarings_total <- flarings_total + reset_flarings()
}

(flarings_total)

# Pt 2
input <- input_backup

n_day <- 0

while (min(input) < max(input)){
  n_day <- n_day + 1
  input <- input + 1
  while (length(input[input > 9] > 0)){
    cells <- which(input > 9)
    cols <- ceiling(cells/10)
    rows <- cells - 10 * (cols - 1)
    mapply(flare_up_neighbours, rows, cols)
  }
  flarings_total <- flarings_total + reset_flarings()
}

(n_day)