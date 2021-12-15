source(here::here("common functions.R"))

day <- 15

# Data input
input <- get_input(day) %>% strsplit(split = "") %>% lapply(as.numeric)
input <- input %>% unlist() %>% matrix(nrow = length(input), byrow = TRUE)

min_cost <- input
min_cost[TRUE] <- 9999
min_cost[1,1] <- 0

neighbours <- function(row, col, input = globalenv()$input){
  out <- c()
  row <- row %>% as.integer()
  col <- col %>% as.integer()
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

neighbour_indices <- function(row, col, input = globalenv()$input){
  out <- list()
  if (row > 1){
    out <- c(out, c((row - 1) %>% as.integer, col %>% as.integer))
  }
  if (row < nrow(input)){
    out <- c(out, c((row + 1) %>% as.integer, col %>% as.integer))
  }
  if (col > 1){
    out <- c(out, c(row %>% as.integer, (col - 1) %>% as.integer))
  }
  if (col < ncol(input)){
    out <- c(out, c(row %>% as.integer, (col + 1) %>% as.integer))
  }
  return(out)
}

# Pt 1

min_cost <- input
min_cost[TRUE] <- 9999
min_cost[1,1] <- 0

current_cost <- 0

may_need_update_new <- matrix(c(1L,2L,2L,1L), ncol = 2, byrow = TRUE)

while(min_cost[100,100] == 9999){
  current_cost <- current_cost + 1
  if (current_cost %% 10 == 0){
    print(current_cost)
  }
  may_need_update <- may_need_update_new %>% 
    .[!duplicated(.),] %>% 
    .[rowSums(is.na(.)) != ncol(.),]
  may_need_update_new <- may_need_update
  for (x in 1:nrow(may_need_update)){
    if (min(neighbours(may_need_update[x,1], may_need_update[x,2], input = min_cost)) + input[may_need_update[x,1], may_need_update[x,2]] == current_cost){
      min_cost[may_need_update[x,1], may_need_update[x,2]] = current_cost
      may_need_update_new[x,] <- NA
      tmp_indices <- neighbour_indices(may_need_update[x,1], may_need_update[x,2], input = input) %>% 
        unlist() %>% 
        matrix(.,ncol = 2, byrow = TRUE) %>%
        .[mapply(function(x,y){min_cost[x,y]}, x = .[,1], y = .[,2]) == 9999] %>% 
        matrix(.,ncol = 2)
      may_need_update_new <- rbind(may_need_update_new, 
                                   tmp_indices)
    }
  }
}

(min_cost[100,100])

# Pt 2

input_p2 <- cbind(rbind((input - 1) %% 9 + 1, ((input + 0) %% 9) + 1, ((input + 1) %% 9) + 1, ((input + 2) %% 9) + 1, ((input + 3) %% 9) + 1),
                  rbind((input + 0) %% 9 + 1, ((input + 1) %% 9) + 1, ((input + 2) %% 9) + 1, ((input + 3) %% 9) + 1, ((input + 4) %% 9) + 1),
                  rbind((input + 1) %% 9 + 1, ((input + 2) %% 9) + 1, ((input + 3) %% 9) + 1, ((input + 4) %% 9) + 1, ((input + 5) %% 9) + 1),
                  rbind((input + 2) %% 9 + 1, ((input + 3) %% 9) + 1, ((input + 4) %% 9) + 1, ((input + 5) %% 9) + 1, ((input + 6) %% 9) + 1),
                  rbind((input + 3) %% 9 + 1, ((input + 4) %% 9) + 1, ((input + 5) %% 9) + 1, ((input + 6) %% 9) + 1, ((input + 7) %% 9) + 1))

min_cost_p2 <- input_p2
min_cost_p2[TRUE] <- 9999
min_cost_p2[1,1] <- 0

may_need_update_new <- matrix(c(1L,2L,2L,1L), ncol = 2, byrow = TRUE)

current_cost <- 0
while(min_cost_p2[500,500] == 9999){
  current_cost <- current_cost + 1
  if (current_cost %% 10 == 0){
    print(current_cost)
  }
  may_need_update <- may_need_update_new %>% 
    .[!duplicated(.),] %>% 
    .[rowSums(is.na(.)) != ncol(.),]
  may_need_update_new <- may_need_update
  for (x in 1:nrow(may_need_update)){
    if (min(neighbours(may_need_update[x,1], may_need_update[x,2], input = min_cost_p2)) + input_p2[may_need_update[x,1], may_need_update[x,2]] == current_cost){
      min_cost_p2[may_need_update[x,1], may_need_update[x,2]] = current_cost
      may_need_update_new[x,] <- NA
      tmp_indices <- neighbour_indices(may_need_update[x,1], may_need_update[x,2], input = input_p2) %>% 
        unlist() %>% 
        matrix(.,ncol = 2, byrow = TRUE) %>%
        .[mapply(function(x,y){min_cost_p2[x,y]}, x = .[,1], y = .[,2]) == 9999] %>% 
        matrix(.,ncol = 2)
      may_need_update_new <- rbind(may_need_update_new, 
                                   tmp_indices)
    }
  }
}

(min_cost_p2[500,500])