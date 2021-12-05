ptm <- proc.time()

source(here::here("common functions.R"))

day <- 5

# Data input
input <- get_input(day) %>% as.data.frame() %>% rename(raw_input = 1)

vent_regex <- "((\\d+),(\\d+)) -> ((\\d+),(\\d+))"

input <- input %>% mutate(vent_start_x = str_match(raw_input, vent_regex)[,3] %>% as.numeric(),
                          vent_start_y = str_match(raw_input, vent_regex)[,4] %>% as.numeric(),
                          vent_end_x   = str_match(raw_input, vent_regex)[,6] %>% as.numeric(),
                          vent_end_y   = str_match(raw_input, vent_regex)[,7] %>% as.numeric())

write_out_vent_locs <- function(row, input){
  temp <- input %>% slice(row) %>% select(vent_start_x, vent_start_y, vent_end_x, vent_end_y)
  if(temp$vent_start_x == temp$vent_end_x){
    vents <- as.data.frame(c(temp$vent_start_y:temp$vent_end_y)) %>% 
      rename(y = 1) %>% 
      mutate(x = temp$vent_start_x)
  } else if (temp$vent_start_y == temp$vent_end_y){
    vents <- as.data.frame(c(temp$vent_start_x:temp$vent_end_x)) %>% 
      rename(x = 1) %>% 
      mutate(y = temp$vent_start_y)
  } else {
    vents <- list(c(temp$vent_start_x:temp$vent_end_x), c(temp$vent_start_y:temp$vent_end_y)) %>%
      as.data.frame() %>% 
      rename(x = 1, y = 2)
  }
  return(vents)
}

# Part 1

# Delete any nondiag
input_p1 <- input %>% filter(vent_start_x == vent_end_x | vent_start_y == vent_end_y)

vents_p1 <- lapply(1:nrow(input_p1), write_out_vent_locs, input = input_p1) %>% do.call("rbind",.)

(vents_p1 %>% count(x,y) %>% filter(n > 1) %>% count())

# Part 2

vents_p2 <- lapply(1:nrow(input), write_out_vent_locs, input = input) %>% do.call("rbind",.)

(vents_p2 %>% count(x,y) %>% filter(n > 1) %>% count())