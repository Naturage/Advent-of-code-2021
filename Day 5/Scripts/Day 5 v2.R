source(here::here("common functions.R"))

day <- 5

# Data input
input <- get_input(day) %>% as.data.frame() %>% rename(raw_input = 1)

vent_regex <- "((\\d+),(\\d+)) -> ((\\d+),(\\d+))"

input <- input %>% mutate(vent_start_x = str_match(raw_input, vent_regex)[,3] %>% as.numeric(),
                          vent_start_y = str_match(raw_input, vent_regex)[,4] %>% as.numeric(),
                          vent_end_x   = str_match(raw_input, vent_regex)[,6] %>% as.numeric(),
                          vent_end_y   = str_match(raw_input, vent_regex)[,7] %>% as.numeric())

write_out_vent_locs <- function(input){
  input %>% 
    rowwise() %>%
    mutate(x = ifelse(vent_start_x == vent_end_x, 
                      rep(vent_start_x, abs(vent_start_y - vent_end_y) + 1) %>% as.list(),
                      list(vent_start_x:vent_end_x)),
           y = ifelse(vent_start_y == vent_end_y, 
                      list(rep(vent_end_y  , abs(vent_start_x - vent_end_x) + 1)),
                      list(vent_start_y:vent_end_y))
           ) %>%
    unnest(c(x, y)) %>%
    select(x, y)
}

# Part 1
 
# Delete any nondiag
vents_p1 <- input %>% 
  filter(vent_start_x == vent_end_x | vent_start_y == vent_end_y) %>% 
  write_out_vent_locs()

(vents_p1 %>% filter(duplicated(.)) %>% unique() %>% nrow())

# Part 2

vents_p2 <- input %>%
  write_out_vent_locs()

(vents_p2 %>% filter(duplicated(.)) %>% unique() %>% nrow())