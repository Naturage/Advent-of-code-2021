source(here::here("common functions.R"))

day <- 23

# Data input - my input for p2 is cursed and want to make that epxlicit.
input <- tibble(room_a = "BDDC"    ,
                room_b = "CCBD"    ,
                room_c = "ABAD"    ,
                room_d = "BACA"    ,
                hallway = "#######",
                path = "",
                price = 0)

# Map as I denote it:

#
# 12.3.4.5.67
#   A B C D
#
# So a turn A6 means top entry in room A is moving to hallway spot 6. Obviously, 6A means the opposite.

all_possible_turns <-
c(paste0("A",c(1:7)),
  paste0("B",c(1:7)),
  paste0("C",c(1:7)),
  paste0("D",c(1:7)),
  paste0(c(1:7),"A"),
  paste0(c(1:7),"B"),
  paste0(c(1:7),"C"),
  paste0(c(1:7),"D")) %>% tibble(next_turn = .)

in_the_way <- tibble(hallspot = rep(1:7,4),
                     room = rep(c("A","B","C","D"),each = 7),
                     check_2  = c(T,F,F,F,F,F,F, T,F,F,F,F,F,F, T,F,F,F,F,F,F, T,F,F,F,F,F,F),
                     check_3  = c(F,F,F,T,T,T,T, T,T,F,F,F,F,F, T,T,F,F,F,F,F, T,T,F,F,F,F,F),
                     check_4  = c(F,F,F,F,T,T,T, F,F,F,F,T,T,T, T,T,T,F,F,F,F, T,T,T,F,F,F,F),
                     check_5  = c(F,F,F,F,F,T,T, F,F,F,F,F,T,T, F,F,F,F,F,T,T, T,T,T,T,F,F,F),
                     check_6  = c(F,F,F,F,F,F,T, F,F,F,F,F,F,T, F,F,F,F,F,F,T, F,F,F,F,F,F,T),
                     distance = c(2,1,1,3,5,7,8, 4,3,1,1,3,5,6, 6,5,3,1,1,3,4, 8,7,5,3,1,1,2))

confirm_clear_path <- function(df){
  df %>% 
    mutate(hallspot = str_extract(next_turn,"\\d") %>% as.integer(),
           room     = str_extract(next_turn,"[:alpha:]")) %>%
    left_join(in_the_way, by = c("room","hallspot")) %>%
    mutate(hallway_to_check = paste0(ifelse(check_2, substr(hallway,2,2), ""),
                                     ifelse(check_3, substr(hallway,3,3), ""),
                                     ifelse(check_4, substr(hallway,4,4), ""),
                                     ifelse(check_5, substr(hallway,5,5), ""),
                                     ifelse(check_6, substr(hallway,6,6), ""))) %>%
    mutate(path_clear = !str_detect(hallway_to_check,"[^#]")) %>%
    select(-starts_with("check"), -room, -hallway_to_check)
}

find_valid_next_turn <- function(df){
  df %>%
    crossing(all_possible_turns) %>%
    confirm_clear_path() %>%
    filter(path_clear == TRUE) %>%
    mutate(from = substr(next_turn, 1, 1),
           to = substr(next_turn, 2, 2),
           rooms_open = paste0(ifelse(!str_detect(room_a, "B|C|D"), "A", ""),
                               ifelse(!str_detect(room_b, "A|C|D"), "B", ""),
                               ifelse(!str_detect(room_c, "A|B|D"), "C", ""),
                               ifelse(!str_detect(room_d, "A|B|C"), "D", "")),
      is_valid = case_when(
      from %in% c("1","2","3","4","5","6","7") & 
        substr(hallway,hallspot,hallspot) == to & 
        str_detect(rooms_open, to) ~ "1",
      from %in% c("A", "B", "C", "D") &
        substr(hallway,hallspot,hallspot) == "#" &
        !str_detect(rooms_open, from) ~ "1",
      TRUE ~ "0"
    )) %>%
    filter(is_valid == 1) %>%
    select(-is_valid, -rooms_open, -path_clear)
}

do_next_turn <- function(df){
  df %>%
    mutate(room_top_a = str_locate(room_a, "[^#]")[,"start"] %>% ifelse(is.na(.),5,.),
           room_top_b = str_locate(room_b, "[^#]")[,"start"] %>% ifelse(is.na(.),5,.),
           room_top_c = str_locate(room_c, "[^#]")[,"start"] %>% ifelse(is.na(.),5,.),
           room_top_d = str_locate(room_d, "[^#]")[,"start"] %>% ifelse(is.na(.),5,.)) %>%
    mutate(moving_fellow = case_when(
      from == hallspot ~ substr(hallway,hallspot,hallspot),
      from == "A" ~ substr(room_a,room_top_a,room_top_a),
      from == "B" ~ substr(room_b,room_top_b,room_top_b),
      from == "C" ~ substr(room_c,room_top_c,room_top_c),
      from == "D" ~ substr(room_d,room_top_d,room_top_d))) %>% 
    rowwise() %>%
    mutate(
      room_a = ifelse(
          to   == "A", str_replace(room_a,paste0("(.{",room_top_a-2,"})."), paste0("\\1",moving_fellow)),
        ifelse(
          from == "A", str_replace(room_a,paste0("(.{",room_top_a-1,"})."), "\\1#"),
                       room_a)),
      room_b = ifelse(
        to     == "B", str_replace(room_b,paste0("(.{",room_top_b-2,"})."), paste0("\\1",moving_fellow)),
        ifelse(
          from == "B", str_replace(room_b,paste0("(.{",room_top_b-1,"})."), "\\1#"),
                       room_b)),
      room_c = ifelse(
        to     == "C", str_replace(room_c,paste0("(.{",room_top_c-2,"})."), paste0("\\1",moving_fellow)),
        ifelse(
          from == "C", str_replace(room_c,paste0("(.{",room_top_c-1,"})."), "\\1#"),
                       room_c)),
      room_d = ifelse(
        to     == "D", str_replace(room_d,paste0("(.{",room_top_d-2,"})."), paste0("\\1",moving_fellow)),
        ifelse(
          from == "D", str_replace(room_d,paste0("(.{",room_top_d-1,"})."), "\\1#"),
                       room_d)),
      hallway = ifelse(hallspot == from, 
                       str_replace(hallway,paste0("(.{",hallspot-1,"})."), "\\1#"),
                       str_replace(hallway,paste0("(.{",hallspot-1,"})."), paste0("\\1",moving_fellow)))
      ) %>%
    ungroup() %>%
    mutate(path = paste0(path,next_turn,","),
           tot_distance = case_when(
             from == "A" ~ distance + room_top_a,
             from == "B" ~ distance + room_top_b,
             from == "C" ~ distance + room_top_c,
             from == "D" ~ distance + room_top_d,
             to == "A" ~ distance + room_top_a - 1,
             to == "B" ~ distance + room_top_b - 1,
             to == "C" ~ distance + room_top_c - 1,
             to == "D" ~ distance + room_top_d - 1,
             TRUE ~ 0),
           movers_pricing = case_when(
             moving_fellow == "A" ~ 1,
             moving_fellow == "B" ~ 10,
             moving_fellow == "C" ~ 100,
             moving_fellow == "D" ~ 1000,
           ),
           price = price + movers_pricing * tot_distance) %>%
    select(-from, -to, -hallspot, -starts_with("room_top"), -next_turn, -distance, -moving_fellow, -tot_distance, -movers_pricing) %>%
    group_by(room_a, room_b, room_c, room_d, hallway) %>% 
    arrange(price) %>% 
    slice(1) %>% 
    ungroup()
}

in_p2 <- input

for (i in 1:32){
  in_p2 <- in_p2 %>% find_valid_next_turn()
  in_p2 <- in_p2 %>% do_next_turn()
  print(paste0("turn ",i,", different states: ", nrow(in_p2)))
}

(in_p2$price)