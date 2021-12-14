source(here::here("common functions.R"))

day <- 14

# Data input
input <- get_input(day)

current_poly <- input[[1]] %>% 
  strsplit(split = "") %>%
  unlist()

formulas <- input %>% .[grepl("[A-Z]{2} -> ([A-Z])",.)] %>% 
  as.data.frame() %>%
  rename(full_formula = 1) %>%
  mutate(from_1 = str_match(full_formula,"([A-Z])([A-Z]) -> ([A-Z])")[,2],
         from_2 = str_match(full_formula,"([A-Z])([A-Z]) -> ([A-Z])")[,3],
         to     = str_match(full_formula,"([A-Z])([A-Z]) -> ([A-Z])")[,4]) %>%
  mutate(from = paste0(from_1,from_2),
         to_1 = paste0(from_1,to),
         to_2 = paste0(to,from_2))
  
current_poly_counts <- sapply(c(1:(length(current_poly)-1)), 
                                function(x){paste0(current_poly[x],current_poly[x+1])}) %>%
  as.data.frame() %>%
  rename(from = 1) %>%
  count(from) %>%
  rename(count = n)

formulas <- formulas %>%
  left_join(current_poly_counts, by = "from") %>%
  mutate(count = ifelse(is.na(count),0,count))

advance_a_day <- function(polymer_in){
  polymer_out <- polymer_in %>% 
    select(-count) %>%
    left_join(rbind(polymer_in %>% select(out = to_1, count),
                    polymer_in %>% select(out = to_2, count)),
              by = c("from" = "out")) %>%
    mutate(count = ifelse(is.na(count),0,count)) %>%
    group_by(full_formula, from_1, from_2, to, from, to_1, to_2) %>%
    summarise(count = sum(count), .groups = "drop")
  return(polymer_out)
}

# Pt 1

polymers <- formulas

for (i in 1:10){
  polymers <- advance_a_day(polymers)
}

(rbind(polymers %>% select(poly = from_1, count),
      polymers %>% select(poly = from_2, count)) %>%
  group_by(poly) %>%
  summarise(count = sum(count)) %>%
  # Everything is doublecounted, except for the two polys at the very ends. The two are different, so we can solve it all in one step.
  mutate(count = ceiling(count/2)) %>%
  summarise(diff = max(count) - min(count)) %>%
  select(diff) %>%
  as.numeric())

# Pt 2

options(scipen = 999)

polymers <- formulas

for (i in 1:40){
  polymers <- advance_a_day(polymers)
}

(rbind(polymers %>% select(poly = from_1, count),
       polymers %>% select(poly = from_2, count)) %>%
    group_by(poly) %>%
    summarise(count = sum(count)) %>%
    # Everything is doublecounted, except for the two polys at the very ends. The two are different, so we can solve it all in one step.
    mutate(count = ceiling(count/2)) %>%
    summarise(diff = max(count) - min(count)) %>%
    select(diff) %>%
    as.numeric())