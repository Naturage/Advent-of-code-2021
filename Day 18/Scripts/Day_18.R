ptm <- proc.time()

source(here::here("common functions.R"))

day <- 18

input <- get_input(day) 

p_wide <- function(df){
  pivot_wider(df,
              id_cols = c(num_id, number, max_level),
              names_from = "level",
              names_prefix = "level_",
              values_from = "level_no")
}

p_long <- function(df){
  pivot_longer(df,
               cols = starts_with("level_"),
               names_to = "level",
               names_prefix = "level_",
               names_transform = list(level = as.numeric),
               values_to = "level_no",
               values_drop_na = TRUE) %>%
    arrange(num_id, level)
}

parse_squid_number <- function(string){
  number_with_brackets <- string %>%
    str_extract_all("\\]*(^|\\,)\\[*\\d") %>% 
    unlist() %>%
    sapply(str_replace, pattern = "\\,", replacement = "")
    
  
  level <- number_with_brackets %>% 
    sapply(function(x){str_count(x,"\\[") - str_count(x,"\\]")}) %>%
    cumsum()
  
  number <- number_with_brackets %>%
    sapply(str_extract, pattern = "\\d")
  
  parsed_number <- data.frame(num_id = 1:length(number_with_brackets), 
                              number = as.numeric(number), 
                              max_level = level, 
                              row.names = NULL) %>%
    mutate(cumulative_count = cumsum(1/2**max_level)) %>%
    mutate(level_1 = ifelse(cumulative_count <= 0.5, 1, 2),
           level_2 = ifelse(cumulative_count * 2  - floor((cumulative_count - 10**-10) * 2 ) <= 0.5, 1, 2),
           level_3 = ifelse(cumulative_count * 4  - floor((cumulative_count - 10**-10) * 4 ) <= 0.5, 1, 2),
           level_4 = ifelse(cumulative_count * 8  - floor((cumulative_count - 10**-10) * 8 ) <= 0.5, 1, 2),
           level_5 = ifelse(cumulative_count * 16 - floor((cumulative_count - 10**-10) * 16) <= 0.5, 1, 2),
           ) %>%
    select(-cumulative_count) %>%
    p_long() %>%
    filter(level <= max_level) %>% 
    p_wide()
  
  return(parsed_number)
}

add_snail_numbers <- function(first, second){
  first <- first %>% 
    p_long() %>%
    mutate(level = level + 1,
           max_level = max_level + 1) %>%
    p_wide() %>%
    mutate(level_1 = 1)
  
  second <- second %>% 
    p_long() %>%
    mutate(level = level + 1,
           max_level = max_level + 1) %>%
    p_wide() %>%
    mutate(level_1 = 2)
  
  out <- bind_rows(first, second) %>% 
    mutate(num_id = 1:nrow(.))
  
  return(out)
}

explode <- function(parsed_number){
  exploding_pair <- parsed_number %>%
    filter(max_level == 5) %>%
    slice(1:2)
  
  last_id_before <- min(exploding_pair$num_id) - 1
  
  if (last_id_before > 0){
    first_half <- parsed_number[1:last_id_before,]
    first_half$number[last_id_before] <- first_half$number[last_id_before] + exploding_pair$number[1]
  } else {
    first_half <- tibble()
  }
  
  first_id_after <- last_id_before + 3
  
  if (first_id_after <= nrow(parsed_number)){
    second_half <- parsed_number[first_id_after:nrow(parsed_number),] %>%
      mutate(num_id = num_id - 1)
    second_half$number[1] <- second_half$number[1] + exploding_pair$number[2]
  } else {
    second_half <- tibble()
  }
  
  new_zero <- exploding_pair %>% distinct(level_1, level_2, level_3, level_4) %>%
    mutate(num_id = last_id_before + 1,
           max_level = 4,
           number = 0)
  
  out <- bind_rows(first_half, new_zero, second_half)
  return(out)
}

split_snail <- function(parsed_number){
  splitting_no <- parsed_number %>%
    filter(number >= 10) %>%
    slice(1)
  
  last_id_before <- splitting_no$num_id - 1
  
  if (last_id_before > 0){
    first_half <- parsed_number[1:last_id_before,]
  } else {
    first_half <- tibble()
  }
  
  first_id_after <- last_id_before + 2
  
  if (first_id_after <= nrow(parsed_number)){
    second_half <- parsed_number[first_id_after:nrow(parsed_number),] %>%
      mutate(num_id = num_id + 1)
  } else {
    second_half <- tibble()
  }
  
  split <- splitting_no %>% 
    mutate(max_level = max_level + 1,
           number = floor(number/2)) %>%
    mutate(across(ends_with(as.character(.$max_level)) , ~ 1)) %>%
    bind_rows(splitting_no %>% 
                 mutate(max_level = max_level + 1,
                        number = ceiling(number/2),
                        num_id = num_id + 1) %>%
                 mutate(across(ends_with(as.character(.$max_level)) , ~ 2)))
  
  out <- bind_rows(first_half, split, second_half)
  
  return(out)
}

add_and_reduce <- function(first, second){
  summed_no <- add_snail_numbers(first, second)
  
  while (max(summed_no$max_level) == 5 | max(summed_no$number) >= 10){
    while (max(summed_no$max_level) == 5){
      summed_no <- explode(summed_no)
    }
    if (max(summed_no$number) >= 10){
      summed_no <- split_snail(summed_no)
    } 
  }
    
  return(summed_no)
}

count_magnitude <- function(parsed_no){
  
  should_exist <- tibble(level_3 = numeric(), level_4 = numeric())
  
  parsed_no %>% 
    mutate(magnitude = number) %>%
    bind_rows(should_exist) %>%
    filter(!is.na(num_id)) %>%
    group_by(level_1, level_2, level_3) %>%
    summarise(magnitude = ifelse(is.na(level_4), magnitude, sum(magnitude * (4 - level_4))), .groups = "drop_last") %>%
    distinct(level_1, level_2, level_3, magnitude) %>%
    summarise(magnitude = ifelse(is.na(level_3), magnitude, sum(magnitude * (4 - level_3))), .groups = "drop_last") %>%
    distinct(level_1, level_2, magnitude) %>%
    summarise(magnitude = ifelse(is.na(level_2), magnitude, sum(magnitude * (4 - level_2))), .groups = "drop_last") %>%
    distinct(level_1, magnitude) %>%
    summarise(magnitude = ifelse(is.na(level_1), magnitude, sum(magnitude * (4 - level_1)))) %>%
    distinct(magnitude) %>%
    as.numeric()
}

input_parsed <- lapply(input, parse_squid_number)

# Pt 1

sum <- input_parsed[[1]]

for (i in 2:length(input_parsed)){
  sum <- add_and_reduce(sum, input_parsed[[i]])
  print(i)
}

(count_magnitude(sum))

# Pt 2
max_magni <- 0

for (i in 1:length(input_parsed)){
  for (j in (i+1):length(input_parsed)){
    sum <- add_and_reduce(input_parsed[[i]], input_parsed[[j]])
    if (count_magnitude(sum) > max_magni){
      max_magni <- count_magnitude(sum)
    }
  }
}

(max_magni)