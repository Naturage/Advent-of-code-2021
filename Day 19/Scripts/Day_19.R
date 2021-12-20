ptm <- proc.time()

source(here::here("common functions.R"))

day <- 19

input <- get_input(day) %>%
  tibble(input = .) %>% 
  mutate(scanner_id = cumsum(grepl("---", input))) %>% 
  group_split(scanner_id) %>%
  lapply(function(df){df %>% 
      filter(grepl("(-?\\d+),(-?\\d+),(-?\\d+)", input)) %>%
      separate(input, c("x","y","z"), ",") %>%
      mutate(across(c("x","y","z"), as.numeric)) %>%
      select(x,y,z) %>% 
      mutate(sensor_flag = 0) %>%
      bind_rows(tibble(x = 0, y = 0, z = 0, sensor_flag = 1))})

rebase <- function(df, baseline){
  df %>% 
    mutate(x = x - (baseline[1] %>% as.numeric()),
           y = y - (baseline[2] %>% as.numeric()),
           z = z - (baseline[3] %>% as.numeric())) %>%
    rowwise() %>%
    mutate(one   = sort(c(abs(x),abs(y),abs(z)))[1],
           two   = sort(c(abs(x),abs(y),abs(z)))[2],
           three = sort(c(abs(x),abs(y),abs(z)))[3])
}

set_reference_unaligned <- function(scanner_id, line, list = input){
  baseline <- list[[scanner_id]] %>% slice(line) %>% as.vector()
  
  aligned_scanner <- list[[scanner_id]] %>% 
    filter(sensor_flag == 0) %>% 
    select(-sensor_flag) %>% 
    rebase(baseline) %>%
    select(one, two, three)
  
  return(aligned_scanner)
}

check_match <- function(aligned_scanner_1, aligned_scanner_2){
  nrow(inner_join(aligned_scanner_1, aligned_scanner_2, by = c("one", "two", "three"))) >= 12
}

set_reference_aligned <- function(scanner_id_base, line_base, scanner_id_attach, line_attach, list = input){
  baseline_base <- list[[scanner_id_base]] %>% slice(line_base) %>% as.vector()
  
  base <- list[[scanner_id_base]] %>% 
    rebase(baseline_base)
  
  baseline_attach <- list[[scanner_id_attach]] %>% slice(line_attach) %>% as.vector()
  
  attach <- list[[scanner_id_attach]] %>% 
    rebase(baseline_attach) %>% 
    rename(x_attach = x,
           y_attach = y,
           z_attach = z,
           sensor_flag_attach = sensor_flag)
  
  joined <- full_join(base, attach, by = c("one","two","three")) %>% 
    mutate(minus_x = -x,
           minus_y = -y,
           minus_z = -z) %>%
    #ensuring we don't get several rows of same
    mutate(diff_three = three - two,
           diff_two   = two - one) %>%
    arrange(is.na(x), is.na(x_attach), desc(diff_three), desc(diff_two)) 
  
  direction_matching <- joined %>%
    ungroup() %>%
    slice(1) %>%
    select(x, y, z, minus_x, minus_y, minus_z, x_attach, y_attach, z_attach) %>%
    as.vector()
  
  x_attach_match <- match(direction_matching[7], direction_matching[1:6]) %>% names(direction_matching)[.]
  y_attach_match <- match(direction_matching[8], direction_matching[1:6]) %>% names(direction_matching)[.]
  z_attach_match <- match(direction_matching[9], direction_matching[1:6]) %>% names(direction_matching)[.]
  
  joined_full <- joined %>%
    mutate("{x_attach_match}" := x_attach,
           "{y_attach_match}" := y_attach,
           "{z_attach_match}" := z_attach) %>%
    mutate(x = ifelse(is.na(x), -minus_x, x),
           y = ifelse(is.na(y), -minus_y, y),
           z = ifelse(is.na(z), -minus_z, z)) %>%
    mutate(sensor_flag = ifelse(is.na(sensor_flag), sensor_flag_attach, sensor_flag)) %>%
    select(x,y,z, sensor_flag)  %>% 
    mutate(x = x + (baseline_base[1] %>% as.numeric()),
           y = y + (baseline_base[2] %>% as.numeric()),
           z = z + (baseline_base[3] %>% as.numeric()))
    
  
  out <- left_join(joined_full, 
                    list[[scanner_id_base]] %>% mutate(id = 1:n()) %>% select(-sensor_flag) ,
                    by = c("x","y","z")) %>%
  arrange(id, sensor_flag) %>%
  ungroup()
  
  return(out)
}

matched <- tibble(scanner_i = numeric(), 
                  beacon_i = numeric(), 
                  scanner_j = numeric(), 
                  beacon_j = numeric())

input_unaligned <- lapply(1:length(input), function(x){lapply(1:(nrow(input[[x]]) - 11), 
                                                              function(y){set_reference_unaligned(x,y)})})

for (i in 1:(length(input_unaligned)-1)){
  for (j in (i+1):length(input_unaligned)){
    # print(j)
    for (align_i in 1:(length(input_unaligned[[i]]))){
      for (align_j in 1:(length(input_unaligned[[j]]))){
        if (check_match(input_unaligned[[i]][[align_i]], input_unaligned[[j]][[align_j]]) == TRUE){
          # print(paste0("Match! Scanner ",i," beacon ", align_i, " matches scanner ",j," beacon ",align_j))
          matched <- rbind(matched, tibble(scanner_i = i, 
                                           beacon_i = align_i,
                                           scanner_j = j, 
                                           beacon_j = align_j))
        }
      }
    }
  }
}

matched_full <- matched %>% bind_rows(matched %>% 
                                        rename(scanner_j = scanner_i, 
                                               scanner_i = scanner_j,
                                               beacon_i = beacon_j, 
                                               beacon_j = beacon_i))

# order nodes so that as we join we arrive at element 1 / sensor 0

order <- c(1)
current_set <- c(1)

while (length(order) < length(input)){
  current_set <- matched_full %>% 
    distinct(scanner_i ,scanner_j) %>% 
    filter(scanner_i %in% current_set) %>% 
    distinct(scanner_j) %>% 
    arrange(scanner_j) %>% 
    pull(scanner_j)
  
  current_set <- setdiff(current_set, order)
  order <- c(order, current_set)
}

order <- order %>% rev()

# Get attaching the beacon locations to a parent sensor until we bring everything back to sensor 0 

input_filled <- input

for (i in order[-36]){
  tmp <- matched_full %>% filter(scanner_j == i) %>% slice(1)
  
  input_filled[[tmp$scanner_i]] <- set_reference_aligned(tmp$scanner_i, 
                                             tmp$beacon_i, 
                                             tmp$scanner_j, 
                                             tmp$beacon_j, 
                                             input_filled)
  
  matched_full <- matched_full %>% filter(scanner_i != i & scanner_j != i)
}

#Pt 1
(input_filled[[1]] %>% filter(sensor_flag == 0) %>% nrow())

#Pt 2
sensors <- input_filled[[1]] %>% filter(sensor_flag == 1) %>% select(x,y,z)

max_manhattan <- 0

for (i in 1:(nrow(sensors)-1)){
  for (j in (i+1):nrow(sensors)){
    manhattan <- sensors %>% slice(i) %>%
      cbind(sensors %>% slice(j) %>% rename(x2 = x, y2 = y, z2 = z)) %>%
      mutate(manhattan = abs(x-x2) + abs(y-y2) + abs(z-z2)) %>%
      pull(manhattan)
    
    max_manhattan <- max(max_manhattan, manhattan)
  }
}

(max_manhattan)

ptm - proc.time()