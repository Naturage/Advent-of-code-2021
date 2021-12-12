source(here::here("common functions.R"))

day <- 12

# Data input
input <- get_input(day) %>% strsplit(split = "-") %>% as.data.frame() %>% t() %>% as.data.frame()
# input <- read_lines(here("day 12","data","example.txt")) %>% strsplit(split = "-") %>% as.data.frame() %>% t() %>% as.data.frame()
rownames(input) <- NULL
colnames(input) <- c("from", "to")
input <- rbind(input, input %>% rename(to = from, from = to))

cave_list <- input %>% distinct(from) %>% pull(from)

lowercase_caves <- cave_list[!grepl("[[:upper:]]", cave_list)]

# all_paths <- list(c("start", "ZZ", "hn"), c("start"))
# path_so_far <- c("start", "ZZ", "hn")
# viable_possibilities <- find_next_cave(path_so_far)
# i <- 1

find_next_cave <- function(path_so_far, p2_flag = FALSE){
  current_loc <- path_so_far[length(path_so_far)]
  if (current_loc == "end"){
    return(c())
  } else {
    all_possibilities <- input %>% filter(from == current_loc) %>% pull(to)
    if (p2_flag == FALSE){
      viable_possibilities <- all_possibilities %>% .[!(. %in% path_so_far & . %in% lowercase_caves)] 
    } else {
      twiced <- path_so_far %>% .[duplicated(.)] %>% .[. %in% lowercase_caves] %>% length() %>% as.logical()
      if (twiced == TRUE){
        viable_possibilities <- all_possibilities %>% .[!(. %in% path_so_far & . %in% lowercase_caves)]
      } else {
        viable_possibilities <- all_possibilities %>% .[!(. %in% c("start"))]
      }
    }
    return(viable_possibilities)
  }
}

update_paths <- function(all_paths, path_so_far, viable_possibilities, i){
  all_paths <- all_paths[-i]
  new_paths <- lapply(viable_possibilities %>% as.list(), function(x){c(path_so_far, x)})
  all_paths <- c(all_paths, new_paths)
  return(all_paths)
}

# Pt 1

i <- 1
all_paths <- list(c("start"))

while(i <= length(all_paths)){
  path_so_far <- all_paths[i] %>% unlist()
  viable_possibilities <- find_next_cave(path_so_far)
  # print(path_so_far)
  # print(viable_possibilities)
  if (length(viable_possibilities) > 0){
    all_paths <- update_paths(all_paths, path_so_far, viable_possibilities, i)
  } else {
    i <- i+1
    # print("no_new_path")
  }
  # print(i)
}

(lapply(all_paths, function(x){x %>% unlist() %>% .[length(.)] == "end"}) %>% unlist() %>% sum())

# Pt 2

ptm <- proc.time()

i <- 1
all_paths <- list(c("start"))

while(i <= length(all_paths)){
  path_so_far <- all_paths[i] %>% unlist()
  viable_possibilities <- find_next_cave(path_so_far, p2_flag = TRUE)
  # print(path_so_far)
  # print(viable_possibilities)
  if (length(viable_possibilities) > 0){
    all_paths <- update_paths(all_paths, path_so_far, viable_possibilities, i)
  } else {
    i <- i+1
    # print("no_new_path")
  }
  # print(i)
  if (i%% 100 == 0){
    print(i)
  }
}

(lapply(all_paths, function(x){x %>% unlist() %>% .[length(.)] == "end"}) %>% unlist() %>% sum())

ptm - proc.time()