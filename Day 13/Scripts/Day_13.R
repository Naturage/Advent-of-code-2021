source(here::here("common functions.R"))

day <- 13

# Data input
input <- get_input(day)

dots <- input %>% .[grepl("(\\d)+,(\\d)+",.)] %>% 
  strsplit(split = ",") %>%
  as.data.frame() %>% t() %>% as.data.frame() %>%
  mutate_all(as.numeric)
colnames(dots) <- c("x", "y")
rownames(dots) <- NULL

instructions <- input %>% 
  .[grepl("fold along (x|y)=(\\d+)",.)] %>% 
  str_match(.,"fold along (x|y)=(\\d+)") %>% .[,2:3]

fold_along_x <- function(input_data, line){
  input_data %>% 
    mutate(x = ifelse(x > line, line - (x - line), x)) %>% 
    distinct(x,y)
}

fold_along_y <- function(input_data, line){
  input_data %>% 
    mutate(y = ifelse(y > line, line - (y - line), y)) %>% 
    distinct(x,y)
}

# Pt 1

dots_p1 <- dots

for (i in 1:1){
  instruction_step <- instructions[i,]
  if (instruction_step[1] == "x"){
    dots_p1 <- fold_along_x(dots_p1, instruction_step[2] %>% as.numeric())
  } else {
    dots_p1 <- fold_along_y(dots_p1, instruction_step[2] %>% as.numeric())
  }
}

(nrow(dots_p1))

# Pt 2

dots_p2 <- dots

for (i in 1:nrow(instructions)){
  instruction_step <- instructions[i,]
  if (instruction_step[1] == "x"){
    dots_p2 <- fold_along_x(dots_p2, instruction_step[2] %>% as.numeric())
  } else {
    dots_p2 <- fold_along_y(dots_p2, instruction_step[2] %>% as.numeric())
  }
}

plot(dots_p2 %>% mutate(y = -y))