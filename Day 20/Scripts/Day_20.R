source(here::here("common functions.R"))

day <- 20

# Data input
# readLines(here("day 20","data","example.txt"))
input <- get_input(day) %>% str_replace_all("\\.","0") %>% str_replace_all("#","1")

key <- input[1] %>% strsplit("") %>% unlist %>% as.numeric

image <- input[-1:-2] %>% strsplit("") %>% unlist %>% as.numeric %>% matrix(nrow = length(input) - 2, byrow = TRUE)

add_rim <- function(image, rim_type){
  image %>% 
    rbind(rep(rim_type, ncol(.)), . , rep(rim_type, ncol(.))) %>%
    cbind(rep(rim_type, nrow(.)), . , rep(rim_type, nrow(.)))
}

remove_rim <- function(image){
  image[2:(nrow(image)-1),2:(ncol(image)-1)]
}

enhance <- function(row, col, img = image){
  img[(row-1):(row+1),(col-1):(col+1)] %>% 
    t() %>% 
    as.vector() %>% 
    paste(collapse = "") %>% 
    strtoi(base = 2) %>%
    sum(.,1) %>%
    key[.]
}

# Pt 1
image_p1 <- image

for (iter in 1:2){
  image_p1 <- image_p1 %>% add_rim((iter-1)%%2) %>% add_rim((iter-1)%%2)
  
  tmp <- image_p1
  
  for (row in (2:(nrow(image_p1)-1))){
    for (col in (2:(ncol(image_p1)-1))){
      tmp[row,col] <- enhance(row, col, image_p1)
    }
  }
  
  image_p1 <- tmp %>% remove_rim()
}

(sum(image_p2))
# Pt 2

image_p2 <- image

for (iter in 1:50){
  image_p2 <- image_p2 %>% add_rim((iter-1)%%2) %>% add_rim((iter-1)%%2)
  
  tmp <- image_p2
  
  for (row in (2:(nrow(image_p2)-1))){
    for (col in (2:(ncol(image_p2)-1))){
      tmp[row,col] <- enhance(row, col, image_p2)
    }
  }
  
  image_p2 <- tmp %>% remove_rim()
}

(sum(image_p2))