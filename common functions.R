library(here)
library(dplyr)
library(httr)
library(readr)
library(tidyr)
library(stringr)
library(stringi)

get_input <- function(day){
  suppressWarnings(session_id <- readLines(here("session.txt")))
  
  input <- GET(paste0("https://adventofcode.com/2021/day/",day,"/input"),
               set_cookies(session = session_id)) %>%
    content(encoding = 'UTF-8') %>%
    read_lines()
  
  write_rds(input, here(paste0("day ",day), "data","input.rds"))
  
  return(input)
}

strtoi_full <- function(binary_num, base = 2){
  if (binary_num == ""){
    return(0)
  } else if (nchar(binary_num) < 30){
    return(strtoi(binary_num, base = 2))
  } else
    return(strtoi_full(str_sub(binary_num, end = -31)) * 2**30 + strtoi(str_sub(binary_num, start = -30), base = 2))
}