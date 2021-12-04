library(here)
library(dplyr)
library(httr)
library(readr)
library(tidyr)

get_input <- function(day){
  suppressWarnings(session_id <- readLines(here("session.txt")))
  
  input <- GET(paste0("https://adventofcode.com/2021/day/",day,"/input"),
               set_cookies(session = session_id)) %>%
    content(encoding = 'UTF-8') %>%
    read_lines()
  
  write_rds(input, here(paste0("day ",day), "data","input.rds"))
  
  return(input)
}