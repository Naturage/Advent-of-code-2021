source(here::here("common functions.R"))

day <- 10

# Data input
input <- get_input(day)

costs <- c(3,57,1197,25137)
names(costs) <- c(")","]","}",">")

close_a_bracket <- function(line){
  out <- str_replace_all(line, "\\<\\>|\\(\\)|\\{\\}|\\[\\]","")
  if(out != line){
    out <- close_a_bracket(out)
  }
  return(out)
}

#strtoi fails past 2^31. A shame.
pencimal_to_dec <- function(input){
  y <- as.numeric(strsplit(input, "")[[1]])
  sum(y * 5^rev((seq_along(y)-1)))
}

cleaned_input <- sapply(input,close_a_bracket)

# Pt 1
detected_corruptions <- cleaned_input %>% str_extract("\\>|\\)|\\}|\\]") %>% .[!is.na(.)] %>% table()

(score <- sapply(names(costs), function(x){detected_corruptions[x] * costs[x]}) %>% sum())

# Pt 2

incomplete_brackets_scored <- cleaned_input %>% 
  str_subset(.,"\\>|\\)|\\}|\\]", negate = TRUE) %>% 
  str_replace_all("\\(","1") %>%
  str_replace_all("\\[","2") %>%
  str_replace_all("\\{","3") %>%
  str_replace_all("\\<","4") %>%
  stri_reverse() %>%
  sapply(pencimal_to_dec)

(incomplete_brackets_scored %>% median)
