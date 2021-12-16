source(here::here("common functions.R"))

day <- 16

hex_to_bin <- matrix(c("0", "0000",
                       "1", "0001",
                       "2", "0010",
                       "3", "0011",
                       "4", "0100",
                       "5", "0101",
                       "6", "0110",
                       "7", "0111",
                       "8", "1000",
                       "9", "1001",
                       "A", "1010",
                       "B", "1011",
                       "C", "1100",
                       "D", "1101",
                       "E", "1110",
                       "F", "1111"), 16, byrow = TRUE)
                     

# Data input
input <- get_input(day) %>% strsplit(split = "") %>% unlist() %>% sapply(., function(x){hex_to_bin[hex_to_bin[,1] == x,2]}) %>% strsplit(split = "") %>% unlist()

# Helper functions:

# Find version and type, drop first 6 chars of the string.
find_version_type <- function(string){
  version <- string[1:3] %>% paste(collapse = "") %>% strtoi(base = 2)
  type <- string[4:6] %>% paste(collapse = "") %>% strtoi(base = 2)
  string_remainder <- string[-1:-6]
  return(list(version,type,string_remainder))
}

# Find length's type, length, and drop 12 or 16 chars of the string.
find_length <- function(string){
  length_type <- string[1]
  if (length_type == "0"){
    length <- string[2:16] %>% paste(collapse = "") %>% strtoi(base = 2)
    string_remainder <- string[-1:-16]
  } else {
    length <- string[2:12] %>% paste(collapse = "") %>% strtoi(base = 2)
    string_remainder <- string[-1:-12]
  }
  return(list(length_type, length, string_remainder))
}

# Read hex digits, dropping every set of 5 and keeping the 4 meaningful ones in the number. Stop at 0-signed 5bit.
find_seq_bits <- function(string){
  another_bit <- "1"
  numbers <- c()
  while (another_bit == "1"){
    another_bit <- string[1] %>% unlist
    numbers <- c(numbers, string[2:5] %>% unlist)
    string <- string[-1:-5]
  }
  string_remainder <- string
  return(list(numbers,string_remainder))
}

# Main parser.
parse <- function(string){
  
  total_version_sum <- 0
  value <- c()
  
  out_tmp <- find_version_type(string)
  
  version <- out_tmp[[1]]
  type <- out_tmp[[2]]
  total_version_sum <- total_version_sum + version
  string <- out_tmp[[3]]
  
  print(paste0("v", version, " , type ", type))
  
  if (type == 4){
  
    out_tmp <- find_seq_bits(string)
    
    numbers <- out_tmp[[1]] %>% paste(collapse = "") %>% strtoi_full()
    string <- out_tmp[[2]]
    
    print(paste0("literal number: ", numbers %>% as.character))
    
    value <- c(value, numbers)
    
  } else if (type != 4){
    
    out_tmp <- find_length(string)
    
    length_type <- out_tmp[[1]]
    length <- out_tmp[[2]]
    string <- out_tmp[[3]]
    
    print(paste0("Operator: length ",length, " ", ifelse(length_type == "0", "bits", "packets"), " (", length_type, ")"))
    
    if (length_type == "0"){
      
      string_tmp <- string[1:length]
      
      while(length(string_tmp) > 0){
        
        out_tmp <- parse(string_tmp)
        
        value <- c(value, out_tmp[[1]])
        total_version_sum <- total_version_sum + out_tmp[[2]]
        string_tmp <- out_tmp[[3]]
      }
      
      string <- string[-1:-length]
      
    } else {
      
      for (i in 1:length) {
        
        out_tmp <- parse(string)
        
        value <- c(value, out_tmp[[1]])
        total_version_sum <- total_version_sum + out_tmp[[2]]
        string <- out_tmp[[3]]
      }
    }
    
    if (type == 0){
      value <- sum(value)
    } else if (type == 1) {
      value <- prod(value)
    } else if (type == 2) {
      value <- min(value)
    } else if (type == 3) {
      value <- max(value)
    } else if (type == 5) {
      value <- ifelse(value[1] > value[2], 1, 0)
    } else if (type == 6) {
      value <- ifelse(value[1] < value[2], 1, 0)
    } else if (type == 7) {
      value <- ifelse(value[1] == value[2], 1, 0)
    }
        
    print(paste0("Closing operator, with value ", paste(value, collapse = ", ")))
  }
  
  return(list(value, total_version_sum, string))
}

out <- parse(input)

# Pt 1
(out[[2]])

# Pt 2
(out[[1]])