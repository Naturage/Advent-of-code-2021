source(here::here("common functions.R"))

day <- 7

# Data input
input <- get_input(day) %>% strsplit(split = ",") %>% unlist() %>% as.numeric()

# Pt 1: I know by heart optimum is at a median for linear error. Also if it's not integer we're still god - any number between two medians will do.
(sum(abs(input - median(input))))

# Pt 2: this is almost quadratic error so has to be near mean in median's direction.
mean <- mean(input)
median <- median(input)
direction <- sign(median - mean)

score <- vector()

for (i in 0:5){
  guess <- ceiling(mean - direction) + i * direction
  score[i+1] = sum(abs(input - guess) + (input - guess)**2)/2
}

(min(score))
