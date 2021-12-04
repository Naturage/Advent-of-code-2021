source(here::here("common functions.R"))

day <- 2

# Data input
input <- get_input(day) %>% 
  as.data.frame() %>% 
  separate(1, into = c("direction", "distance"), convert = TRUE)

# Pt 1
unaimed_input <- input %>% 
  mutate(dist_horiz = cumsum(distance * (direction == "forward")),
         dist_vert = cumsum(distance * (direction == "down")) - cumsum(distance * (direction == "up")))

((unaimed_input %>% tail(1) %>% mutate(ans = dist_horiz * dist_vert) %>% select(ans) %>% as.numeric()))

# Pt 2
aimed_input <- input %>% 
  mutate(aim = cumsum(distance * (direction == "down")) - cumsum(distance * (direction == "up"))) %>%
  mutate(dist_horiz = cumsum(distance * (direction == "forward")),
         dist_vert  = cumsum(distance * (direction == "forward") * aim))

(aimed_input %>% tail(1) %>% mutate(ans = dist_horiz * dist_vert) %>% select(ans) %>% as.numeric())

