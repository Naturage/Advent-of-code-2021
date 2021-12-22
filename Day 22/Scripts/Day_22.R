ptm <- proc.time()

source(here::here("common functions.R"))

day <- 22

# Data input
input <- get_input(day) %>% 
  str_match("(off|on) x=(-?\\d+)\\.\\.(-?\\d+),y=(-?\\d+)\\.\\.(-?\\d+),z=(-?\\d+)\\.\\.(-?\\d+)") %>%
  tibble(on_off = .[,2],
         xmin = .[,3], xmax = .[,4],
         ymin = .[,5], ymax = .[,6],
         zmin = .[,7], zmax = .[,8]) %>%
  select(-1) %>%
  mutate(across(2:7, as.numeric)) %>%
  mutate(on_off = ifelse(on_off == "on",1,-1))
  
intersect_cubes <- function(total_cube_list, new_cube){
  total_cube_list %>% 
    filter(!(xmin > new_cube$xmax | new_cube$xmin > xmax |
             ymin > new_cube$ymax | new_cube$ymin > ymax |
             zmin > new_cube$zmax | new_cube$zmin > zmax)) %>%
    rowwise() %>%
    mutate(on_off = -on_off,
      xmin = max(xmin, new_cube$xmin),
      ymin = max(ymin, new_cube$ymin),
      zmin = max(zmin, new_cube$zmin),
      xmax = min(xmax, new_cube$xmax),
      ymax = min(ymax, new_cube$ymax),
      zmax = min(zmax, new_cube$zmax)
    ) %>%
  group_by(xmin, xmax, ymin, ymax, zmin, zmax) %>% 
  summarise(on_off = sum(on_off), .groups = "drop") %>%
  filter(on_off != 0)
}

# Pt 1 

input_p1 <- input %>% 
  rowwise() %>% 
  filter(min(xmin, ymin, zmin) >= -50 & max(xmax, ymax, zmax) <= 50) %>%
  ungroup()

total_cube_list <- input_p1[1,] %>% tibble()

for (i in 2:nrow(input_p1)){
  total_cube_list <- bind_rows(total_cube_list, intersect_cubes(total_cube_list, input_p1[i,]))
  if (input_p1[i,"on_off"] == 1){
    total_cube_list <- rbind(total_cube_list, input_p1[i,])
  }
  print(i)
}

(total_cube_list %>% 
  mutate(volume = on_off * 
           (xmax - xmin + 1) * 
           (ymax - ymin + 1) *
           (zmax - zmin + 1)) %>%
  summarise(sum(volume)) %>% as.numeric())

# Pt 2

input_p2 <- input

total_cube_list <- input_p2[1,] %>% tibble()

for (i in 2:nrow(input_p2)){
  total_cube_list <- bind_rows(total_cube_list, intersect_cubes(total_cube_list, input_p2[i,]))
  if (input_p2[i,"on_off"] == 1){
    total_cube_list <- rbind(total_cube_list, input_p2[i,])
  }
  print(i)
}

options(scipen = 999)

(total_cube_list %>% 
    mutate(volume = on_off * 
             (xmax - xmin + 1) * 
             (ymax - ymin + 1) *
             (zmax - zmin + 1)) %>%
    summarise(sum(volume)) %>% as.numeric())

proc.time() - ptm