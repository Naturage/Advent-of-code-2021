ptm <- proc.time()

source(here::here("common functions.R"))

day <- 17

# Data input
input <- get_input(day) %>% str_match("target area: x=(-?\\d+)\\.\\.(-?\\d+), y=(-?\\d+)\\.\\.(-?\\d+)") %>% .[-1] %>% as.numeric()

xmin <- input[1]
xmax <- input[2]
ymin <- input[3]
ymax <- input[4]

# Pt 1
# Did it by hand already, but just getting all things in order

# Confirm that if probe flies for very long we can pick x which slows down to 0 and hits target
# for that we need x(x+1)/2 between xmin and xmax, i.e., 2*xmin + 0.25 <= (x+0.5)^2 <= 2*xmax + 0.25.

v_x_long_min <- sqrt(2*xmin + 0.25) - 0.5
v_x_long_max <- sqrt(2*xmax + 0.25) - 0.5

ceiling(v_x_long_min) < floor(v_x_long_max)
# Check - 19 or 20 work.

# Which means we want to toss with a set x and as high y as possible.
# start from velocity y, in time 2y we will be at height 0 again and velocity -y-1;
# in the next turn we'll move down by -y-1 and that, at best, is our ymin.
# in which case out max height is ((-ymin) -1)(-ymin)/2

((-ymin - 1)*(-ymin)/2)

# Pt 2

# Do this in 2 parts - when flight is >= 20 (long pitch - ends with v_x = 0) and <20 (yeet - ends with positiv x velocity)

solutions <- matrix(ncol = 4)

long_pitch_v_x_options <- c(ceiling(v_x_long_min):floor(v_x_long_max))

# obviously the y has to be positive or we are far below where we need to be, so we'll again be at 0, falling at speed -y-1. Iterate over # of turns from then till goal.

for (t in 1:15){
  y_to_fall_min <- min(0, ymin + t*(t+1)/2)
  y_to_fall_max <- min(0, ymax + t*(t+1)/2)
  v_y_longpitch_min <- -floor(y_to_fall_max/t)
  v_y_longpitch_max <- -ceiling(y_to_fall_min/t)
  
  # for total time to be >= 20, 2*v_y_longpitch_min + 1 + t has to be >= 20, v_y_longpitch_min >= 10 - (t+1) / 2
  v_y_longpitch_min <- max(v_y_longpitch_min, ceiling(ceiling(v_x_long_min/2) - (t+1) / 2))
  
  if (v_y_longpitch_min <= v_y_longpitch_max){
    long_pitch_v_y_options <- c(v_y_longpitch_min:v_y_longpitch_max)
  } else {
    long_pitch_v_y_options <- c()
  }
  
  for (x in long_pitch_v_x_options){
    for (y in long_pitch_v_y_options){
      solutions <- rbind(solutions, c(x,y,t,1))
    }
  }
}

# Now the yeets.

# With total t < 20, we need to count possibilities for both c and y. Luckily, the logic is very similar.

for (t in 1:ceiling(v_x_long_min)){
  x_aim_min <- xmin + t*(t-1)/2
  x_aim_max <- xmax + t*(t-1)/2
  y_aim_min <- ymin + t*(t-1)/2
  y_aim_max <- ymax + t*(t-1)/2
  
  yeet_v_x_min <- min(x_aim_min/t, x_aim_max/t)
  yeet_v_x_max <- max(x_aim_min/t, x_aim_max/t)
  yeet_v_y_min <- min(y_aim_min/t, y_aim_max/t)
  yeet_v_y_max <- max(y_aim_min/t, y_aim_max/t)
  
  yeet_v_x_options <- c(ceiling(yeet_v_x_min):floor(yeet_v_x_max))
  yeet_y_y_options <- c(ceiling(yeet_v_y_min):floor(yeet_v_y_max))
  
  for (x in yeet_v_x_options){
    for (y in yeet_y_y_options){
      solutions <- rbind(solutions, c(x,y,t,2))
    }
  }
}

(solutions %>% .[,1:2] %>% .[rowSums(is.na(.)) != ncol(.), ] %>% unique(.) %>% nrow())
