source(here::here("R", "raindl.R"))
test <- read_importrain(curyr = 2021, catch_pixels = catch_pixels, mos = 1:3, quiet = F)
