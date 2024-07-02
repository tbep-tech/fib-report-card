#' download daily rainfall data in 10-year chunks

library(tictoc)


#' read in catchments, associate with pixels, and
#' set up read_importrain function
source(here::here("R", "raindl.R"))

# set up function to loop through several years
read_importrain_many <- function(yrs,
                                 quiet = FALSE){
    yrs <- yrs
    prcp_out <- list()
    
    for(i in seq_along(yrs)){
        yr = yrs[[i]]
        prcptmp <- read_importrain(curyr = yr, 
                                   catch_pixels = catch_pixels,
                                   quiet = quiet)
        prcp_out[[i]] <- prcptmp
        rm(prcptmp)
        # pause for 5 seconds between years
        Sys.sleep(5)
    }
    
    prcpdat <- bind_rows(prcp_out)
    
    return(prcpdat)
}

# start the multi-looping ----
tic("all")

tic("1995.2004")
prcp_1995.2004 <- read_importrain_many(yrs = 1995:2004, quiet = TRUE)
toc()

Sys.sleep(5)

tic("2005.2014")
prcp_2005.2014 <- read_importrain_many(yrs = 2005:2014, quiet = TRUE)
toc()

Sys.sleep(5)

tic("2015.2023")
prcp_2015.2023 <- read_importrain_many(yrs = 2015:2023, quiet = TRUE)
toc()

toc()
# end multi-looping


# bind it all together
prcp_all <- bind_rows(prcp_1995.2004,
                      prcp_2005.2014,
                      prcp_2015.2023)


save(prcp_all, file = here::here("data", "precipitation", "daily_rain_stns_1995-2023.RData"))