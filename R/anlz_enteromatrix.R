#' modifying anlz_fibmatrix function to work with entero data
#' committing anlz_fibmatrix code as original copy to make it easier to see differences
#' 
#' changes: 
#' min of yrrng to min of data
#' epchc_station to station
#' fcolif to ecocci (following format used in other fibdat functions)
#' threshold of 400 changed to 70

anlz_enteromatrix <- function (fibdata, yrrng = NULL, stas = NULL, lagyr = 3) 
{
    geomean <- function(x) {
        prod(x)^(1/length(x))
    }
    if (is.null(yrrng)) 
        yrrng <- c(min(fibdata$yr, na.rm = T), max(fibdata$yr, na.rm = T))
    stasval <- fibdata %>% dplyr::filter(yr >= (yrrng[1] - (lagyr - 1)) & yr <= yrrng[2]) %>% 
        dplyr::filter(!is.na(ecocci) | ecocci < 0) %>% 
        dplyr::summarise(nyrs = length(unique(yr)), .by = "station") %>% 
        dplyr::filter(nyrs >= lagyr) %>% 
        dplyr::pull(station) %>% unique()
    if (is.null(stas)) 
        stas <- stasval
    chk <- stas %in% fibdata$station
    if (any(!chk)) 
        stop("Station(s) not found in fibdata: ", paste(stas[!chk], 
                                                        collapse = ", "))
    chk <- !stas %in% stasval
    if (any(chk)) 
        stop("Stations with insufficient data for lagyr: ", 
             paste(stas[chk], collapse = ", "))
    dat <- fibdata %>% dplyr::filter(station %in% stas) %>% 
        dplyr::filter(yr >= (yrrng[1] - (lagyr - 1)) & yr <= 
                          yrrng[2]) %>% dplyr::filter(!is.na(ecocci) | ecocci < 
                                                          0) %>% 
        summarise(gmean = geomean(ecocci), 
                  sumgt70 = sum(ecocci > 70), 
                  station_tot = dplyr::n(), 
                  .by = c("station", "yr")) %>% 
        dplyr::arrange(station, yr) %>% 
        dplyr::mutate(sumgt70 = stats::filter(sumgt70, rep(1, lagyr), sides = 1, method = "convolution"), 
                      station_tot = stats::filter(station_tot,  rep(1, lagyr), sides = 1, method = "convolution"), 
                      .by = "station") %>% 
        dplyr::mutate(exceed_10_70_prob = pbinom(sumgt70 - 1, station_tot, 0.1, lower.tail = FALSE), 
                      exceed_30_70_prob = pbinom(sumgt70 - 1, station_tot, 0.3, lower.tail = FALSE), 
                      exceed_50_70_prob = pbinom(sumgt70 - 1, station_tot, 0.5, lower.tail = FALSE), 
                      exceed_75_70_prob = pbinom(sumgt70 - 1, station_tot, 0.75, lower.tail = FALSE))
    dat$MWQA <- NA
    dat$MWQA[dat$exceed_10_70_prob >= 0.1] <- "A"
    dat$MWQA[dat$exceed_10_70_prob < 0.1 & dat$exceed_30_70_prob >= 
                 0.1] <- "B"
    dat$MWQA[dat$exceed_30_70_prob < 0.1 & dat$exceed_50_70_prob >= 
                 0.1] <- "C"
    dat$MWQA[dat$exceed_50_70_prob < 0.1 & dat$exceed_75_70_prob >= 
                 0.1] <- "D"
    dat$MWQA[dat$exceed_75_70_prob < 0.1] <- "E"
    out <- dat %>% dplyr::select(yr, station, gmean, cat = MWQA) %>% 
        dplyr::mutate(station = factor(station, 
                                       levels = stas)) %>% 
        dplyr::filter(yr >= yrrng[1] & yr <= yrrng[2]) %>% 
        tidyr::complete(yr = yrrng[1]:yrrng[2], station)
    return(out)
}
