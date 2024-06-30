#' modifying anlz_fibmatrix function to work with entero data
#' committing anlz_fibmatrix code as original copy to make it easier to see differences

anlz_enteromatrix <- function (fibdata, yrrng = NULL, stas = NULL, lagyr = 3) 
{
    geomean <- function(x) {
        prod(x)^(1/length(x))
    }
    if (is.null(yrrng)) 
        yrrng <- c(1985, max(fibdata$yr, na.rm = T))
    stasval <- fibdata %>% dplyr::filter(yr >= (yrrng[1] - (lagyr - 
                                                                1)) & yr <= yrrng[2]) %>% dplyr::filter(!is.na(fcolif) | 
                                                                                                            fcolif < 0) %>% dplyr::summarise(nyrs = length(unique(yr)), 
                                                                                                                                             .by = "epchc_station") %>% dplyr::filter(nyrs >= lagyr) %>% 
        dplyr::pull(epchc_station) %>% unique()
    if (is.null(stas)) 
        stas <- stasval
    chk <- stas %in% fibdata$epchc_station
    if (any(!chk)) 
        stop("Station(s) not found in fibdata: ", paste(stas[!chk], 
                                                        collapse = ", "))
    chk <- !stas %in% stasval
    if (any(chk)) 
        stop("Stations with insufficient data for lagyr: ", 
             paste(stas[chk], collapse = ", "))
    dat <- fibdata %>% dplyr::filter(epchc_station %in% stas) %>% 
        dplyr::filter(yr >= (yrrng[1] - (lagyr - 1)) & yr <= 
                          yrrng[2]) %>% dplyr::filter(!is.na(fcolif) | fcolif < 
                                                          0) %>% summarise(gmean = geomean(fcolif), sumgt400 = sum(fcolif > 
                                                                                                                       400), station_tot = dplyr::n(), .by = c("epchc_station", 
                                                                                                                                                               "yr")) %>% dplyr::arrange(epchc_station, yr) %>% dplyr::mutate(sumgt400 = stats::filter(sumgt400, 
                                                                                                                                                                                                                                                       rep(1, lagyr), sides = 1, method = "convolution"), station_tot = stats::filter(station_tot, 
                                                                                                                                                                                                                                                                                                                                      rep(1, lagyr), sides = 1, method = "convolution"), .by = "epchc_station") %>% 
        dplyr::mutate(exceed_10_400_prob = pbinom(sumgt400 - 
                                                      1, station_tot, 0.1, lower.tail = FALSE), exceed_30_400_prob = pbinom(sumgt400 - 
                                                                                                                                1, station_tot, 0.3, lower.tail = FALSE), exceed_50_400_prob = pbinom(sumgt400 - 
                                                                                                                                                                                                          1, station_tot, 0.5, lower.tail = FALSE), exceed_75_400_prob = pbinom(sumgt400 - 
                                                                                                                                                                                                                                                                                    1, station_tot, 0.75, lower.tail = FALSE))
    dat$MWQA <- NA
    dat$MWQA[dat$exceed_10_400_prob >= 0.1] <- "A"
    dat$MWQA[dat$exceed_10_400_prob < 0.1 & dat$exceed_30_400_prob >= 
                 0.1] <- "B"
    dat$MWQA[dat$exceed_30_400_prob < 0.1 & dat$exceed_50_400_prob >= 
                 0.1] <- "C"
    dat$MWQA[dat$exceed_50_400_prob < 0.1 & dat$exceed_75_400_prob >= 
                 0.1] <- "D"
    dat$MWQA[dat$exceed_75_400_prob < 0.1] <- "E"
    out <- dat %>% dplyr::select(yr, epchc_station, gmean, cat = MWQA) %>% 
        dplyr::mutate(epchc_station = factor(epchc_station, 
                                             levels = stas)) %>% dplyr::filter(yr >= yrrng[1] & 
                                                                                   yr <= yrrng[2]) %>% tidyr::complete(yr = yrrng[1]:yrrng[2], 
                                                                                                                       epchc_station)
    return(out)
}