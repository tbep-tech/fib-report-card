#' modifying anlz_fibmap function to work with entero data
#' removed areasel stuff
#' removed class and ind stuff because we're only using Enterococcus in 2 or 3M
#' wetdry means, is there a column for wet_sample?


anlz_enteromap <- function (fibdata, yrsel = NULL, mosel = NULL, wetdry = FALSE) 
{
    levs <- util_fiblevs()
    cols <- c("#2DC938", "#E9C318", "#EE7600", "#CC3231")
    out <- fibdata %>% select(station, yr, 
                              mo, Latitude, Longitude, ecocci) %>% 
        dplyr::mutate(cat = cut(ecocci, breaks = levs$ecoccilev, right = F, levs$ecoccilbs), 
                      col = cut(ecocci, breaks = levs$ecoccilev, right = F, cols), 
                      col = as.character(col),
                      ind = "Enterococcus",
                      indnm = "ecocci",
                      conc = ecocci)
    if (wetdry == TRUE) {
        stopifnot("fibdat does not contain a 'wet_sample' column" = exists("wet_sample", fibdata))
        out$wet_sample = fibdata$wet_sample 
    }
    if (!is.null(yrsel)) {
        yrsel <- match.arg(as.character(yrsel), unique(out$yr))
        out <- out %>% dplyr::filter(yr %in% yrsel)
    }
    if (!is.null(mosel)) {
        mosel <- match.arg(as.character(mosel), 1:12)
        out <- out %>% dplyr::filter(mo %in% mosel)
    }
        chk <- length(na.omit(out$cat)) == 0
    if (chk) 
        stop("No FIB data for ", paste(lubridate::month(mosel, 
                                                        label = T), yrsel, sep = " "), ", ", areasel)
    return(out)
}
