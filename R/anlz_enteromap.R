#' modifying anlz_fibmap function to work with entero data
#' committing anlz_fibmap code as original copy to make it easier to see differences


anlz_enteromap <- function (fibdata, yrsel = NULL, mosel = NULL) 
{
    levs <- util_fiblevs()
    cols <- c("#2DC938", "#E9C318", "#EE7600", "#CC3231")
    out <- fibdata %>% select(station, class, yr, 
                              mo, Latitude, Longitude, ecoli, ecocci) %>% dplyr::mutate(ind = dplyr::case_when(class %in% 
                                                                                                                   c("1", "3F") ~ "E. coli", class %in% c("2", "3M") ~ 
                                                                                                                   "Enterococcus"), cat = dplyr::case_when(ind == "E. coli" ~ 
                                                                                                                                                               cut(ecoli, breaks = levs$ecolilev, right = F, labels = levs$ecolilbs), 
                                                                                                                                                           ind == "Enterococcus" ~ cut(ecocci, breaks = levs$ecoccilev, 
                                                                                                                                                                                       right = F, levs$ecoccilbs)), col = dplyr::case_when(ind == 
                                                                                                                                                                                                                                               "E. coli" ~ cut(ecoli, breaks = levs$ecolilev, right = F, 
                                                                                                                                                                                                                                                               labels = cols), ind == "Enterococcus" ~ cut(ecocci, 
                                                                                                                                                                                                                                                                                                           breaks = levs$ecoccilev, right = F, cols)), col = as.character(col))
    if (!is.null(yrsel)) {
        yrsel <- match.arg(as.character(yrsel), unique(out$yr))
        out <- out %>% dplyr::filter(yr %in% yrsel)
    }
    if (!is.null(mosel)) {
        mosel <- match.arg(as.character(mosel), 1:12)
        out <- out %>% dplyr::filter(mo %in% mosel)
    }
    if (!is.null(areasel)) {
        areasls <- list(`Alafia River` = c("Alafia River", "Alafia River Tributary"), 
                        `Hillsborough River` = c("Hillsborough River", "Hillsborough River Tributary", 
                                                 "Lake Thonotosassa", "Lake Thonotosassa Tributary", 
                                                 "Lake Roberta"), `Big Bend` = "Big Bend", `Cockroach Bay` = c("Cockroach Bay", 
                                                                                                               "Cockroach Bay Tributary"), `East Lake Outfall` = "East Lake Outfall", 
                        `Hillsborough Bay` = c("Hillsborough Bay", "Hillsborough Bay Tributary"), 
                        `Little Manatee River` = c("Little Manatee River", 
                                                   "Little Manatee River Tributary"), `Lower Tampa Bay` = "Lower Tampa Bay", 
                        `McKay Bay` = c("McKay Bay", "McKay Bay Tributary"), 
                        `Middle Tampa Bay` = c("Middle Tampa Bay", "Middle Tampa Bay Tributary"), 
                        `Old Tampa Bay` = c("Old Tampa Bay", "Old Tampa Bay Tributary"), 
                        `Palm River` = c("Palm River", "Palm River Tributary"), 
                        `Tampa Bypass Canal` = c("Tampa Bypass Canal", "Tampa Bypass Canal Tributary"), 
                        `Valrico Lake` = "Valrico Lake")
        areasel <- match.arg(areasel, names(areasls), several.ok = TRUE)
        out <- out %>% dplyr::filter(area %in% unlist(areasls[areasel]))
    }
    chk <- length(na.omit(out$cat)) == 0
    if (chk) 
        stop("No FIB data for ", paste(lubridate::month(mosel, 
                                                        label = T), yrsel, sep = " "), ", ", areasel)
    return(out)
}
