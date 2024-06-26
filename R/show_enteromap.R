#' Map Enterococcus results by month, year, and location
#'
#' @inheritParams anlz_enteromap
#'
#' @return A \code{leaflet} map for the selected year, month, and area showing stations and FIB concentration category
#'
#' @details Placing the mouse cursor over an item on the map will reveal additional information about a station.
#'
#' @concept anlz
#'
#' @seealso \code{\link{anlz_enteromap}} for details on the categories
#' @export
#'
#' @examples

show_enteromap <- function(fibdata, yrsel, mosel){
    
    # get categories
    fibmap <- anlz_enteromap(fibdata, yrsel = yrsel, mosel = mosel)
    
    # create the object to map
    tomap <- fibmap %>%
        dplyr::filter(!is.na(Longitude)) %>%
        dplyr::filter(!is.na(Latitude)) %>%
        dplyr::filter(!is.na(cat)) %>%
        sf::st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326, remove = F) %>%
        dplyr::mutate(
            colnm = factor(col,
                           levels = c('#2DC938', '#E9C318', '#EE7600', '#CC3231'),
                           labels = c('green', 'yellow', 'orange', 'red')
            ),
            conc = round(conc, 1),
            cls = 'Marine'
        ) %>%
        tidyr::unite('grp', indnm, colnm, remove = F)
    
    # create levels for group, must match order of icons list
    levs <- expand.grid(levels(tomap$colnm), unique(tomap$indnm)) %>%
        unite('levs', Var2, Var1) %>%
        pull(levs)
    
    # get correct levels
    tomap <- tomap %>%
        dplyr::mutate(
            grp = factor(grp, levels = levs),
            lab = paste0('<html>Station Number: ', station, '<br>Class: ', cls, ' (<i>', ind, '</i>)<br> Category: ', cat, ' (', conc, '/100mL)</html>')
        ) %>%
        dplyr::select(-colnm, -indnm)
    
    # create custom icon list for fib categories
    icons <- leaflet::iconList(
        ecoli_green <- leaflet::makeIcon(iconUrl = system.file('ecoli_green.png', package = 'tbeptools'),
                                         iconWidth = 18, iconHeight = 18),
        ecoli_yellow <- leaflet::makeIcon(iconUrl = system.file('ecoli_yellow.png', package = 'tbeptools'),
                                          iconWidth = 18, iconHeight = 18),
        ecoli_orange <- leaflet::makeIcon(iconUrl = system.file('ecoli_orange.png', package = 'tbeptools'),
                                          iconWidth = 18, iconHeight = 18),
        ecoli_red <- leaflet::makeIcon(iconUrl = system.file('ecoli_red.png', package = 'tbeptools'),
                                       iconWidth = 18, iconHeight = 18),
        ecocci_green <- leaflet::makeIcon(iconUrl = system.file('ecocci_green.png', package = 'tbeptools'),
                                          iconWidth = 18, iconHeight = 18),
        ecocci_yellow <- leaflet::makeIcon(iconUrl = system.file('ecocci_yellow.png', package = 'tbeptools'),
                                           iconWidth = 18, iconHeight = 18),
        ecocci_orange <- leaflet::makeIcon(iconUrl = system.file('ecocci_orange.png', package = 'tbeptools'),
                                           iconWidth = 18, iconHeight = 18),
        ecocci_red <- leaflet::makeIcon(iconUrl = system.file('ecocci_red.png', package = 'tbeptools'),
                                        iconWidth = 18, iconHeight = 18)
    )
    
    # legend as HTML string
    levs <- util_fiblevs()
    leg <- tibble::tibble(
        src = paste0('https://github.com/tbep-tech/tbeptools/blob/master/inst/', basename(sapply(icons, `[[`, 1)), '?raw=true'),
        brk = c(levs$ecolilbs, levs$ecoccilbs)
    ) %>%
        tidyr::unite('val', src, brk, sep = "' style='width:10px;height:10px;'> ") %>%
        dplyr::mutate(
            val = paste0("<img src='", val)
        ) %>%
        dplyr::pull(val)
    ecolileg <- leg %>%
        grep('ecoli', ., value = T) %>%
        paste(collapse = '<br/>') %>%
        paste0('<b>Freshwater (<em>E. Coli</em>)</b><br/>#/100mL<br/>', .)
    ecoccileg <- leg %>%
        grep('ecocci', ., value = T) %>%
        paste(collapse = '<br/>') %>%
        paste0('<b>Marine (<em>Enterococcus</em>)</b><br/>#/100mL<br/>', .)
    
    # create map
    out <- util_map(tomap) %>%
        leaflet::addMarkers(
            data = tomap,
            lng = ~Longitude,
            lat = ~Latitude,
            icon = ~icons[as.numeric(grp)],
            label = ~lapply(as.list(lab), util_html)
        ) %>%
        leaflet::addControl(html = ecoccileg, position = 'topright')
    
    return(out)
    
}