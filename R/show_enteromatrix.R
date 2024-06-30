#' modifying show_fibmatrix function to work with entero data
#' meant to but did not in fact commit show_fibmatrix code as original copy 
#' to make it easier to see differences, but they're simple enough
#' 
#' changes:
#' stas to look like anlz_enteromatrix - where they're pulled from data 
#' (leaving this to happen in anlz_enteromatrix)
#' anlz_fibmatrix to anlz_enteromatrix
#' epchc_station to station


show_enteromatrix <- function (fibdata, yrrng = NULL, stas = NULL, 
                               lagyr = 3, txtsz = 3, asreact = FALSE, 
                               nrows = 10, family = NA, plotly = FALSE, 
                               width = NULL, height = NULL) 
{
    cols <- c("#2DC938", "#E9C318", "#EE7600", "#CC3231", "#800080")
    toplo <- anlz_enteromatrix(fibdata, yrrng = yrrng, stas = stas, 
                            lagyr = lagyr)
    yrrng <- range(toplo$yr)
    if (asreact) {
        totab <- toplo %>% dplyr::select(station, yr, 
                                         cat) %>% 
            tidyr::spread(station, cat)
        colfun <- function(x) {
            out <- dplyr::case_when(x %in% "A" ~ "#2DC938", 
                                    x %in% "B" ~ "#E9C318", x %in% "C" ~ "#EE7600", 
                                    x %in% "D" ~ "#CC3231", x %in% "E" ~ "#800080")
            return(out)
        }
        out <- show_reactable(totab, colfun, nrows = nrows, 
                              txtsz = txtsz)
        return(out)
    }
    toplo <- toplo %>% dplyr::filter(!is.na(cat))
    p <- ggplot2::ggplot(toplo, ggplot2::aes(x = station, 
                                             y = yr, fill = cat)) + 
        ggplot2::geom_tile(color = "black") + 
        ggplot2::scale_fill_manual(values = cols, na.value = "white") + 
        ggplot2::scale_y_reverse(expand = c(0, 0), limits = c(yrrng[2] + 0.5, 
                                                              yrrng[1] - 0.5), 
                                 breaks = c(yrrng[1]:yrrng[2])) + 
        ggplot2::scale_x_discrete(expand = c(0, 0), position = "top") + 
        ggplot2::theme_bw() + 
        ggplot2::theme(axis.title = ggplot2::element_blank(), 
                       legend.position = "none", panel.grid = ggplot2::element_blank())
    if (!is.null(txtsz)) 
        p <- p + ggplot2::geom_text(data = subset(toplo, !is.na(cat)), 
                                    ggplot2::aes(label = cat), size = txtsz, family = family)
    if (plotly) 
        p <- show_matrixplotly(p, family = family, width = width, 
                               height = height)
    return(p)
}
