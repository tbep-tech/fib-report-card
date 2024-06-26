#' Identify Fecal Indicator Bacteria samples as coming from a 'wet' or 'dry' time period  
#'
#' @param fibdata input data frame
#' @param precipdata input data frame as returned by \code{\link{read_importrain}}. columns should be: station, date (yyyy-mm-dd), rain (in inches).  
#' @param temporal_window numeric, number of days precipitation should be summed over (1 = day of sample only; 2 = day of sample + day before; etc.)
#' @param wet_threshold  numeric, inches accumulated through the defined temporal window, above which a sample should be defined as being from a 'wet' time period
#'
#' @return
#' 
#' @details
#' 
#' @export
#'
#' @examples
anlz_fibwetdry <- function(fibdata,
                           precipdata,
                           temporal_window = 1,
                           wet_threshold = 0.5){
    
    # in precipdata, calculate precip in the temporal window
    
    # want all the lags up to [temporal_window - 1]  (e.g. for 2-day temporal window, want rain + lag1(rain); for 3-day window want rain + lag1 + lag2),
    # so build the formula for that. if temporal window = 1, just use "rain" as the formula.
    
    if(temporal_window > 1){
        lag_formula = ""
        for(i in 2:temporal_window){
            lag_formula <- paste0(lag_formula, "+ lag(rain, ", i-1, ")")
        }
        formula_string = paste("rain", lag_formula)
    } else {
        formula_string = "rain"
    }
    
    formula_obj = parse(text = formula_string)
    
    
    # now calculate
    prcp_calcd <- precipdata |> 
        group_by(station) |> 
        mutate(rain_total = eval(formula_obj)) |> 
        rename(rain_sampleDay = rain) |> 
        ungroup()
    
    
    # left join, fibdata = left, prcipdata = right; on station and date
    # use threshold to show wet or dry
    out <- left_join(fibdata, prcp_calcd,
                     by = c("station", "date")) |> 
        mutate(wet_sample = rain_total >= wet_threshold)
    
    return(out)
}