library(tidyverse)
library(sf)

# station, catchment data
catchments <- read_sf(here::here("data", "geospatial", "TB_Select_Fib_Catchments_Dissolved.shp"))

# catchment pixels, final includes full station names in long format
# this will need to be saved to tbeptools
pixels <- read_sf(here::here("data", "geospatial", "swfwmd_pixel_2_utm_m_83.shp"))
catch2 <- st_transform(catchments,
                       crs = st_crs(pixels))
catch_pixels <- st_intersection(pixels, catch2) |> 
    sf::st_drop_geometry() |> 
    select(PIXEL, StationNam) |> 
    separate_wider_delim(StationNam, delim = " + ", names = c('a','b', 'c','d'), 
                         too_few = 'align_start', cols_remove = F) |> 
    pivot_longer(cols = c(a, b, c, d), names_to = 'col', values_to = 'value') |> 
    filter(!is.na(value)) |> 
    mutate(
        station = case_when(
            StationNam == '21FLHILL_WQX-102 + 103 + 619' & col != 'a' ~ paste0('21FLHILL_WQX-', value),
            StationNam == '21FLHILL_WQX-596 + 597' & col != 'a' ~ paste0('21FLHILL_WQX-', value),
            StationNam == '21FLHILL_WQX-136 + 264' & col != 'a' ~ paste0('21FLHILL_WQX-', value),
            StationNam == '21FLHILL_WQX-112 + 180' & col != 'a' ~ paste0('21FLHILL_WQX-', value),
            StationNam == '21FLDOH_WQX-MANATEE152 + 153' & col != 'a' ~ paste0('21FLDOH_WQX-', value),
            StationNam == '21FLCOSP_WQX-NORTH CANAL + SOUTH CANAL + CENTRAL *' & col != 'a' ~ paste0('21FLCOSP_WQX-', value),
            StationNam == '21FLPDEM_WQX-39-01 + 39-05' & col != 'a' ~ paste0('21FLPDEM_WQX-', value),
            StationNam == '21FLMANA_WQX-LM3 + LM4 + LM5 + LM6' & col != 'a' ~ paste0('21FLMANA_WQX-', value),
            T ~ value
        )
    ) |> 
    select(station, pixel = PIXEL)

#' Download daily precip data and summarise by station catchment
#' 
#' @param curyr numeric for year
#' @param catch_pixels data.frame with station, pixel columns
#' @param mos numeric vector for months to download
#' @param quiet logical for messages
#' 
#' @details Data from ftp://ftp.swfwmd.state.fl.us/pub/radar_rainfall/Daily_Data/
#' @return data.frame with station, date, rain columns as a daily average (inches) for all pixels in a catchment
#' 
#' @examples
#' \dontrun{
#' read_importrain(2021, catch_pixels, quiet = F)
#' }
read_importrain <- function(curyr, catch_pixels, mos = 1:12, quiet = T){
    
    ftp <- 'ftp://ftp.swfwmd.state.fl.us/pub/radar_rainfall/Daily_Data/'
    
    # all files to dl
    mos <- sprintf('%02d', mos)
    fls <- paste0(ftp, curyr, '/', 'daily_', curyr, '_', mos, '_txt.zip')
    
    # download, extract, subset, average
    out <- NULL
    for(fl in fls){

        if(!quiet)
            cat(basename(fl), '\n')
        
        # download daily month data
        dl <- try({
            
            tmp1 <- tempfile(fileext = ".zip")
            download.file(url = fl, destfile = tmp1, method = 'curl', quiet = quiet)    
  
        }, silent = quiet)
        
        # download error
        if(inherits(dl, 'try-error')){

            unlink(tmp1, recursive = T)
    
            next()
            
        }
        
        # extract text file and import
        tmp2 <- tempdir()
        utils::unzip(tmp1, exdir = tmp2)

        txtfl <- list.files(tmp2, pattern = gsub('\\_txt.*$', '', basename(fl)), full.names = T)
        
        datall <- read.table(txtfl, sep = ',', header = F) |> 
            dplyr::rename(pixel = V1, date = V2, rain = V3)
        
        # join with grd cells, average by date, station
        dat <- dplyr::left_join(catch_pixels, datall, by = 'pixel', relationship = 'many-to-many') |> 
            dplyr::summarise(
                rain = mean(rain, na.rm = T),
                .by = c(station, date)
            )
        
        # append to output
        out <- dplyr::bind_rows(out, dat)

        # clean up
        unlink(tmp1, recursive = T)
        unlink(tmp2, recursive = T)
        
    }
    
    # date as date
    out <- out |> 
        dplyr::mutate(date = as.Date(date, format = '%m/%d/%Y')) |> 
        dplyr::arrange(station, date)
    
    return(out)

}
