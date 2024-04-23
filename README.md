# FIB Report Card generation  


## General outline  

Ideally, a single script will be created that can be run to do all of the things below, without having to think about it (beyond "this is still appropriate, right?"). Just Ctrl-A, Ctrl-Enter. But if changes need to be made, flexibility will be in place to allow for it.  

So essentially, I need to write a series of functions to do each thing below, and string them together into one script that can be re-run in the future.  

### Things we can pull together now  

1.  FIB data for selected stations through the present (end of 2023?)  
2.  Precip data for selected catchments, through the present  
3.  Processing script for precip, to go from pixels to catchment-level  
4.  Geospatial data - catchments for the selected stations  



### Things that need to be updated next year, and the next, etc.  

These are things we need to write functions for so they can be done easily  

1.  Additional (more recent) precipitation data - pull from SWFWMD's ftp site and process to catchment level  
2.  Additional (more recent) FIB data - can potentially use `tbeptools::read_importwqp()`, but might be nice to filter to only our stations of choice - by either updating `read_importwqp()`, or using something like `dataRetrieval::readWQPdata()`. (also keep an eye on the `TADA` package)  
3.  Check/updating of older FIB data - in case any updates have been made in WQP (?) (I suspect updating old data is rare, but happens)   


### Appending new to old  

Should be easy?  


### Compiling and producing scores  

This should probably just be a single function or two.  

1.  Arguments:  
    a.  stations to use/data frame to work on    
    b.  criteria/thresholds  
    c.  time period over which to calculate  
2.  Body:  
    a.  any necessary filtering
    b.  any necessary calculations  
3.  Output:  
    a.  station-level output - time series of scores  
    b.  aggregated output - counts of stations improving, deteriorating, stable? maps of any sotr? tabular summaries?  
    b.  stoplight graphic  


### Places to build in flexibility  

-  list of stations to use - may want to use additional ones in the future, or remove some of what we use now  
-  catchment for each station - will need to add catchments if stations are added; may decide someday that the catchment used here is not representative enough and change criteria  - precip processing script will need to be responsive to changes in catchment data  
-  level of aggregation of precip data? (may want to choose between previous 24, 48, or 72 hours, based on feedback) / definition for 'wet' vs. 'dry' samples 
-  scoring functions - may decide to use different criteria for determining.....  
-  do we care about detection limits??  
