## ---------------------------
##
## Script name: hycom_functions.R
##
## Purpose of script: Functions for downloading HYCOM temperature data
##
## Author: Kevin Siwicke
##
## Date Created: 2021-03-25
##
## Email: kevin.siwicke@noaa.gov
##
## ---------------------------
##
## Notes: There are multiple versions of HYCOM output, and this will have two
##   scripts for retrieving data, 'get.hycom.U' (GLBu0.08) and 'get.hycom.V' (GLBv0.08).
##   The 'v' output has a 'water_temp_bottom' variable, while both 'u' and 'v' have 
##   a 'water_temp' variable for modeled temp at specific depths. The most recent 
##   version in both of these is GLBy0.08, which does have 'water_temp_bottom'. There 
##   is a specific function for getting bottom temperature: 'get.hycom.bot' as well.
##
## ---------------------------
## load up the packages we will need:  (uncomment as required)

library(curl)

#' @param limits A list of length 4; minlon, maxlon, minlat, maxlat. Longitude values are -180,180
#' @param time A vector of length 2 with the minimum and maximum times in form
#'   \code{as.Date(date)}.
#' @param vars A list of variables to download. This can contain
#'   'water_temp', 'water_u', 'water_v', 'salinity' or 'surf_el' but is not checked
#'   for errors.
#' @param include_latlon Should the array of latitude and longitude values be
#'   included?
#' @param filename An optional filename. If provided, then the data is
#'   downloaded to that file. Otherwise the data is not downloaded and the url
#'   is returned.
#' @param download.file Logical. Should use the default \code{download.file} to
#'   query the server and download or use the optional \code{curl}. Some users
#'   may need to use \code{curl} in order to get this to work.
#' @param dir is local directory where ncdf files should be downloaded to.
#'   default is current working directory. if enter a directory that doesn't
#'   exist, it will be created.
#' @param depLevels is an integer describing which depth levels to download from Hycom (e.g. 1=surface). Default is NULL and all levels are downloaded.
#' @return The url used to extract the requested data from the NetCDF subset
#'   service.
#' @importFrom curl curl_download
#'
#' @author   Function originally written for R by Ben Jones (WHOI) and modified
#'   by Camrin Braun, Ben Galuardi, Julie Nielsen, and Kevin Siwicke.
#' @references \url{https://hycom.org/}
#'

# Function for retrieving HYCOM modelled bottom temperature, given inputs lat/lon/time
# from GLBv0.08 which goes from Jan 1, 1994 to present
get_hycom_bot <- function(limits, time, vars=c("water_temp_bottom"), include_latlon=TRUE, depLevels=NULL) {
  
  ## Set the base URL based on the start date. If the ending date exceeds the
  ## period for this experiment, then print a warning and truncate the output
  ## early.
  
  expts = data.frame(
    start=c(as.Date('1994-01-01'), as.Date('2015-12-31'),
            as.Date('2016-05-01'), as.Date('2017-02-01'),
            as.Date('2017-06-01'), as.Date('2017-10-01'),
            as.Date('2018-01-01'), as.Date('2020-02-19')),
    end=c(as.Date('2015-12-30'), as.Date('2016-04-30'), 
          as.Date('2017-01-31'), as.Date('2017-05-31'),
          as.Date('2017-09-30'), as.Date('2017-12-31'),
          as.Date('2020-02-18'), Sys.Date() + 1),
    url=c(paste0('https://ncss.hycom.org/thredds/ncss/GLBv0.08/expt_53.X/data/', year(time),'?'),
          'https://ncss.hycom.org/thredds/ncss/GLBv0.08/expt_56.3?',
          'https://ncss.hycom.org/thredds/ncss/GLBv0.08/expt_57.2?',
          'https://ncss.hycom.org/thredds/ncss/GLBv0.08/expt_92.8/ts3z?',
          'https://ncss.hycom.org/thredds/ncss/GLBv0.08/expt_57.7?',
          'https://ncss.hycom.org/thredds/ncss/GLBv0.08/expt_92.9/ts3z?',
          'https://ncss.hycom.org/thredds/ncss/GLBv0.08/expt_93.0/ts3z?',
          'https://ncss.hycom.org/thredds/ncss/GLBy0.08/expt_93.0?'))
  
  if(time[1] < expts$start[1])
    stop('Data begins at %s and is not available at %s.',
         strftime(expts$start[1], '%d %b %Y'),
         strftime(time[1], '%d %b %Y'))
  if(time[1] > expts$end[nrow(expts)])
    stop('Data ends at %s and is not available at %s.',
         strftime(expts$end[nrow(expts)], '%d %b %Y'),
         strftime(time[1], '%d %b %Y'))
  for(i in seq(nrow(expts))) {
    if((time[1] >= expts$start[i]) & (time[1] <= expts$end[i]))
      url = expts$url[i]
  }
  
  ## Add the variables.
  for(var in vars)
    url = sprintf('%svar=%s&', url, var)
  ## Add the spatial domain.
  url = sprintf('%snorth=%f&west=%f&east=%f&south=%f&horizStride=1&',
                url, limits[[4]], limits[[1]], limits[[2]], limits[[3]])
  # north, west, east, south
  
  ## Add the time domain.
  if(length(time) == 2){
    url = sprintf('%stime_start=%s%%3A00%%3A00Z&time_end=%s%%3A00%%3A00Z&',
                  url, strftime(time[1], '%Y-%m-%dT18'),
                  strftime(time[2], '%Y-%m-%dT18'))
  } else if(length(time) == 1){
    url = sprintf('%stime_start=%s%%3A00%%3A00Z&time_end=%s%%3A00%%3A00Z&',
                  url, strftime(time[1], '%Y-%m-%dT18'),
                  strftime(time[1], '%Y-%m-%dT18'))
  }
  ## Check for the newer HYCOM experiments (3hr time resolution) and add stride=8 if needed, otherwise 1 for daily HYCOM data
  if(any(grep('GLBy', url))){
    url = sprintf('%stimeStride=%s&', url, 8)
  } else{
    url = sprintf('%stimeStride=%s&', url, 1)
  }
  
  ## Add the lat-lon points if requested.
  if(include_latlon)
    url = sprintf('%saddLatLon=true&', url)
  ## Finish the URL.
  if (is.null(depLevels)){
    url = sprintf('%sdisableProjSubset=on&vertCoord=&accept=netcdf', url)
  } else{
    url = paste(url,'disableProjSubset=on&vertCoord=', depLevels, '&accept=netcdf', sep='')
  }
  
  tmp <- tempfile(fileext = ".nc")
  download.file(url, method = "curl", tmp)
  bot <- tidync::tidync(tmp) |> tidync::hyper_tibble() |>
    dplyr::rename(water_temp = water_temp_bottom) |> 
    dplyr::mutate(lon = as.numeric(lon),
                  lon = ifelse(lon > 180, lon - 360, lon),
                  lat = as.numeric(lat),
                  date = time[1], is.bot = 1)
}

# First, get temperature/depth profiles at specific HYCOM depths from GLBv0.08
# given inputs lat/lon/time, which goes from Jan 1, 1994 to present.
get_hycom_V <- function(limits, time, vars=c('water_temp'), include_latlon=TRUE, depLevels=NULL) {

    ## Set the base URL based on the start date. If the ending date exceeds the
  ## period for this experiment, then print a warning and truncate the output
  ## early.
  
  expts = data.frame(
    start=c(as.Date('1994-01-01'), as.Date('2015-12-31'),
            as.Date('2016-05-01'), as.Date('2017-02-01'),
            as.Date('2017-06-01'), as.Date('2017-10-01'),
            as.Date('2018-01-01'), as.Date('2020-02-19')),
    end=c(as.Date('2015-12-30'), as.Date('2016-04-30'), 
          as.Date('2017-01-31'), as.Date('2017-05-31'),
          as.Date('2017-09-30'), as.Date('2017-12-31'),
          as.Date('2020-02-18'), Sys.Date() + 1),
    url=c(paste0('https://ncss.hycom.org/thredds/ncss/GLBv0.08/expt_53.X/data/', year(time),'?'),
          'https://ncss.hycom.org/thredds/ncss/GLBv0.08/expt_56.3?',
          'https://ncss.hycom.org/thredds/ncss/GLBv0.08/expt_57.2?',
          'https://ncss.hycom.org/thredds/ncss/GLBv0.08/expt_92.8/ts3z?',
          'https://ncss.hycom.org/thredds/ncss/GLBv0.08/expt_57.7?',
          'https://ncss.hycom.org/thredds/ncss/GLBv0.08/expt_92.9/ts3z?',
          'https://ncss.hycom.org/thredds/ncss/GLBv0.08/expt_93.0/ts3z?',
          'https://ncss.hycom.org/thredds/ncss/GLBy0.08/expt_93.0?'))
  
  if(time[1] < expts$start[1])
    stop('Data begins at %s and is not available at %s.',
         strftime(expts$start[1], '%d %b %Y'),
         strftime(time[1], '%d %b %Y'))
  if(time[1] > expts$end[nrow(expts)])
    stop('Data ends at %s and is not available at %s.',
         strftime(expts$end[nrow(expts)], '%d %b %Y'),
         strftime(time[1], '%d %b %Y'))
  for(i in seq(nrow(expts))) {
    if((time[1] >= expts$start[i]) & (time[1] <= expts$end[i]))
      url = expts$url[i]
  }
  
  ## Add the variables.
  for(var in vars)
    url = sprintf('%svar=%s&', url, var)
  ## Add the spatial domain.
  url = sprintf('%snorth=%f&west=%f&east=%f&south=%f&horizStride=1&',
                url, limits[[4]], limits[[1]], limits[[2]], limits[[3]])
  # north, west, east, south
  
  ## Add the time domain.
  if(length(time) == 2){
    url = sprintf('%stime_start=%s%%3A00%%3A00Z&time_end=%s%%3A00%%3A00Z&',
                  url, strftime(time[1], '%Y-%m-%dT18'),
                  strftime(time[2], '%Y-%m-%dT18'))
  } else if(length(time) == 1){
    url = sprintf('%stime_start=%s%%3A00%%3A00Z&time_end=%s%%3A00%%3A00Z&',
                  url, strftime(time[1], '%Y-%m-%dT18'),
                  strftime(time[1], '%Y-%m-%dT18'))
  }
  ## Check for the newer HYCOM experiments (3hr time resolution) and add stride=8 if needed, otherwise 1 for daily HYCOM data
  if(any(grep('GLBy', url))){
    url = sprintf('%stimeStride=%s&', url, 8)
  } else{
    url = sprintf('%stimeStride=%s&', url, 1)
  }
  
  ## Add the lat-lon points if requested.
  if(include_latlon)
    url = sprintf('%saddLatLon=true&', url)
  ## Finish the URL.
  if (is.null(depLevels)){
    url = sprintf('%sdisableProjSubset=on&vertCoord=&accept=netcdf', url)
  } else{
    url = paste(url,'disableProjSubset=on&vertCoord=', depLevels, '&accept=netcdf', sep='')
  }
  
  tmp <- tempfile(fileext = ".nc")
  download.file(url, method = "curl", tmp)
  wc <- tidync::tidync(tmp) |> tidync::hyper_tibble() |>
    dplyr::mutate(lon = as.numeric(lon),
                  lon = ifelse(lon > 180, lon - 360, lon),
                  lat = as.numeric(lat),
                  depth = as.numeric(depth), date = time[1], is.bot = 0)
}

# # Alternatively, get temperature/depth profiles at specific HYCOM depths from GLBu0.08
# # given inputs lat/lon/time, which goes from Oct 2, 1992 to present.
# get.hycom.U <- function(limits, time, vars=c('water_temp'), include_latlon=TRUE,
#                         filename='', download.file=TRUE, dir = getwd(), depLevels=NULL) {
#   
#   dir.create(file.path(dir), recursive = TRUE, showWarnings = FALSE)
#   setwd(dir)
#   
#   ## Set the base URL based on the start date. If the ending date exceeds the
#   ## period for this experiment, then print a warning and truncate the output
#   ## early.
#   
#   expts = data.frame(
#     start=c(as.Date('1992-10-02'), as.Date('1995-08-01'),
#             as.Date('2013-01-01'), as.Date('2013-08-21'),
#             as.Date('2014-04-05'), as.Date('2016-04-18'),
#             as.Date('2018-12-04')),
#     end=c(as.Date('1995-07-31'), as.Date('2012-12-31'),
#           as.Date('2013-08-20'), as.Date('2014-04-04'),
#           as.Date('2016-04-17'), as.Date('2018-11-20'),
#           Sys.Date() + 1),
#     url=c('https://ncss.hycom.org/thredds/ncss/GLBu0.08/expt_19.0/',
#           'https://ncss.hycom.org/thredds/ncss/GLBu0.08/expt_19.1/',
#           'https://ncss.hycom.org/thredds/ncss/GLBu0.08/expt_90.9?',
#           'https://ncss.hycom.org/thredds/ncss/GLBu0.08/expt_91.0?',
#           'https://ncss.hycom.org/thredds/ncss/GLBu0.08/expt_91.1?',
#           'https://ncss.hycom.org/thredds/ncss/GLBu0.08/expt_91.2?',
#           'https://ncss.hycom.org/thredds/ncss/GLBy0.08/expt_93.0?'))
#   
#   if(time[1] < expts$start[1])
#     stop('Data begins at %s and is not available at %s.',
#          strftime(expts$start[1], '%d %b %Y'),
#          strftime(time[1], '%d %b %Y'))
#   if(time[1] > expts$end[nrow(expts)])
#     stop('Data ends at %s and is not available at %s.',
#          strftime(expts$end[nrow(expts)], '%d %b %Y'),
#          strftime(time[1], '%d %b %Y'))
#   for(i in seq(nrow(expts))) {
#     if((time[1] >= expts$start[i]) & (time[1] <= expts$end[i]))
#       url = expts$url[i]
#   }
#   
#   if(any(grep('19', url))) url = sprintf('%s%s?', url, as.numeric(format(time, '%Y')))
#   
#   ## Add the variables.
#   for(var in vars)
#     url = sprintf('%svar=%s&', url, var)
#   ## Add the spatial domain.
#   url = sprintf('%snorth=%f&west=%f&east=%f&south=%f&horizStride=1&',
#                 url, limits[[4]], limits[[1]], limits[[2]], limits[[3]])
#   # north, west, east, south
#   
#   ## Add the time domain.
#   if(length(time) == 2){
#     url = sprintf('%stime_start=%s%%3A00%%3A00Z&time_end=%s%%3A00%%3A00Z&',
#                   url, strftime(time[1], '%Y-%m-%dT18'),
#                   strftime(time[2], '%Y-%m-%dT18'))
#   } else if(length(time) == 1){
#     url = sprintf('%stime_start=%s%%3A00%%3A00Z&time_end=%s%%3A00%%3A00Z&',
#                   url, strftime(time[1], '%Y-%m-%dT18'),
#                   strftime(time[1], '%Y-%m-%dT18'))
#   }
#   ## Check for the newer HYCOM experiments (3hr time resolution) and add stride=8 if needed, otherwise 1 for daily HYCOM data
#   if(any(grep('GLBy', url))){
#     url = sprintf('%stimeStride=%s&', url, 8)
#   } else{
#     url = sprintf('%stimeStride=%s&', url, 1)
#   }
#   
#   ## Add the lat-lon points if requested.
#   if(include_latlon)
#     url = sprintf('%saddLatLon=true&', url)
#   ## Finish the URL.
#   if (is.null(depLevels)){
#     url = sprintf('%sdisableProjSubset=on&vertCoord=&accept=netcdf', url)
#   } else{
#     url = paste(url,'disableProjSubset=on&vertCoord=', depLevels, '&accept=netcdf', sep='')
#   }
#   # 
#   # print(url)
#   # 
#   # ## Download the data if a filename was provided.
#   # if(filename != ''){
#   #   if(download.file == TRUE){
#   #     #download.file(url, filename, method = 'auto')
#   #     curl_download(url, filename, quiet=FALSE)
#   #   } else if(download.file == FALSE){
#   #     system(sprintf('curl -o "%s" "%s"', filename, url))
#   #   }
#   # }
#   # return(url)
# }


# Get HYCOM bathymetry for various uses
# The bathymetry doesn't change north or south 40 degrees, 
# but there may be changes through time otherwise
get_hycom_bathy <- function(limits) {
  
  url <- "https://ncss.hycom.org/thredds/ncss/datasets/GLBy0.08/expt_93.0/topo/depth_GLBy0.08_09m11.nc?var=bathymetry&"
 
  ## Add the spatial domain.
  url = sprintf('%snorth=%f&west=%f&east=%f&south=%f&',
                url, limits[[4]], limits[[1]], limits[[2]], limits[[3]])
  # north, west, east, south
  
  url <- paste(url, 'disableProjSubset=on&horizStride=1&time=2018-01-01T00%3A00%3A00Z&addLatLon=true&accept=netcdf4', sep = '')

  temp_nc <- tempfile(fileext = ".nc")
  download.file(url, method = "curl", temp_nc)
  new <- tidync::tidync(temp_nc) %>% tidync::hyper_tibble() |>
    dplyr::mutate(lon = as.numeric(Longitude),
                  lon = ifelse(lon > 180, lon - 360, lon),
                  lat = as.numeric(Latitude),
                  depth = bathymetry - 10) |> 
    dplyr::select(lon, lat, depth)
}

get_hycom_temp <- function(date_begin = as.Date("2020-06-01"), 
                           date_end = as.Date("2020-06-02"), 
                           lat_min = 50, lat_max = 50, 
                           lon_min = -160, lon_max = -160,
                           z_min = 0, z_max = 7000,
                           include_wc = TRUE, # make true if you want water column temperature profiles
                           include_bot = TRUE) { # make true if you want bottom (10m above) temperatures
  
  if(include_wc == FALSE & include_bot == FALSE) {
    print("Error: No water column or bottom data is included.")
    return(NULL)
  }
  
  days <- seq(date_begin, date_end, by="days")
  
  lim <- c(lon_min - 0.02, lon_max + 0.02, lat_min - 0.02, lat_max + 0.02)
  
  # If dateline is crossed, you'll need to separate sets of longitude limits
  if(lon_min > 0 & lon_max < 0) {
    print('Your longitude range crosses the dateline.')
    lim_w <- c(lon_min - 0.02, 180.000000, lat_min - 0.02, lat_max + 0.02)
    lim <- c(-180.000000, lon_max + 0.02, lat_min - 0.02, lat_max + 0.02)
  }
  
  # Pre-allocate lists
  if(include_wc == TRUE) {
    wc_list <- list()
  }
  
  if(include_bot == TRUE) {
    bot_list <- list()
  } 
  
  
  for(i in 1:length(days)) {
    
    if(lon_min > 0 & lon_max < 0) {
      
      if(include_wc == TRUE) {
        wc_w <- get_hycom_V(limits = lim_w, time = days[i])
      }
      
      if(include_bot == TRUE) {
        bot_w <- get_hycom_bot(limits = lim_w, time = days[i])
      } 
    }
    
    if(include_wc == TRUE) {
      wc <- get_hycom_V(limits = lim, time = days[i])
      if(lat_min == lat_max & lon_min == lon_max) {
        wc <- wc |> 
          dplyr::mutate(delta_lat = abs(lat - lat_min),
                        delta_lon = abs(lon - lon_min)) |> 
          dplyr::filter(delta_lat == min(delta_lat)) |>  
          dplyr::filter(delta_lon == min(delta_lon)) |> 
          dplyr::select(!c(delta_lat, delta_lon))
      }
    }
    
    if(include_bot == TRUE) {
      bot <- get_hycom_bot(limits = lim, time = days[i])
      if(lat_min == lat_max & lon_min == lon_max) {
        bot <- bot |> 
          dplyr::mutate(delta_lat = abs(lat - lat_min),
                        delta_lon = abs(lon - lon_min)) |> 
          dplyr::filter(delta_lat == min(delta_lat)) |>  
          dplyr::filter(delta_lon == min(delta_lon))  |> 
          dplyr::select(!c(delta_lat, delta_lon))
      }
    }
    
    if(lon_min > 0 & lon_max < 0) {
      if(include_wc == TRUE) {
        wc <- dplyr::bind_rows(wc, wc_w) |> 
          dplyr::distinct()
      }
      if(include_bot == TRUE) {
        bot <- dplyr::bind_rows(bot, bot_w) |> 
          dplyr::distinct()
      }
    }
    if(include_wc == TRUE) {
      wc_list[[i]] <- wc
    }
    
    if(include_bot == TRUE) {
      bot_list[[i]] <- bot
    } 
    Sys.sleep(1)
  }
  wc <- data.table::rbindlist(wc_list, fill = TRUE)
  
  # get bottom depths to add to bottom temperatures
  bathy <- get_hycom_bathy(lim)
  if(lon_min > 0 & lon_max < 0) {
    bathy_w <- get_hycom_bathy(lim_w)
    bathy <- dplyr::bind_rows(bathy, bathy_w)
  }
  
  bot <- data.table::rbindlist(bot_list, fill = TRUE) |> 
    dplyr::left_join(bathy, relationship = "many-to-many") |> # multiple dates will lead to many-to-many
    dplyr::select(water_temp, lon, lat, depth, date, is.bot)
  
  hycom_data <- dplyr::bind_rows(wc, bot)
} 
