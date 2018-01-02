library(dplyr)
library(KernSmooth)
library(geosphere)


DEFAULT_TZ = 'America/Chicago'

read_geo_csv <- function(x, smooth=1, degree=2, time_format = NULL, tz='', 
                         scale_factor = 1.0){
  df = read.csv(x, stringsAsFactors=FALSE)
  df$source = x 
  #fix timestamps
  if ('time' %in% names(df))
    df[,'timestamp'] = df[,'time']
  df$time = NULL
  
  if (!is.null(time_format)){
    if (time_format != '' && tz != '')
      df$timestamp = as.POSIXct(df$timestamp, format=time_format, tz=tz)
    else
      df$timestamp = as.POSIXct(df$timestamp)
    attributes(df$timestamp)$tzone <- DEFAULT_TZ
  }
  #fix lat/lon varnames if necessary
  if ('position_long' %in% names(df)){
    df$longitude = df$position_long
    df$position_long = NULL
    df$latitude = df$position_lat
    df$position_lat = NULL
  }
  #smooth lat/lon
  if (smooth > 0){
    n_entries = nrow(df)
    df = df %>% mutate(latitude_smooth=locpoly(1:n_entries, latitude, degree=degree, gridsize=n_entries, bandwidth=smooth)$y,
                       longitude_smooth=locpoly(1:n_entries, longitude, degree=degree, gridsize=n_entries, bandwidth=smooth)$y)
    #fix smoothing of areas where tdiff > 1
    tdiff = c(1, as.numeric(diff(df$time)))
    #if tdiff > 1, revert...
    df = df %>% mutate(latitude_smooth=ifelse(tdiff > 1, latitude, latitude_smooth),
                       longitude_smooth=ifelse(tdiff > 1, longitude, longitude_smooth)
                       )
  }
  else{
    df$latitude_smooth = df$latitude
    df$longitude_smooth = df$longitude
  }
  #recalculate distance/smooth distance
  df$distance = calc_distance(df)
  df$distance_smooth = calc_distance(df, c('longitude_smooth','latitude_smooth'))
  #continue as normal
  
  #scale distance to miles from km
  df$distance = df$distance/scale_factor
  
  #adjust speed
  df = df %>% adjust_speed()
  
  return(df)
}


adjust_speed <- function(data){
  tdiff = c(1, diff(data$timestamp))
  data = data %>% mutate(speed = distance*3600/tdiff, speed_smooth=pmin(22, distance_smooth*3600/tdiff), 
                         pace=tdiff/(60*distance), pace_smooth = pmax(3, tdiff/(60*distance_smooth)),
                         pace_smooth = pmin(pace_smooth, 25))
 

  return(data)
}

calc_distance <- function(data, cols = c('longitude','latitude')){
  p1 = head(data[,cols], -1) %>% as.matrix()
  p2 = tail(data[,cols], -1) %>% as.matrix()
  c(0, distVincentyEllipsoid(p1, p2)) * 1/1609 #default return is in meters, so convert to miles
}

