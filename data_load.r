library(plyr)
library(dplyr)
source('load_smooth_geo_csv.r')

#load strava and garmin files
#load csv and check for overlaps; omit strava that have overlaps with Garmin


strava_dir = '../../data/workouts/workout_gpx/strava_gpx/gpx_csv'
garmin_dir = '../../data/workouts/workout_gpx/garmin_fit'

path <- function(...) paste(..., sep='/')

strava_files = dir(strava_dir) 
strava_files = strava_files[grepl('run.*csv', strava_files)]
strava_time_format = '%Y-%m-%dT%H:%M:%SZ'

garmin_files = dir(garmin_dir)
garmin_files = garmin_files[grepl('running.*[0-9].csv', garmin_files)]
#garmin_time_format = '%Y-%m-%d %H:%M%:%S'#bugged, default '' works

strava_csvs = list()
garmin_csvs = list()

#COLUMN NAMES FOR EACH SET OF FILES
#strava:
#times | longitude | latitude | distance | elevation

#garmin:
#timestamp | position_lat | position_long | distance | enhanced_altitude | altitude | 
## enhanced_speed | speed (km/h) | heart_rate | cadence (spm) | fractional_cadence [| temperature (C)]

for (file in strava_files){
  strava_csvs[[file]] = read_geo_csv(path(strava_dir, file), smooth=4, 
                                     time_format = strava_time_format, tz='GMT')
}

for (file in garmin_files){
  garmin_csvs[[file]] = read_geo_csv(path(garmin_dir, file), smooth=4,
                                     time_format = '', 
                                     scale_factor = 1/1.609)
}
