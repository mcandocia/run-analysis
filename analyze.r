library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(cetcolor)

#distance_smooth has been added to them
summarize_geo <- function(data){
  #time stats
  tdiffs = as.numeric(diff(data$timestamp))
  data %>% summarise(
    #time start/end
    start.time = first(timestamp),
    end.time = last(timestamp),
    
    #bounding box stats
    min.latitude = min(latitude),
    max.latitude = max(latitude),
    min.longitude = min(longitude),
    max.longitude = max(longitude),
    
    #position average stats
    mean.latitude = mean(latitude),
    mean.longitude = mean(longitude),
    weighted.mean.latitude = weighted.mean(latitude, speed),
    weighted.mean.longitude = weighted.mean(longitude, speed),
    
    #duration
    duration = sum(tdiffs[tdiffs <= 3]),
    total.duration = sum(tdiffs),
    rest.duration = total.duration - duration,
    rest.percentage = rest.duration/total.duration,
    rest.ratio = rest.duration/duration,
    
    #distance
    total.distance = sum(distance_smooth * (c(1, tdiffs) <= 3)),
    
    #speed & pace
    mean.speed = 3600 * total.distance/duration,
    mean.pace = 60/mean.speed ,
    
    #speed & pace by total duration
    mean.speed_total = 3600 * total.distance/total.duration,
    mean.pace_total = 60/mean.speed_total,
    
    #variation in speed
    sd.speed = sd(speed_smooth),
    sd.pace = sd(pace_smooth),
    source=source[1]#all values are the sam
  )
}

garmin_data = ldply(garmin_csvs, summarize_geo) %>% mutate(type='garmin')
strava_data = ldply(strava_csvs, summarize_geo) %>% mutate(type='strava')
combined_data = rbind(strava_data, garmin_data) %>% filter(!duplicated(start.time, fromLast=TRUE))

combined_data = combined_data %>% mutate(month = cut(start.time, breaks='month'),
                                         week = cut(start.time, breaks='week'))

##manual fixing
#95 hot chocolate 5k - too much gps data jumbled underground
#53 bucktown
#72 edgewater
keys = c('total.distance','mean.speed','mean.pace')
for (i in 1:3){
  combined_data[95,keys[i]] = c(3.1, 3.1/17.8*60, 17.8/3.1)[i]
  combined_data[53,keys[i]] = c(3.1, 3.1/18.48*60, 18.48/3.1)[i]
  combined_data[72, keys[i]] = c(2.99, 2.99/17.43*60, 17.43/2.99)[i]
}
###############
###COUNTERS####
###############

#get counters ready
hours = formatC(0:23, width=2, format='d', flag=0)
weekdays = c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday')

WEEKDAY_COUNTER = list()
HOUR_COUNTER = list()
WEEKHOUR_COUNTER = list()

for (day in weekdays){
  WEEKDAY_COUNTER[[day]] = character(0)
  for (hour in hours){
    WEEKHOUR_COUNTER[[paste(day, hour)]] = character(0)
  }
}
for (hour in hours){
  HOUR_COUNTER[[as.character(hour)]] = character(0)
}

update_counters <- function(data){
  if (nrow(data) < 100){
    return(NULL)
  }
  
  times = data$timestamp
  #there were no date-crossing runs that also had other runs on either day
  date = substr(times, 1, 10)[1]
  stat.weekdays =base::weekdays(times)
  stat.hours = substr(times, 12, 13)
  unique_weekdays = unique(stat.weekdays)
  unique_hours = unique(stat.hours)
  unique_weekhours = unique(paste(stat.weekdays, stat.hours))
  #apply globally
  for (key in unique_weekdays){
    WEEKDAY_COUNTER[[key]] <<- c(WEEKDAY_COUNTER[[key]], date)
  }
  for (key in unique_hours){
    HOUR_COUNTER[[key]] <<- c(HOUR_COUNTER[[key]], date)
  }
  for (key in unique_weekhours){
    WEEKHOUR_COUNTER[[key]] <<- c(WEEKHOUR_COUNTER[[key]], date)
  }
}

contract_counter <- function(counter){
  llply(counter, function(x) length(unique(x)))
}

l_ply(garmin_csvs, update_counters)
l_ply(strava_csvs, update_counters)

weekhour_frame = melt(WEEKHOUR_COUNTER %>% contract_counter()) %>% mutate(day = factor(gsub('[^A-Za-z]+', '', L1),  levels = weekdays),
                                                   hour = factor(gsub('[^0-9]+', '',L1), levels=rev(hours)))

hour_frame = melt(HOUR_COUNTER %>% contract_counter()) %>% mutate(hour = factor(L1, levels=rev(hours)))
weekday_frame = melt(WEEKDAY_COUNTER %>% contract_counter()) %>% mutate(day=factor(L1, levels=weekdays))

#plot

ggplot(weekhour_frame, aes(x=day, y=hour, fill = value)) +
  geom_tile() + scale_fill_gradientn('count', colors=cet_pal(7, 'inferno')) + 
  geom_text(aes(label=value, alpha = (0:1)[2-(value==0)]), color='#777777', size=4) +
  scale_alpha_identity() + 
  ggtitle('Number of runs during hours of week', 
          subtitle='In 2017, starting June 3rd, 2017') + 
  xlab('') 

ggplot(weekday_frame, aes(x=day, y=value)) + 
  geom_bar(stat='identity') +
  geom_text(aes(label=value), vjust='inward', size=6) + 
  ylab('count') + xlab('') + 
  ggtitle("Number of runs in 2017 by day of week",
          subtitle='In 2017, starting June 3rd, 2017') + 
  theme_bw()

ggplot(hour_frame, aes(x=hour, y=value)) + 
  geom_bar(stat='identity')  + 
  geom_text(aes(label=value, alpha=(0:1)[2-(value==0)]), vjust=0, size=5) + 
  ggtitle("Number of runs in 2017 by hour of day",
          subtitle='In 2017, starting June 3rd, 2017') + 
  scale_alpha_identity()

##############
####TRENDS####
##############

mpm_format <- function(x){
  mins = floor(x)
  secs =formatC(floor(60 * (x-mins)), width=2, format='d', flag='0')
  paste(mins, secs, sep=":")
}

ggplot(combined_data, aes(x=start.time, y=total.distance)) + 
  geom_point() + geom_line() 

ggplot(combined_data %>% group_by(week) %>% summarise(total.distance=sum(total.distance)), 
       aes(x=strftime(week, '%m-%d'), y=total.distance)) + 
  geom_bar(stat='identity') + 
  geom_text(aes(label=round(total.distance)), vjust=0, size=4.5) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=70),
        panel.border= element_blank(),
        axis.ticks.x = element_blank()) + 
  xlab('Week of Year') + 
  ylab('Weekly Distance (miles)') +
  ggtitle('Weekly Running Distance in 2017')

ggplot(combined_data %>% group_by(week) %>%
         summarise(pace=weighted.mean(mean.pace, duration),
                   total.distance=sum(total.distance)),
       aes(y=pace, x=strftime(week, '%m-%d'))) +
  geom_bar(stat='identity', 
           aes(fill=total.distance)) + 
  geom_text(aes(label=mpm_format(pace), 
                color=ifelse(total.distance < 10, 'white','black')),  
            size=4.5, angle=90, hjust=1, vjust=0.55) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=70),
        panel.border= element_blank(),
        axis.ticks.x = element_blank()) + 
  xlab('Week of Year') + 
  ylab('Pace (minutes/mile)') +
  ggtitle('Average Weekly Running Pace in 2017') + 
  scale_y_continuous(label=mpm_format) +
  scale_fill_gradientn('weekly distance', colors=cet_pal(7, 'rainbow')) + 
  scale_color_identity()

races = combined_data %>% filter(between(total.distance, 2.9, 3.25) | 1:n() == 1) %>%
  filter(mean.pace < 6 | 1:n()==1)

ggplot(races, aes(x=start.time, y=mean.pace)) + 
  geom_label(aes(label=paste(c('Uptown 5K\n(about 4 miles)','Bucktown 5K','Edgewater\n5K','Hot Chocolate\n5K','Winnetka\nTurkey\nTrot (5K)'),
                              mpm_format(mean.pace), sep='\n'))) +
  scale_y_continuous(label=mpm_format, limits = c(5.5,7.1)) + 
  xlab('Date') + ylab('Race Pace (min/mile)') + 
  ggtitle("Race Paces for 2017 5Ks") + 
  scale_x_datetime(date_breaks = '1 month')

##############
##CLUSTERING##
##############

source('cluster.r')
cluster_data = gen_model(combined_data, variables=c('total.distance',
                                                    'rest.percentage','mean.speed'),
                         n_groups=5, n_iter=50)
combined_data$cluster = cluster_data$classes
ggplot(combined_data, aes(y=weighted.mean.latitude, x=weighted.mean.longitude, color=factor(cluster))) + geom_point()
