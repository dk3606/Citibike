require(tidyverse)
require(doParallel)
require(foreach)
require(tm)
require(lubridate)
require(stringr)

### 1: Load Data 
filenames <- dir("data18/")
filenames <- paste0("data18/",filenames)

citi18 <- foreach(month = 1:12, .combine = 'rbind') %dopar% {
  file <- filenames[month]
  this.data <- readr::read_csv(file)
  colnames(this.data) <- removePunctuation(tolower(colnames(this.data)))
  colnames(this.data) <- str_replace_all(colnames(this.data), fixed(" "), "")
  this.data
}

stops <- read_csv("data/citibike_locations.csv")
names(stops)[1:2] <- c("station", "capacity")

rm(this.data,file,filenames,month)

### Data cleaning (remove variables, filter trips less than 45 minutes, and make/add date variables)
citi18 <- citi18 %>% select(-bikeid, -birthyear, -gender) %>% 
  filter(citi18$startstationname %in% stops$station & citi18$endstationname %in% stops$station) %>% 
  filter(tripduration < 2700) %>% 
  mutate(start_date = as.Date(starttime, format = "%m/%d/$Y"),
         start_hour = lubridate::hour(starttime),
         end_date = as.Date(stoptime, format = "%m/$d/$Y"),
         end_hour = lubridate::hour(stoptime))

### Count the number of bikes removed and returned 
station_start <- citi18 %>%
  group_by(startstationname, start_date, start_hour) %>%
  summarize(start_count = n())
names(station_start)[1:3] <- c("station", "date", "hour")

station_end <- citi18 %>%
  group_by(endstationname, end_date, end_hour) %>%
  summarize(end_count = n()) 
names(station_end)[1:3] <- c("station", "date", "hour")

# Identify the unique stations 
stations1 <- data.frame(station = citi18$startstationname, 
                        date = citi18$start_date) %>% 
  distinct()
stations2 <- data.frame(station = citi18$endstationname,
                        date = citi18$end_date) %>% 
  distinct()
stations <- rbind(stations1,stations2) %>% 
  distinct()

stations <- stations %>% crossing(hour = 0:23)
rm(stations1,stations2)

# Identifying capacity and how many bikes 
station_capacity <- stations %>% 
  left_join(station_start, by = c("station", "date", "hour")) %>% 
  left_join(station_end,by = c("station", "date", "hour")) %>% 
  mutate(start_count = replace_na(start_count, 0),
         end_count = replace_na(end_count, 0)) %>% 
  left_join(stops, by = "station") 

# Identify the number of stations closed (if they were not used for more than a week)
stations_closed <- stations %>% 
  select(station,date) %>% 
  distinct() %>% 
  group_by(station) %>%
  arrange(date, .by_group = T) %>% 
  mutate(diffs = date - lag(date)) %>%
  filter(diffs > 7) 

# Count the number of bike stations that are with 0.5km of each bike station 
lat_longs <- citi18 %>% 
  select(startstationname, startstationlatitude, startstationlongitude) %>%
  distinct()

res1 <- RANN::nn2((lat_longs[,2:3]),k = nrow(lat_longs),searchtype = "radius",
                  radius = (0.5/111))
distances <- res1$nn.dists
distances <- distances < 0.05
radius_count <- rowSums(distances) - 1

stops$radius_count <- ifelse(stops$station %in% lat_longs$startstationname,radius_count,NA)

rm(lat_longs, res1,distances, radius_count)

# Weather info from NOAA in 2018
weather <- readr::read_csv("data/weather_full.csv")
weather <- weather %>% 
  mutate(trace.snow = ifelse(Snow.Depth == "T" | New.Snow == "T", 1, 0),
         trace.rain = ifelse(Precopitation == "T", 1, 0),
         Snow.Depth = as.numeric(ifelse(Snow.Depth == "T", 0, Snow.Depth)),
         New.Snow = as.numeric(ifelse(New.Snow == "T", 0, New.Snow)),
         Precopitation = as.numeric(ifelse(Precopitation == "T", 0, Precopitation)),
         date = lubridate::date(lubridate::mdy(Date)))

# Identify holidays
Holidays18 <- date(c("2018-01-01","2018-01-15","2018-02-19","2018-05-28","2018-07-04",
                     "2018-09-03","2018-10-08","2018-11-12","2018-11-22","2018-11-23",
                     "2018-12-25"))


### Create dataset aggregated to day
##Summary by day 
stations_day <- stations[,-3]
stations_day <- unique(stations_day)

station_start_day <- station_start %>%
  ungroup() %>%
  group_by(station, date) %>%
  arrange(date, .by_group=T) %>%
  summarize(start_day = sum(start_count))

colnames(station_start_day) <- c("station","date", "start_day")

station_end_day <- station_end %>%
  ungroup() %>%
  group_by(station, date) %>%
  arrange(date, .by_group=T) %>%
  summarize(end_day = sum(end_count))

colnames(station_end_day) <- c("station","date", "end_day")

stations_day <- stations_day %>% filter(!(station %in% stations_closed))

stations_day <- left_join(stations_day, station_start_day, by = c("station","date"))
stations_day <- left_join(stations_day, station_end_day, by = c("station","date"))
stations_day <- left_join(stations_day, stops, by = "station")

stations_day$start_day <- ifelse(is.na(stations_day$start_day), 0, stations_day$start_day)
stations_day$end_day <- ifelse(is.na(stations_day$end_day),0, stations_day$end_day)
stations_day$percent <- (stations_day$end_day - stations_day$start_day)/stations_day$capacity

stations_day$outcome <- ifelse(stations_day$percent > .25 | stations_day$percent < -.25, 1, 0)
stations_day$weekday <- as.factor(weekdays(stations_day$date))
stations_day$month <-as.factor(month(stations_day$date))
stations_day$holiday <- ifelse(stations_day$date %in% Holidays18,1, 0)

full_data <- left_join(stations_day, weather, by = "date")
full_data <- full_data %>% select(-Date, -Departure, -HDD, -CDD)

write_csv(full_data, "data/citibike_2018.csv")