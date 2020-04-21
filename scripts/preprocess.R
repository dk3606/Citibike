source('scripts/library_citibike.R')

library(stringr)

citi_2017 <- foreach(month = 1:12, .combine = 'rbind') %dopar% {
  filenames2017  <- paste0("data/2017/2017", month,"-citibike-tripdata.csv")
  this.data <- readr::read_csv(filenames2017)
  names(this.data) <- tm::removePunctuation(tolower(names(this.data)))
  names(this.data) <- str_replace_all(names(this.data), fixed(" "), "") #removes spaces between colnames
  this.data
}
rm(this.data, filenames2017)

citi_2018 <- foreach(month = 1:12, .combine = 'rbind') %dopar% {
  filenames2018  <- paste0("data/2018/2018", month,"-citibike-tripdata.csv")
  this.data <- readr::read_csv(filenames2018)
  names(this.data) <- tm::removePunctuation(tolower(names(this.data)))
  names(this.data) <- str_replace_all(names(this.data), fixed(" "), "") #removes spaces between colnames
  this.data
}
rm(this.data, filenames2018)

stops <- readxl::read_xlsx("data/citibike_locations.xlsx")
names(stops)[1:2] <- c("station", "capacity")

citi_2017 <- citi_2017 %>% 
  filter(startstationname %in% stops$Address & endstationname %in% stops$Address) %>% 
  filter(tripduration < 2700)

citi_2018 <- citi_2018 %>% 
  filter(startstationname %in% stops$Address & endstationname %in% stops$Address) %>% 
  filter(tripduration < 2700)

#####
bikes_leave_2017 <- citi_2017 %>% 
  mutate(start.date = as.Date(starttime, format = "%m/%d/$Y"),
         start.hour = lubridate::hour(starttime)) %>% 
  group_by(startstationname, start.date, start.hour) %>% 
  summarize(bikes_left = n()) %>% 
  select(startstationname, start.date, start.hour, bikes_left)
names(bikes_leave_2017)[(1:3)] <- c("station", "date", "hour")

bikes_return_2017 <- citi_2017 %>% 
  mutate(end.date = as.Date(stoptime, format = "%m/%d/$Y"),
         end.hour = lubridate::hour(stoptime)) %>% 
  group_by(endstationname, end.date, end.hour) %>% 
  summarize(bikes_returned = n()) %>% 
  select(endstationname, end.date, end.hour, bikes_returned)
names(bikes_return_2017)[(1:3)] <- c("station", "date", "hour")

# allhours <- tidyr::crossing(unique(bikes_leave_2017$start.date), hour = c(0:23))
# names(allhours)[1] <- "date"
# 
# bikes_leave_2017 <- dplyr::left_join(allhours, bikes_leave_2017, by = c("hack_license", "pickup_hour"))

citi_2017 <- citi_2017 %>%
  mutate(start_date = as.Date(starttime, format = "%m/%d/$Y"),
         end_date = as.Date(stoptime, format = "%m/%d/$Y"))

stations1 <- data.frame(station = citi_2017$startstationname, date = citi_2017$start_date) %>%
  distinct()
stations2 <- data.frame(station = citi_2017$endstationname, date = citi_2017$end_date) %>%
  distinct()
stations <- rbind(stations1, stations2) %>% distinct()
stations <- stations %>% crossing(hour = 0:23)

station_test <- stations %>% 
  left_join(bikes_leave_2017, by = c("station", "date", "hour")) %>% 
  left_join(bikes_return_2017,by = c("station", "date", "hour")) %>% 
  left_join(stops, by = "station")

station_test <- stations %>% 
  left_join(stops, by = "station")

