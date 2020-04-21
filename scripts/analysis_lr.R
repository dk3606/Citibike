require(tidyverse)
require(doParallel)
require(foreach)
require(tm)
require(lubridate)
require(stringr)

factor_variables <- c("station", "weekday", "month", "holiday", 
                      "trace.rain", "trace.snow", "station")
citi17 <- readr::read_csv("data/citibike_2017.csv")
citi18 <- readr::read_csv("data/citibike_2018.csv")

citi17 <- citi17 %>% 
  mutate_at(factor_variables, as.factor)

citi18 <- citi18 %>% 
  mutate_at(factor_variables, as.factor)

weekday_17 <- citi17 %>% group_by(weekday) %>% summarize(frequency = sum(outcome))
weekday_17$weekday <- ordered(weekday_17$weekday, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                                     "Thursday","Friday", "Saturday"))
weekday_17 %>% ggplot() + 
  geom_bar(stat = "identity", aes(x=weekday, y=frequency)) +
  labs(x = "Day of the Week", y= "Frequency") + theme_bw()

