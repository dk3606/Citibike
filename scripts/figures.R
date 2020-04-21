library(reshape2)
library(leaflet)
library(ggplot2)

# Figure 1 - Research Questions: Total Trips and Rebalanced Bikes per Month and Year

## import data and reshape for ggplot
rebalance <- read_csv("data/rebalance.csv")
rebalance$month <- factor(rebalance$month, levels = c("January", "February", "March","April","May", "June", "July", "August", "September", "October", "November", "December"))
rebalance$year <- factor(rebalance$year, levels = c("2017","2018"))
rebalance_ggplot <- melt(rebalance, id = c("year", "month"))
rebalance_ggplot <- rebalance_ggplot %>% filter(variable != "percent")

## bar chart
ggplot(rebalance_ggplot, aes(x = month, y = value)) +
  geom_bar(aes(fill = variable), stat="identity", position = "dodge") +
  facet_wrap(~ year) + 
  theme_bw() +
  theme(text = element_text(size=18), axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "Month", y = "", title = "Trip and Rebalanced Bike Counts by Month and Year", fill = "") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) + 
  scale_fill_manual("Legend", values = c("trips" = "lightblue3", "rebalanced" = "darkblue"))

# Figure 2 - Results & Discussion (Maps)

## Lasso Logistic Regression Station Map
pred_lasso_station_map  %>% leaflet() %>%
  setView(lng = -73.98, lat = 40.75, zoom = 12) %>%
  addTiles() %>%
  addCircleMarkers(lat = ~ latitude, lng = ~ longitude, weight = 2, fillOpacity = 0.3, radius = ~(1 + 15*rebalance_proportion), color = "darkgreen") 

## Random Forest Station Map
station_rebalance %>% 
  filter(station %in% test$station) %>% 
  leaflet() %>% 
  setView(lng = -73.98, lat = 40.75, zoom = 12) %>%
  addTiles() %>%
  addCircleMarkers(lat = ~ lat, lng = ~ long, weight = 2, fillOpacity = 0.3, 
                   radius = ~(1 + 15*proportion_rebalance))

