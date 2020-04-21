require(tidyverse)
require(doParallel)
require(foreach)
require(tm)
require(lubridate)
require(stringr)
require(ranger)
require(caret)
require(glmnet)
require(ROCR)

#####  Random Forest Models

### Read in data 
citi17 <- readr::read_csv("data/citibike_2017.csv")
citi18 <- readr::read_csv("data/citibike_2018.csv")

citi17 <- citi17 %>% rename(Precipitation = Precopitation)%>% 
  select(-start_day, -end_day, -date, -percent)  %>% 
  mutate(month = as.factor(month),
         holiday = as.factor(holiday),
         weekday = as.factor(weekday),
         trace.snow = as.factor(trace.snow),
         trace.rain = as.factor(trace.rain))

citi18 <- citi18 %>% rename(Precipitation = Precopitation) %>% 
  select(-start_day, -end_day, -date, -percent)  %>% 
  mutate(month = as.factor(month),
         holiday = as.factor(holiday),
         weekday = as.factor(weekday),
         trace.snow = as.factor(trace.snow),
         trace.rain = as.factor(trace.rain)) %>% 
  filter(station %in% citi17$station)

#### Set training and validation for crossvalidation
set.seed(2020)

sample_size <- floor(0.75*nrow(citi17))
end_split <- nrow(citi17)

citi17 <- citi17[sample(1:end_split),]
training <- citi17[1:sample_size, ]
validation <- citi17[(sample_size+1):end_split,]
testing_set <- validation
#### Tuning parameters   
### mtry = 1
rfmodel0 <- ranger(factor(outcome) ~ ., data=training, num.trees = 1000, 
                   mtry = 1, splitrule = "gini", respect.unordered.factors = TRUE,
                   probability = TRUE)

test.rf <- test.rf %>% 
  mutate(mtry1.prob = predict(rfmodel0, testing_set, type = 'response')$predictions[,2])

pred.rf <- prediction(test.rf$mtry1.prob, test.rf$outcome)
perf.rf <- performance(pred.rf,'auc')
mtry1_AUC <- perf.rf@y.values[[1]]
cat('The AUC of random forest model is:', mtry1_AUC) #0.5021396

### mtry = 2 ### WINNER 
rfmodel1 <- ranger(factor(outcome) ~ ., data=training, num.trees = 1000, 
                     mtry = 2, splitrule = "gini", respect.unordered.factors = TRUE,
                   probability = TRUE, importance = "impurity")

test.rf <- testing_set %>% 
  mutate(mtry2.prob = predict(rfmodel1, testing_set, type = 'response')$predictions[,2])

pred.rf <- prediction(test.rf$mtry2.prob, test.rf$outcome)
perf.rf <- performance(pred.rf,'auc')
mtry2_AUC <- perf.rf@y.values[[1]]
cat('The AUC of random forest model is:', mtry2_AUC) #0.720035

### mtry = 3
rfmodel2 <- ranger(factor(outcome) ~ ., data=training, num.trees = 1000, 
                   mtry = 3, splitrule = "gini", respect.unordered.factors = TRUE,
                   probability = TRUE)

test.rf <- test.rf %>% 
  mutate(mtry3.prob = predict(rfmodel2, testing_set, type = 'response')$predictions[,2])

pred.rf <- prediction(test.rf$mtry3.prob, test.rf$outcome)
perf.rf <- performance(pred.rf,'auc')
mtry3_AUC <- perf.rf@y.values[[1]]
cat('The AUC of random forest model is:', mtry3_AUC) #0.716463

### mtry = 4
rfmodel3 <- ranger(factor(outcome) ~ ., data=training, num.trees = 1000, 
                   mtry = 4, splitrule = "gini", respect.unordered.factors = TRUE,
                   probability = TRUE)

test.rf <- test.rf %>% 
  mutate(mtry4.prob = predict(rfmodel3, testing_set, type = 'response')$predictions[,2])

pred.rf <- prediction(test.rf$mtry4.prob, test.rf$outcome)
perf.rf <- performance(pred.rf,'auc')
mtry4_AUC <- perf.rf@y.values[[1]]
cat('The AUC of random forest model is:', mtry4_AUC) #0.7109716

### mtry = 5
rfmodel4 <- ranger(factor(outcome) ~ ., data=training, num.trees = 1000, 
                   mtry = 5, splitrule = "gini", respect.unordered.factors = TRUE,
                   probability = TRUE)

test.rf <- test.rf %>% 
  mutate(mtry5.prob = predict(rfmodel4, testing_set, type = 'response')$predictions[,2])

pred.rf <- prediction(test.rf$mtry5.prob, test.rf$outcome)
perf.rf <- performance(pred.rf,'auc')
mtry5_AUC <- perf.rf@y.values[[1]]
cat('The AUC of random forest model is:', mtry5_AUC) #0.7081588


rf_aucs <- data.frame(mtry = 1:5, AUC = c(mtry1_AUC, mtry2_AUC, mtry3_AUC, mtry4_AUC, mtry5_AUC))

ggplot(data = rf_aucs, aes(x = mtry, y = AUC)) + geom_line() + theme_bw() + 
  labs(x = "mtry", y = "AUC")

###### 
rfmodel_opt <- ranger(factor(outcome) ~ ., data=training, num.trees = 1000, 
                   mtry = 2, splitrule = "gini", respect.unordered.factors = TRUE,
                   probability = TRUE, importance = "impurity")

test.rf <- test.rf %>% 
  mutate(opt.prob = predict(rfmodel_opt, testing_set, type = 'response')$predictions[,2])

pred.rf <- prediction(test.rf$opt.prob, test.rf$outcome)
opt_AUC <- perf.rf@y.values[[1]]
cat('The AUC of random forest model is:', opt_AUC) # 0.7081588

important.variables <- data.frame(importance = rfmodel_opt$variable.importance)
important.variables$predictors <- rownames(important.variables)
important.variables <- important.variables %>% arrange(desc(importance))
important.variables$predictors <- tolower(important.variables$predictors)

ggplot(data = important.variables, aes(x = predictors, y = importance)) + 
  labs(x = "Predictors", y = "Importance") + 
  geom_bar(stat = "identity") + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### FINAL RANDOM FOREST MODEL
rfmodel_final <-  ranger(factor(outcome) ~ ., data=citi17, num.trees = 1000, 
                         mtry = 2, splitrule = "gini", respect.unordered.factors = TRUE,
                         probability = TRUE, importance = "impurity")

citi18 <- citi18[complete.cases(citi18),]
citi18 <- citi18 %>% 
  mutate(pred.prob = predict(rfmodel_final, citi18, type = 'response')$predictions[,2])

preds <-  predict(rfmodel_final, citi18, type = 'response')$predictions
citi18$preds <- ifelse(preds[,2] > 0.5 , 1,0)
    
pred.rf <- prediction(citi18$pred.prob, citi18$outcome)
perf.rf <- performance(pred.rf,'auc')
final_AUC <- perf.rf@y.values[[1]]
cat('The AUC of random forest model is:', final_AUC) #0.7123533

stations_latlong <- read_csv("data/latlong_stations.csv")
names(stations_latlong) <- c("station", "lat", "long")


station_rebalance <- citi18 %>% group_by(station) %>% 
  summarize(proportion_rebalance = mean(preds)) %>% 
  arrange(desc(proportion_rebalance)) %>% 
  left_join(stations_latlong, by = "station")

test <- data.frame(pred.prob = as.numeric(citi18$pred.prob), 
                   station= as.character(citi18$station))
test <- test %>%
  filter(pred.prob >= 0.5) %>% 
  group_by(station) %>%
  summarize(count = n())
sum(test$count >=30)

library(leaflet)
station_rebalance %>% 
  filter(station %in% test$station) %>% 
  leaflet() %>% 
  setView(lng = -73.98, lat = 40.75, zoom = 12) %>%
  addTiles() %>%
  addCircleMarkers(lat = ~ lat, lng = ~ long, weight = 2, fillOpacity = 0.3, 
                   radius = ~(1 + 15*proportion_rebalance))





# pred_rf_station <- data.frame(predicted = as.numeric(citi18$preds),
#                                  station = as.character(citi18$station))
# pred_rf_station <- pred_rf_station %>%
#   group_by(station) %>%
#   summarize(count = n(),
#             proportion_rebalance = mean(predicted)) %>%
#   arrange(desc(count))
# 
# sum(pred_rf_station$count >= 30)
# 
# lat_long_stations <- read_csv("data/latlong_stations.csv")
# colnames(lat_long_stations) <- c("station", "latitude", "longitude")
# pred_rf_station <- pred_rf_station %>%
#   filter(count >= 30) %>%
#   left_join(lat_long_stations, by = "station")
#   
# 
# pred_rf_station %>% leaflet() %>% 
#   setView(lng = -73.98, lat = 40.75, zoom = 11) %>%
#   addTiles() %>%
#   addCircleMarkers(lat = ~ latitude, lng = ~ longitude, weight = 2, fillOpacity = 0.3, radius = ~(1 + 15*proportion_rebalance))
