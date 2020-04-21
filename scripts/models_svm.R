require(tidyverse)
require(class)
require(e1071)
require(plot3D)
require(ROCR)

set.seed(2020)

rm(list = ls())

citi17 <- read.csv("data/citibike_2017.csv")
citi18 <- read.csv("data/citibike_2018.csv")

## Training & Validation Set (Citibike 2017 data)
citi17 <- citi17 %>% 
  select(outcome, station, radius_count, weekday, month, holiday, Max.Temp, Min.Temp, 
         Avg.Temp, Precopitation, New.Snow, Snow.Depth, trace.snow, trace.rain) %>%
  mutate(month = as.factor(month),
         holiday = as.factor(holiday),
         trace.snow = as.factor(trace.snow),
         trace.rain = as.factor(trace.rain))

x <- citi17[sample(nrow(citi17)),]

citi17_train <- x %>% slice(1:(0.75*nrow(x)))
citi17_test <- x[(0.75*nrow(x)+1):nrow(x),]

rm(x)

## Test Set (Citibike 2018 data)
station17 <- as.character(unique(citi17$station))
station18 <- as.character(unique(citi18$station))
station_remove <- station18[-match(station17,station18)]

oos <- citi18 %>% 
  select(outcome, station, capacity, radius_count, weekday, month, holiday, Max.Temp, Min.Temp, 
         Avg.Temp, Precopitation, New.Snow, Snow.Depth, trace.snow, trace.rain) %>%
  mutate(month = as.factor(month),
         holiday = as.factor(holiday),
         trace.snow = as.factor(trace.snow),
         trace.rain = as.factor(trace.rain)) %>%
  filter(!station %in% station_remove)

oos$station <- as.character(oos$station)
oos$station <- as.factor(oos$station)
oos <- oos[complete.cases(oos),]

# RUNNING SVM

start.time <- Sys.time()
fitSVM1 <- svm(outcome ~ ., data = citi17_train,kernel = "radial")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# PREDICTIONS 2017

predictions17 <- predict(fitSVM1,citi17_test,probability = T)

pred_ROCR <- prediction(predictions17,citi17_test$outcome)
roc_ROCR <- performance(pred_ROCR, measure = "tpr", x.measure = "fpr")
plot(roc_ROCR, main = "ROC curve")
abline(a = 0, b = 1)

auc_ROCR17 <- performance(pred_ROCR, measure = "auc")
auc_ROCR17 <- auc_ROCR17@y.values[[1]]
# 0.6823675

# PREDICTIONS 2018

predictions18 <- predict(fitSVM1,oos,probability = T)

pred_ROCR <- prediction(predictions18,oos$outcome)
roc_ROCR <- performance(pred_ROCR, measure = "tpr", x.measure = "fpr")
plot(roc_ROCR, main = "ROC curve")
abline(a = 0, b = 1)

auc_ROCR18 <- performance(pred_ROCR, measure = "auc")
auc_ROCR18 <- auc_ROCR18@y.values[[1]]
# 0.6096035

pred_stations <- data.frame(pred = predictions18,station = oos$station)
pred_stations$pred <- ifelse(pred_stations$pred > 0.5,1,0)
pred_stations <- pred_stations %>% group_by(station) %>% summarise(times = sum(pred))

sum(pred_stations$times > 30)





