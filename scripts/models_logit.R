library(tidyverse)
library(ROCR)
library(ranger)
library(glmnet)

# Import CitiBike 2017 & 2018 Data ###########################################################################
citi17 <- read.csv("data/citibike_2017.csv")
citi18 <- read.csv("data/citibike_2018.csv")

# Training, validation, and testing datasets ################################################################

## Training & Validation Set (Citibike 2017 data)
modeling_data <- citi17 %>% 
                 select(outcome, station, capacity, radius_count, weekday, month, holiday, Max.Temp, Min.Temp, 
                        Avg.Temp, Precopitation, New.Snow, Snow.Depth, trace.snow, trace.rain) %>%
                 mutate(outcome = as.factor(outcome),
                        month = as.factor(month),
                        holiday = as.factor(holiday),
                        trace.snow = as.factor(trace.snow),
                        trace.rain = as.factor(trace.rain))

## Shuffle Training / Validation Dataset & Split (75%, 25%)
set.seed(2020)
row_shuffle <- sample(nrow(modeling_data))
modeling_data <- modeling_data[row_shuffle,]

training_set <- modeling_data[1:(nrow(modeling_data)*.75),]
validation_set <- modeling_data[(nrow(modeling_data)*.75 + 1):nrow(modeling_data),]

## Test Set (Citibike 2018 data)
station17 <- as.character(unique(citi17$station))
station18 <- as.character(unique(citi18$station))
station_remove <- station18[-match(station17,station18)]

test_set <- citi18 %>% 
            select(outcome, station, capacity, radius_count, weekday, month, holiday, Max.Temp, Min.Temp, 
                   Avg.Temp, Precopitation, New.Snow, Snow.Depth, trace.snow, trace.rain) %>%
            mutate(outcome = as.factor(outcome),
                   month = as.factor(month),
                   holiday = as.factor(holiday),
                   trace.snow = as.factor(trace.snow),
                   trace.rain = as.factor(trace.rain)) %>%
            filter(!station %in% station_remove)

test_set$station <- as.character(test_set$station)
test_set$station <- as.factor(test_set$station)
test_set <- test_set[complete.cases(test_set),]

# Logistic Regression Model ##############################################################################

## Logistic Regression Model
model_full <- glm(outcome ~., data = training_set, family = binomial)
glm_coefs <- model_full$coefficients
glm_coefs <- data.frame(name = names(glm_coefs), coefs = as.numeric(glm_coefs), abs = abs(as.numeric(glm_coefs)))
glm_coefs <- glm_coefs %>%
             arrange(desc(abs))

## Validation Set AUC
pred_glm_valid <- predict(model_full, newdata = validation_set, type = 'response')
rocr_glm_valid <- prediction(pred_glm_valid, validation_set$outcome)
perf_glm_valid <- performance(rocr_glm_valid, "auc")
(auc_glm_valid <- perf_glm_valid@y.values[[1]])
##.7109764

## Test Set AUC
pred_glm_test <- predict(model_full, newdata = test_set, type = 'response')
rocr_glm_test <- prediction(pred_glm_test, test_set$outcome)
perf_glm_test <- performance(rocr_glm_test, "auc")
(auc_glm_test <- perf_glm_test@y.values[[1]])
##.7102467

### Accuracy
### (true positive + true negative) / total
(glm_acc <- sum(pred_glm_test >= .5 & test_set$outcome == 1) + sum(pred_glm_test < .5 & test_set$outcome == 0))/length(pred_glm_test)
### 71.5%

### Precision
### (true positive) / (true positive + false positive)
(glm_prec <- sum(pred_glm_test >= .5 & test_set$outcome == 1)/(sum(pred_glm_test >= .5 & test_set$outcome == 1) + sum(pred_glm_test >= .5 & test_set$outcome == 0)))
### 55.0%

### Recall
### (true positive) / (true positive + false negative)
(glm_rec <- sum(pred_glm_test >= .5 & test_set$outcome == 1)/(sum(pred_glm_test >= .5 & test_set$outcome == 1) + sum(pred_glm_test < .5 & test_set$outcome == 1)))
### 24.2%

### Which 2018 stations are predicted (outcome == 1)

pred_station <- data.frame(predicted = as.numeric(pred_glm_test), station = as.character(test_set$station))
pred_station <- pred_station %>%
                filter(predicted >= .5) %>%
                group_by(station) %>%
                summarize(count = n(),
                          proportion = sum(predicted)/n()) %>%
                arrange(desc(count))

sum(pred_station$count >= 30)

# Regularization Models ##############################################################################
reg_y <- as.matrix(as.factor(training_set$outcome))
reg_x <- model.matrix(outcome~station + capacity + radius_count + weekday + month + holiday + Max.Temp + Min.Temp + Avg.Temp + Precopitation + New.Snow + Snow.Depth + trace.snow + trace.rain, training_set)[,-1]

valid_y <- as.matrix(as.factor(validation_set$outcome))
valid_x <- model.matrix(outcome~station + capacity + radius_count + weekday + month + holiday + Max.Temp + Min.Temp + Avg.Temp + Precopitation + New.Snow + Snow.Depth + trace.snow + trace.rain, validation_set)[,-1]

test_y <- as.matrix(test_set$outcome)
test_x <- model.matrix(outcome~station + capacity + radius_count + weekday + month + holiday + Max.Temp + Min.Temp + Avg.Temp + Precopitation + New.Snow + Snow.Depth + trace.snow + trace.rain, test_set)[,-1]

## Ridge #############################################################################################
cv_ridge <- cv.glmnet(reg_x, reg_y, alpha = 0, family = 'binomial')
ridge_lambda_min <- cv_ridge$lambda.min
ridge_full <- glmnet(x = reg_x, y = reg_y, lambda = ridge_lambda_min, alpha = 0, family = 'binomial')
ridge_coefs <- ridge_full$beta
ridge_coefs <- data.frame(name = rownames(ridge_coefs), coefs = as.numeric(ridge_coefs), abs = abs(as.numeric(ridge_coefs)))
ridge_coefs <- ridge_coefs %>%
               arrange(desc(abs))

### Validation AUC
pred_ridge_valid <- predict(ridge_full, valid_x, type = 'response')
rocr_ridge_valid <- prediction(pred_ridge_valid, valid_y)
perf_ridge_valid <- performance(rocr_ridge_valid,'auc')
(auc_ridge_valid <- perf_ridge_valid@y.values[[1]])
###.7109858

### Test Set AUC
pred_ridge_test <- predict(ridge_full, test_x, type = 'response')
rocr_ridge_test <- prediction(pred_ridge_test, test_y)
perf_ridge_test <- performance(rocr_ridge_test, "auc")
(auc_ridge_test <- perf_ridge_test@y.values[[1]])
###.7104057

### Accuracy 
### (true positive + true negative) / total
(ridge_acc <- sum(pred_ridge_test >= .5 & test_set$outcome == 1) + sum(pred_ridge_test < .5 & test_set$outcome == 0))/length(pred_ridge_test)
### 71.5%

### Precision
### (true positive) / (true positive + false positive)
(ridge_prec <- sum(pred_ridge_test >= .5 & test_set$outcome == 1)/(sum(pred_ridge_test >= .5 & test_set$outcome == 1) + sum(pred_ridge_test >= .5 & test_set$outcome == 0)))
### 55.3%

### Recall
### (true positive) / (true positive + false negative)
(ridge_rec <- sum(pred_ridge_test >= .5 & test_set$outcome == 1)/(sum(pred_ridge_test >= .5 & test_set$outcome == 1) + sum(pred_ridge_test < .5 & test_set$outcome == 1)))
### 23.6%

### Which 2018 stations are predicted (outcome == 1)

pred_ridge_station <- data.frame(predicted = as.numeric(pred_ridge_test), station = as.character(test_set$station))
pred_ridge_station <- pred_ridge_station %>%
                      filter(predicted >= .5) %>%
                      group_by(station) %>%
                      summarize(count = n()) %>%
                      arrange(desc(count))

sum(pred_ridge_station$count >= 30)

## Lasso #############################################################################################
cv_lasso <- cv.glmnet(reg_x, reg_y, alpha = 1, family = 'binomial')
lasso_lambda_min <- cv_lasso$lambda.min
lasso_full <- glmnet(x = reg_x, y = reg_y, lambda = lasso_lambda_min, alpha = 1, family = 'binomial')
lasso_coefs <- lasso_full$beta
lasso_coefs <- data.frame(name = rownames(lasso_coefs), coefs = as.numeric(lasso_coefs), abs = abs(as.numeric(lasso_coefs)))
lasso_coefs <- lasso_coefs %>%
               arrange(desc(abs))

### Validation AUC
pred_lasso_valid <- predict(lasso_full, valid_x, type = 'response')
rocr_lasso_valid <- prediction(pred_lasso_valid, valid_y)
perf_lasso_valid <- performance(rocr_lasso_valid,'auc')
(auc_lasso_valid <- perf_lasso_valid@y.values[[1]])
###.710834

### Test Set AUC
pred_lasso_test <- predict(lasso_full, test_x, type = 'response')
rocr_lasso_test <- prediction(pred_lasso_test, test_y)
perf_lasso_test <- performance(rocr_lasso_test,'auc')
(auc_lasso_test <- perf_lasso_test@y.values[[1]])
###.7101613

### Accuracy
### (true positive + true negative) / total
(lasso_acc <- sum(pred_lasso_test >= .5 & test_set$outcome == 1) + sum(pred_lasso_test < .5 & test_set$outcome == 0))/length(pred_lasso_test)
### 71.5%

### Precision
### (true positive) / (true positive + false positive)
(lasso_prec <- sum(pred_lasso_test >= .5 & test_set$outcome == 1)/(sum(pred_lasso_test >= .5 & test_set$outcome == 1) + sum(pred_lasso_test >= .5 & test_set$outcome == 0)))
### 55.6%

### Recall
### (true positive) / (true positive + false negative)
(lasso_rec <- sum(pred_lasso_test >= .5 & test_set$outcome == 1)/(sum(pred_lasso_test >= .5 & test_set$outcome == 1) + sum(pred_lasso_test < .5 & test_set$outcome == 1)))
### 22.95%

### Which 2018 stations are predicted (outcome == 1)

pred_lasso_station <- data.frame(predicted = as.numeric(pred_lasso_test), station = as.character(test_set$station))
pred_lasso_station <- pred_lasso_station %>%
                      filter(predicted >= .5) %>%
                      group_by(station) %>%
                      summarize(count = n()) %>%
                      arrange(desc(count))

### Generate prediction lasso data frame for leaflet map in figures.R

pred_lasso_mean <- data.frame(predicted = as.numeric(pred_lasso_test), station = as.character(test_set$station))
pred_lasso_mean <- pred_lasso_mean %>%
                   group_by(station) %>%
                   summarize(rebalance_proportion = mean(predicted))

sum(pred_lasso_station$count >= 30)
pred_lasso_station <- left_join(pred_lasso_station, pred_lasso_mean, by = "station")

lat_long_stations <- read_csv("data/latlong_stations.csv")
colnames(lat_long_stations) <- c("station", "latitude", "longitude")
pred_lasso_station_map <- pred_lasso_station %>%
                          filter(count >= 30) %>%
                          left_join(lat_long_stations, by = "station")
