library(dplyr)
library(ggplot2)
library(caret)
library(lubridate)
library(gbm)
library(Metrics)


#Read in the bikesharing data set

train <- read.csv("train.csv", header = T, 
                 stringsAsFactors = F)
test <- read.csv("test.csv", header = T, 
                 stringsAsFactors = F)


###################################################
train <- clean_data(train)
test <- clean_data(test)
###################################################

plot_year_nRent(train)
sd_seasons(train,"Summer")
sd_seasons(train,"Fall")
sd_seasons(train,"Winter")
sd_seasons(train,"Spring")

plot_Days_Count(train)

###################################################

inTrain <- createDataPartition(y=train$count, p=0.75, list=F)
training <- train[inTrain, ]
validation <- train[-inTrain,]

Grid <-  expand.grid( n.trees = 500, interaction.depth = 10, shrinkage = 0.05,
                      n.minobsinnode = 10)

fitControl <- trainControl(
  method = "repeatedcv",
  number = 6,
  repeats = 6)

regress <- count ~ season + holiday + workingday + weather + temp + humidity + windspeed + hour + weekday


t_out_gbm <- caret::train(regress, data=training, tuneGrid = Grid, trControl = fitControl, method = 'gbm', maximize = F)


t_out_lm <- caret::train(regress, data = training, trControl = fitControl, method = "lm")

t_out_gbm %>% print
t_out_lm %>% print
