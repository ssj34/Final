library(dplyr)
library(ggplot2)
library(caret)
library(lubridate)
library(sqldf)
library(Metrics)


##### convert celcius to ferenheit #####
cTOf <- function(int){
  fer <- rep(NA,length(int))
  for (i in 1:length(int)){
    ci <- int[i]
    cel <- ci*(9/5)
    fer[i] <- cel+32
  }
  return(fer)
}


#########################################################
clean_data <- function(data_frame){
  
  #Create a hour coulmn
  data_frame$hour <- substr(data_frame$datetime, 12, 20) %>% as.factor
  
  #create day of week column
  data_frame$weekday <- data_frame$datetime %>% as.Date %>% weekdays %>% as.factor
  
  #turn temps in to ferenheit
  data_frame$temp <- cTOf(data_frame$temp)
  
  data_frame$season <- as.factor(data_frame$season)
  data_frame$holiday <- as.factor(data_frame$holiday)
  data_frame$workingday <- as.factor(data_frame$workingday)
  data_frame$weather <- as.factor(data_frame$weather)
  data_frame$casual <- NULL
  data_frame$registered <- NULL
  
  return(data_frame)
}
##########################################################################

plot_Days_Count <- function(data_frame){
  day_hour <- sqldf::sqldf('select weekday, hour, avg(count) as count from data_frame group by weekday, hour')
  
  p <-ggplot(train, aes(x=hour, y=count, color=weekday))+
    geom_line(data =day_hour, aes(group = weekday))+
    ggtitle("Bikes Rented By Weekday")
  return(print(p))
}
############################################################################

plot_year_nRent <- function(unfactored){
  rows <- nrow(unfactored)
  mon_day <- rep(NA,rows)
  for(i in 1:rows){
    cd <- as.character(unfactored$datetime[i])
    month <- strsplit(cd,"\\- |\\-| ")[[1]][2]
    day <- strsplit(cd,"\\- |\\-| ")[[1]][3]
    mon_day[i] <- paste(month,day, sep = "/")
  }
  
  
  unfactored$mon_day <- mon_day
  
  cl <- length(unique(unfactored$mon_day))
  nRent <- rep(NA, cl)
  avg_temp <- rep(NA, cl)
  for (i in 1:cl){
    uni_day <-unique(unfactored$mon_day)
    cd <- uni_day[i]
    df_filt <- dplyr::filter(unfactored, mon_day == cd)
    nRent[i] <- sum(df_filt$count)
    avg_temp[i] <- df_filt$temp %>% as.numeric %>% mean
  }
  
  temp <- cTOf(avg_temp)
  uni_day <- unique(unfactored$mon_day)
  dayCount <- data.frame(uni_day,nRent,temp)
  
  p <- ggplot(aes(uni_day, nRent, color = temp), data = dayCount)
  p <- p + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))+scale_color_gradient(low="blue", high="red")+
    ggtitle("Bikes Rented over the year")
  return (print(p))
}
####################################################

sd_seasons <- function(traindf, season){
  
  if(season == "Spring"){
    season2 <- 1
  }
  if(season == "Summer"){
    season2 <- 2
  }
  if(season == "Fall"){
    season2 <- 3
  }
  if(season == "Winter"){
    season2 <- 4
  }
  
  df_filt <- filter(traindf, season == season2)
  sd <- df_filt$count %>% sd
  return(sd)
}
####################################################

makePrediction <- function (model, test) {
  pred <- predict(model, test)
  pred <- round(pred)
  pred[pred<0] <- 0
  df <- data.frame(test$datetime, c(count=pred))
  names(df) <- c("datetime", "count")
  return(df)
}

prediction_error <- function(model, validation){
  pred1 <- predict(model, validation)
  pred1[pred1<0] <- 0
  error <- Metrics::rmsle(validation$count,pred1)
  return(error)
}




