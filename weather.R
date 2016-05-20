rm(list = ls())

library(reshape)

setwd("~/Playground/RunnerTrack/uber/")
weather = read.csv("./training_data/weather_data/weather_data_2016-01-01", sep = "\t", header = FALSE)

weather = transform(weather, V1 = colsplit(V1, split = " ", names = c("date", "ts")))

weather = data.frame("Date" = weather$V1$date, 
                     "TS" = weather$V1$ts, 
                     "Weather" = weather$V2,
                     "temperature" = weather$V3,
                     "PM2.5" = weather$V4)

timeToTS = function(time) {

    time1 = strptime(time, format = "%H:%M:%S")
    time2 = strptime("00:00:00", format = "%H:%M:%S")
    
    minutespassed = as.numeric(difftime(time1, time2, units = "mins"))
    return(sprintf("t%d", ceiling(minutespassed/10)))
}

timeToTS("23:59:59")

weather$TS = sapply(weather$TS, timeToTS)

library(dplyr)
weather = weather %>% 
          group_by(Date, TS) %>% 
          summarise(Weather=mean(Weather), 
                    temperature = mean(temperature), 
                    PM2.5 = mean(PM2.5))
weather = ungroup(weather)

