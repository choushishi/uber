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
    hourpassed = strptime(time, format = "%H:%M:%S") - strptime("00:00:00", format = "%H:%M:%S")
    minutespassed = as.numeric(hourpassed * 60)
    return(sprintf("t%d", floor(minutespassed/10)))
}



timeToTS("08:12:20")
