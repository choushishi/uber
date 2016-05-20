rm(list = ls())

library(reshape)
library(dplyr)

setwd("~/Playground/RunnerTrack/uber/")
source("util.R")

weather.daily = function(df) {
    
    df = transform(df, V1 = colsplit(V1, split = " ", names = c("date", "ts")))
    
    df = data.frame("Date" = df$V1$date, 
                    "TS" = df$V1$ts, 
                    "Weather" = df$V2,
                    "temperature" = df$V3,
                    "PM2.5" = df$V4)
    
    df$TS = sapply(df$TS, timeToTS)
    df$TS = as.integer(df$TS)
    
    
    df = df %>%
        group_by(Date, TS) %>%
        summarise(Weather=mean(Weather),
                  temperature = mean(temperature),
                  PM2.5 = mean(PM2.5))
    df = ungroup(df)
    
    df = full_join(timeslotSeq, df, by = "TS")
    
    missing.rows = grep(TRUE, is.na(df$Date))

    for (i in missing.rows) {
        
        if (i == 1) {
            
            j = 1
            while (is.na(df[j, "Date"])) {
                j = j + 1
            }

            df[i, c("Date", "Weather", "temperature", "PM2.5")] =
                df[j , c("Date", "Weather", "temperature", "PM2.5")]
        } else{ 
            df[i, c("Date", "Weather", "temperature", "PM2.5")] =
                df[i - 1, c("Date", "Weather", "temperature", "PM2.5")]
        }
    }
    
    df = full_join(crossProduct, df, by = "TS")
    
    return(df[,c(3,1,2,4,5,6)])
}

timeslotSeq = data.frame(TS = c(1:144))
timeslotSeq$TS = as.integer(timeslotSeq$TS)
destSeq = data.frame(Dest = c(1:66))
crossProduct = merge(timeslotSeq, destSeq)

weather.1 = weather.daily(read.csv("./training_data/weather_data/weather_data_2016-01-01", sep = "\t", header = FALSE))
grep(FALSE, complete.cases(weather.1))
weather.2 = weather.daily(read.csv("./training_data/weather_data/weather_data_2016-01-02", sep = "\t", header = FALSE))
grep(FALSE, complete.cases(weather.2))
weather.3 = weather.daily(read.csv("./training_data/weather_data/weather_data_2016-01-03", sep = "\t", header = FALSE))
grep(FALSE, complete.cases(weather.3))
weather.4 = weather.daily(read.csv("./training_data/weather_data/weather_data_2016-01-04", sep = "\t", header = FALSE))
grep(FALSE, complete.cases(weather.4))
weather.5 = weather.daily(read.csv("./training_data/weather_data/weather_data_2016-01-05", sep = "\t", header = FALSE))
grep(FALSE, complete.cases(weather.5))
weather.6 = weather.daily(read.csv("./training_data/weather_data/weather_data_2016-01-06", sep = "\t", header = FALSE))
grep(FALSE, complete.cases(weather.6))
weather.7 = weather.daily(read.csv("./training_data/weather_data/weather_data_2016-01-07", sep = "\t", header = FALSE))
grep(FALSE, complete.cases(weather.7))
weather.8 = weather.daily(read.csv("./training_data/weather_data/weather_data_2016-01-08", sep = "\t", header = FALSE))
grep(FALSE, complete.cases(weather.8))
weather.9 = weather.daily(read.csv("./training_data/weather_data/weather_data_2016-01-09", sep = "\t", header = FALSE))
grep(FALSE, complete.cases(weather.9))
weather.10 = weather.daily(read.csv("./training_data/weather_data/weather_data_2016-01-10", sep = "\t", header = FALSE))
grep(FALSE, complete.cases(weather.10))
weather.11 = weather.daily(read.csv("./training_data/weather_data/weather_data_2016-01-11", sep = "\t", header = FALSE))
grep(FALSE, complete.cases(weather.11))
weather.12 = weather.daily(read.csv("./training_data/weather_data/weather_data_2016-01-12", sep = "\t", header = FALSE))
grep(FALSE, complete.cases(weather.12))
weather.13 = weather.daily(read.csv("./training_data/weather_data/weather_data_2016-01-13", sep = "\t", header = FALSE))
grep(FALSE, complete.cases(weather.13))
weather.14 = weather.daily(read.csv("./training_data/weather_data/weather_data_2016-01-14", sep = "\t", header = FALSE))
grep(FALSE, complete.cases(weather.14))
weather.15 = weather.daily(read.csv("./training_data/weather_data/weather_data_2016-01-15", sep = "\t", header = FALSE))
grep(FALSE, complete.cases(weather.15))
weather.16 = weather.daily(read.csv("./training_data/weather_data/weather_data_2016-01-16", sep = "\t", header = FALSE))
grep(FALSE, complete.cases(weather.16))
weather.17 = weather.daily(read.csv("./training_data/weather_data/weather_data_2016-01-17", sep = "\t", header = FALSE))
grep(FALSE, complete.cases(weather.17))
weather.18 = weather.daily(read.csv("./training_data/weather_data/weather_data_2016-01-18", sep = "\t", header = FALSE))
grep(FALSE, complete.cases(weather.18))
weather.19 = weather.daily(read.csv("./training_data/weather_data/weather_data_2016-01-19", sep = "\t", header = FALSE))
grep(FALSE, complete.cases(weather.19))
weather.20 = weather.daily(read.csv("./training_data/weather_data/weather_data_2016-01-20", sep = "\t", header = FALSE))
grep(FALSE, complete.cases(weather.20))

weather = bind_rows(weather.1,weather.2,weather.3,weather.4, weather.5,
                weather.6,weather.7,weather.8,weather.9, weather.10,
                weather.11,weather.12,weather.13,weather.14, weather.15,
                weather.16,weather.17,weather.18,weather.19, weather.20)

write.csv(weather, "weather.csv", row.names = FALSE)

