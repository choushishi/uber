rm(list = ls())

library(reshape)
library(dplyr)

setwd("~/Playground/RunnerTrack/uber/")
source("util.R")

# the script wrangle the weather data on a daily basis and 
# stick them together afterwards

# this function wrangle the weather data for a single day
weather.daily = function(df) {

    # Split data and time into two different columns
    
    df = transform(df, V1 = colsplit(V1, split = " ", names = c("date", "ts")))
    
    df = data.frame("Date" = df$V1$date, 
                    "TS" = df$V1$ts, 
                    "Weather" = df$V2,
                    "temperature" = df$V3,
                    "PM2.5" = df$V4)

    # Convert time to timeslot
    
    df$TS = sapply(df$TS, timeToTS)
    df$TS = as.integer(df$TS)
    
    # Summarize different rows appear in the same timeslot. 
    
    df = df %>%
        group_by(Date, TS) %>%
        summarise(Weather=mean(Weather),
                  temperature = mean(temperature),
                  PM2.5 = mean(PM2.5))
    df = ungroup(df)

    # Join the data into a full timeslot sequence. See if there
    # is any timeslot possess NA value
    
    df = full_join(timeslotSeq, df, by = "TS")
    
    missing.rows = grep(TRUE, is.na(df$Date))

    # If there is indeed missing values, replace them with the data in 
    # previous time slot

    for (i in missing.rows) {
        
        if (i == 1) { # If the first row is NA, look for values downwards
            
            j = 1
            while (is.na(df[j, "Date"])) { # Keep finding until a valid row show up
                j = j + 1
            }

            # Replace NA values from rows below
            df[i, c("Date", "Weather", "temperature", "PM2.5")] =
                df[j , c("Date", "Weather", "temperature", "PM2.5")]
        } else{ 
            # Replace NA values from the above row
            df[i, c("Date", "Weather", "temperature", "PM2.5")] =
                df[i - 1, c("Date", "Weather", "temperature", "PM2.5")]
        }
    }

    # Join the data into Dist-timeslot-sequence. Now it is a big table for a single day
    # Every Dist share the same weather, so `by` parameter is only fed by "TS" column
    df = full_join(crossProduct, df, by = "TS")
    
    # Re-arrange column order and output something neat.
    return(df[,c(3,1,2,4,5,6)])
}

# Create a timeslot sequence
timeslotSeq = data.frame(TS = c(1:144))
timeslotSeq$TS = as.integer(timeslotSeq$TS)

# Create a Dist index sequence
DistSeq = data.frame(Dist = c(1:66))
timeslotSeq$TS = as.integer(timeslotSeq$TS)

# Cross-join these two sequence to make a big table for a single day
crossProduct = merge(timeslotSeq, DistSeq)

# Now bind them together. 
weather.1 = weather.daily(read.csv("./training_data/weather_data/weather_data_2016-01-01", sep = "\t", header = FALSE))
grep(FALSE, complete.cases(weather.1)) # See if there is annomoly.
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
weather.21 = weather.daily(read.csv("./training_data/weather_data/weather_data_2016-01-21", sep = "\t", header = FALSE))
grep(FALSE, complete.cases(weather.21))

weather = bind_rows(weather.1,weather.2,weather.3,weather.4, weather.5,
                weather.6,weather.7,weather.8,weather.9, weather.10,
                weather.11,weather.12,weather.13,weather.14, weather.15,
                weather.16,weather.17,weather.18,weather.19, weather.20,
                weather.21)

weather = arrange(weather, Date, TS, Dist)

write.csv(weather, "weather.csv", row.names = FALSE)

