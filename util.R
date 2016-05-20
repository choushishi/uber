## This file stores utilities. 

## time to TS converter.
timeToTS = function(time) {

    if (time == "00:00:00") {
        return(1)
    }

    time1 = strptime(time, format = "%H:%M:%S")
    time2 = strptime("00:00:00", format = "%H:%M:%S")
    minutespassed = as.numeric(difftime(time1, time2, units = "mins"))
    
    return(ceiling(minutespassed / 10))
}

