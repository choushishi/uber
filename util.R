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

## map hash and Id back and forth

clustermap = read.csv("./training_data/cluster_map/cluster_map", sep = "\t", header = FALSE)

hashIdMap = function(x) {
    wow = clustermap[(clustermap == x)[,c(2,1)]]
    return(wow)
}