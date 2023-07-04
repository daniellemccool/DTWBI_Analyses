library(gatepoints)
library(geodist)
source("plot_functions.R")

selectByLatLon <- function(data, flaggedid, buffer = 100){
  flaggedid[, `:=`(low = id - buffer, high = id + buffer)]
  test <- data[flaggedid, on = .(personset = personset, id > low, id < high), .(x.id, latitude, longitude, flag_sev)]
  plot(test$longitude, test$latitude, col = ifelse(test$flag_sev > 0, "black", "red"), lwd = ifelse(test$flag_sev > 0, 5, 1), type = "b")
  bla <- fhs(test[, .(longitude, latitude)])
  data[test[as.numeric(bla)], on = .(id == x.id), filter := TRUE]

}

plotByTimeLat <- function(data, flaggedid, buffer = 100){
  flaggedid[, `:=`(low = id - buffer, high = id + buffer)]
  test <- data[flaggedid, on = .(personset = personset, id > low, id < high), .(x.id, timestamp, latitude, flag_sev)]
  plot(test$timestamp, test$latitude, col = ifelse(test$flag_sev > 0, "black", "red"), lwd = ifelse(test$flag_sev > 0, 5, 1), type = "b")
  
}

ggplotByTimeLat <- function(data, flaggedid, buffer = 100){
  flaggedid[, `:=`(low = id - buffer, high = id + buffer)]
  test <- data[flaggedid, on = .(personset = personset, id > low, id < high), .(x.id, timestamp, latitude, flag_sev)]
  ggplot(test, aes(x = timestamp, y = latitude)) +
    geom_path(color = "black", size = .1)+
    geom_point(aes(color = factor(flag_sev), size = flag_sev != -1)) +
    scale_size_manual(values = c(.5, 3)) +
    theme_minimal()
  
}



selectByTimeLat <- function(data, flaggedid, buffer = 100){
  flaggedid[, `:=`(low = id - buffer, high = id + buffer)]
  test <- data[flaggedid, on = .(personset = personset, id > low, id < high), .(x.id, timestamp, latitude, flag_sev)]
  plot(test$timestamp, test$latitude, col = ifelse(test$flag_sev > 0, "black", "red"), lwd = ifelse(test$flag_sev > 0, 5, 1), type = "b")
  bla <- fhs(test[, .(timestamp, latitude)])
  data[test[as.numeric(bla)], on = .(id == x.id), filter := TRUE]

}

selectByTimeLon <- function(data, flaggedid, buffer = 100){
  flaggedid[, `:=`(low = id - buffer, high = id + buffer)]
  test <- data[flaggedid, on = .(personset = personset, id > low, id < high), .(x.id, timestamp, longitude, flag_sev)]
  plot(test$timestamp, test$longitude, col = ifelse(test$flag_sev > 0, "black", "red"), lwd = ifelse(test$flag_sev > 0, 5, 1), type = "b")
  bla <- fhs(test[, .(timestamp, longitude)])
  data[test[as.numeric(bla)], on = .(id == x.id), filter := TRUE]

}

selectByGps <- function(data, flaggedid, buffer = 100){
  flaggedid[, `:=`(low = id - buffer, high = id + buffer)]
  test <- data[flaggedid, on = .(personset = personset, id > low, id < high), .(x.id, timestamp, latitude, flag_sev, speed)]
  plot(test$timestamp, test$latitude, lwd = ifelse(test$flag_sev > 0, 5, 1), col = ifelse(test$speed > 0, "black", "red"), type = "b")
  bla <- fhs(test[, .(timestamp, latitude)])
  data[test[as.numeric(bla)], on = .(id == x.id), filter := TRUE]

}



justData <- function(data, flaggedid, buffer = 100){
  flaggedid[, `:=`(low = id - buffer, high = id + buffer)]
  data[flaggedid, on = .(personset = personset, id > low, id < high), .(x.id, latitude, longitude, flag_sev, filter, slope)]
}


calculateSpeeds <- function(){
  data[, ttn2 :=  shift(timestamp,n = -1) - timestamp, personset]
  data[, tfp2 := shift(ttn2), personset]
  data[, kmfp := geodist_vec(longitude, latitude, sequential = TRUE, pad = TRUE, measure = "haversine")/1000, .(personset)]
  data[, kph := kmfp/time_length(tfp2, "hours")]
  data[, easytfp2 := tfp2 + 2]
  data[, easykph := kmfp/(time_length(easytfp2, "hours"))]
}

recalculateSpeedsFiltered <- function(){
  data[, `:=`(ttn2 = NA, tfp2 = NA, kmfp = NA, kph = NA, easytfp2 = NA, easykph = NA)]
  data[filter == FALSE, ttn2 :=  shift(timestamp,n = -1) - timestamp, personset]
  # data[filter == FALSE, tfp2 := shift(ttn2, fill = NA), personset]
  data[filter == FALSE, tfp2 := timestamp - shift(timestamp), personset]
  data[filter == FALSE, kmfp := geodist_vec(longitude, latitude, sequential = TRUE, pad = TRUE, measure = "haversine")/1000, .(personset)]
  data[, kph := kmfp/time_length(tfp2, "hours")]
  data[, easytfp2 := tfp2 + 2]
  data[, easykph := kmfp/(time_length(easytfp2, "hours"))]
}

recalculateSpeedsFilteredForDevid <- function(ps){
  data[filter == FALSE & personset == ps, ttn2 :=  shift(timestamp,n = -1) - timestamp, personset]
  # data[filter == FALSE & personset == ps, tfp2 := shift(ttn2), personset]
  data[filter == FALSE & personset == ps, tfp2 := timestamp - shift(timestamp), personset]
  data[filter == FALSE & personset == ps, kmfp := geodist_vec(longitude, latitude, sequential = TRUE, pad = TRUE, measure = "haversine")/1000, .(personset)]
  data[personset == ps, kph := kmfp/time_length(tfp2, "hours")]
  data[personset == ps, easytfp2 := tfp2 + 2]
  data[personset == ps, easykph := kmfp/(time_length(easytfp2, "hours"))]
}


flagSpeeds <- function(){
  data[, flag_sev := -1]
  data[easykph > 200, flag_sev := 4]
  data[easykph > 500, flag_sev := 6]
  data[easykph > 1000, flag_sev := 7]
}

flagSpeedsForDevid <- function(ps){
  data[personset == ps, flag_sev := -1]
  data[personset == ps & easykph > 200, flag_sev := 4]
  data[personset == ps & easykph > 500, flag_sev := 6]
  data[personset == ps & easykph > 1000, flag_sev := 7]
}

updateForCurrent <- function(flaggedid){
  ps <- flaggedid[, personset]
  recalculateSpeedsFilteredForDevid(ps)
  flagSpeedsForDevid(ps)
}

removeZeroSpeeds <- function(data, flaggedid, buffer = 50) {
  id <- flaggedid[, id]
  low <- id - buffer
  high <- id + buffer
  data[id %in% low:high & speed == 0, filter := TRUE]
}


mapByGps <- function(data, flaggedid, buffer = 500){
  flaggedid[, `:=`(low = id - buffer, high = id + buffer)]
  test <- data[flaggedid, on = .(personset = personset, id > low, id < high), .(x.id, latitude, longitude, flag_sev, filter, "fromgps" = (speed > 0 ))]
  plotMapPointLineBinaryColorFacet(test[filter == FALSE], color.by = "flag_sev")
}

checkMap <- function(data, flaggedid, buffer = 2000){
  flaggedid[, `:=`(low = id - buffer, high = id + buffer)]
  test <- data[flaggedid, on = .(personset = personset, id > low, id < high), .(x.id, latitude, longitude, flag_sev, filter)]
  plotMapPointLineBinaryColor(test[filter == FALSE], color.by = "flag_sev")
}

unfilterForPersonset <- function(ps){
  data[personset == ps, filter := FALSE]
}
