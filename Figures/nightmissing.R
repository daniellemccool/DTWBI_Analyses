library(lubridate)
library(ggplot2)
library(scales)
library(gridExtra)

source("../shared_code/file_reading_functions.R")
source("../shared_code/file_merging_functions.R")
source("../shared_code/file_cleaning_functions.R")

data <- readInData(getNewFilenames(as.if.day = "2018-12-15"))

spss    <- getSpssData("haven")

devices   <- readInData(getNewFilenames(type = "device", as.if.day = "2018-12-16"))
users     <- readInData(getNewFilenames(type = "user", as.if.day = "2018-12-16"))
config    <- readInData(getNewFilenames(type = "config", as.if.day = "2018-12-16"))
questions <- readInData(getNewFilenames(type = "questions", as.if.day = "2018-12-15"))
motives   <- readInData(getNewFilenames(type = "motives", as.if.day = "2018-12-16"))
stops     <- readInData(getNewFilenames(type = "stops", as.if.day = "2018-12-16"))
stopnames <- readInData(getNewFilenames(type = "stopnames", as.if.day = "2018-12-16"))
tracks    <- readInData(getNewFilenames(type = "tracks", as.if.day = "2018-12-16"))
modes     <- readInData(getNewFilenames(type = "modes", as.if.day = "2018-12-16"))
logs      <- readInData(getNewFilenames(type = "logs", as.if.day = "2018-12-16"))
battery   <- readInData(getNewFilenames(type = "battery", as.if.day = "2018-12-16"))

data <- mergeDataDevices(data, devices)
data <- mergeDataUsers(data, users)
data <- keepParticipants(data)
data <- copy(data[!is.na(id)])
data[, date := floor_date(timestamp,  "day")]

setorder(data, device_id, timestamp)
data[, tfp := timestamp - shift(timestamp), .(device_id)]
data[, ttn := difftime(shift(timestamp, -1), timestamp, "seconds"), .(device_id)]
data[ttn > minutes(6), gap_start := TRUE]
data[ttn <= minutes(6), covered := TRUE]
data[covered == TRUE, covered_intervals := time_length((sum(ttn, na.rm = TRUE)), "days"), .(device_id)]


data[, datehour := floor_date(timestamp, "hour")]
data[covered == TRUE, covered_hours := time_length(sum(ttn, na.rm = TRUE), "hours"), .(device_id, datehour)]
data[, covered_hours := na.exclude(covered_hours)[1], .(device_id, datehour)]

data2 <- copy(data[, .SD[1], .(device_id, datehour)])
data2[, houronly := datehour - date + date[1]]

data2[, hours_this_date := uniqueN(datehour), .(device_id, date)]
data2[, first_day := date[1], device_id]
data3 <- copy(data2[((date > first_day) & hours_this_date > 2)])
data3[, weekday := wday(datehour, label = TRUE)]
data3[, nppldays := uniqueN(data.table(device_id, date)), weekday]

timecov <- unique(data3[,   .("perc_coverage" = sum(covered_hours, na.rm = TRUE)/nppldays), .(houronly, weekday)])

saveRDS(timecov, "timecov.RDS")

p2 <- ggplot(timecov, aes(weekday, houronly, fill = perc_coverage))+
  geom_tile(color = "white", size = 0.1) +
  viridis::scale_fill_viridis("Mean hourly coverage", labels = percent, limits = c(.2, .8),
                              guide = guide_colorsteps(even.steps = TRUE, limits = c(0, 100),title.position = "top", direction = "horizontal", title.hjust = .5, barwidth = 10))+
  scale_y_time(labels = function(t) strftime(t, "%H:%M"), breaks = scales::breaks_width("2 hours"), expand = c(0,0))+
  theme_minimal(base_size = 12) +
  labs( x = "Day of the week", y = "hour commencing") +
  theme( strip.background = element_rect(color = "white"),
         legend.position = "bottom") +
  ggExtra::removeGrid()

grid.arrange(p1, p2, ncol = 2)
