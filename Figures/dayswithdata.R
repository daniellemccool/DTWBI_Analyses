library(lubridate)
library(ggplot2)

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
data[, n_days_data := uniqueN(date), .(device_id)]


data[, datehour := floor_date(timestamp, "hour")]
data[covered == TRUE, covered_hours := time_length(sum(ttn, na.rm = TRUE), "hours"), .(device_id, datehour)]
data[, covered_hours := na.exclude(covered_hours)[1], .(device_id, datehour)]

missfig <- data[, covered_hours[1], .(device_id, datehour, date, n_days_data, os)][
  V1 > .8, uniqueN(datehour), .(device_id, date, n_days_data, os)][
    , .("total_covered_hours" = sum(V1)), .(device_id, n_days_data)]

missfig[, avg_hourly_coverage := (total_covered_hours / 24)/n_days_data ]

saveRDS(missfig, "missfig1.RDS")

p1 <- ggplot(missfig, aes(x = n_days_data, fill = avg_hourly_coverage, group = avg_hourly_coverage)) +
  geom_histogram(binwidth = 1) +
  viridis::scale_fill_viridis("Mean hourly coverage", labels = percent, guide = guide_colorsteps(even.steps = TRUE, title.position = "top", direction = "horizontal", title.hjust = .5, barwidth = 10))+
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom") +
  scale_x_continuous("N days contributed")
