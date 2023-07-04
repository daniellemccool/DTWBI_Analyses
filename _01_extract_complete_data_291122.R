library(data.table)
library(lubridate)
library(topdowntimeratio)
library(stopdetection)
library(hms)

## After manual cleaning/visual speed inspection
data <- readRDS("Data/datacleaning.RDS")
setDT(data)

# Remove filtered points
data <- copy(data[filter == FALSE])
data <- copy(data[, .(device_id, latitude, longitude, accuracy, speed, altitude, timestamp, user_id, username, filter)])

setorder(data, device_id, timestamp)
data[, prev_ts := shift(timestamp), device_id]
data[, next_ts := shift(timestamp, -1), device_id]
data[, ttn := next_ts - timestamp]
data[, tfp := timestamp - prev_ts]

data[, last_before_gap := FALSE]
data[, first_after_gap := FALSE]

data[tfp > minutes(6), first_after_gap := TRUE]
data[, group := cumsum(first_after_gap), device_id]

data[, device_grp := paste(device_id, sprintf("%03d", group), sep = "-")]

data[first_after_gap == FALSE, covered_time := sum(tfp, na.rm = TRUE), device_grp]


fulldata <- copy(data[covered_time > hours(24)])
setorder(fulldata, device_grp, timestamp)
fulldata[, time_elapsed := timestamp - timestamp[1], device_grp]
fulldata[, fifteens := floor(as.period(time_elapsed)/minutes(15))]
fulldata[, fifteens := sprintf("%04d", fifteens)]
fulldata[, grouping := paste(device_grp, fifteens, sep = "-")]
setnames(fulldata, "group", "grp")



res <- tdtr(fulldata,
            col_names = list(entity_id_col = "device_grp",
                             timestamp_col = "timestamp",
                             latitude_col = "latitude",
                             longitude_col = "longitude"),
            group_col = "grouping", max_error = 50)
saveRDS(res, "segmentation_results.RDS")
segs <- getSegments(res, group = TRUE)
saveRDS(segs, "segmentation_segs.RDS")
databyfifteen <- segs[, .(tsstart = min(seg_start_time),
                          tsend = max(seg_end_time),
                          dist = sum(segdist),
                          tsstartlat = seg_start_lat[1],
                          tsendlat = seg_end_lat[.N],
                          tsstartlon = seg_start_lon[1],
                          tsendlon = seg_end_lon[.N]),
                      .(entity_id, .id)]
saveRDS(databyfifteen, "dist_by_fifteen.RDS")


eids <- res[, unique(entity_id)]
res[, longitude := lon]
res[, latitude := lat]

for (this_eid in eids) {
  stops <- stopFinder(res[entity_id == this_eid], thetaD = 200, thetaT = 60*3)
  try(mergingCycle(stops, thetaD = 200, small_track_action = "exclude"))
  res[entity_id == this_eid, `:=`(state = stops$state, state_id = stops$state_id)]
}
saveRDS(res, "segmentation_and_move_results.RDS")

res[, any_move_events := any(state == "moving"), .id]
res[state == "moving", n_move_events := uniqueN(state_id), .id]
res[any_move_events == FALSE, n_move_events := 0]
res[, n_move_events := max(n_move_events, na.rm = TRUE), .id]

databyfifteen[res, on = .(.id), n_move_events := i.n_move_events]


fulldat <- databyfifteen
setDT(fulldat)

gapHours <- function(gaplength, minlead, minlag, hours = 24 * 4){
  window <- hours - minlead - minlag - gaplength
  start <- sample(1:window, 1) + minlead
  start:(start+gaplength)
}

fulldat[, dist_bu := copy(dist)]
groups <- fulldat[, unique(entity_id)]
fulldat[, n15 := .N, entity_id]

set.seed(11399327)
missinggroups <- sample(groups, 100)
nmiss <- sample(1:48, 100, replace = TRUE)

for (i in seq_along(missinggroups)) {
  fulldat[entity_id %in% missinggroups[i], missing := .I %in% gapHours(nmiss[i], 2, 2, n15[1])]
}

fulldat[missing == TRUE, dist := NA]

## For some sims
saveRDS(fulldat, "Data/ddataforsim-29112022.RDS")

ddataforsim <- readRDS("Data/ddataforsim-29112022.RDS")
ddataforsim[, dist_bu := dist_bu/1000]
ddataforsim[, dist := dist_bu]
ddataforsim[, missing := NULL]
ddataforsim[, starttime := as_datetime(tsstart)]
ddataforsim[, time := as_hms(starttime)]
setnames(ddataforsim, c("tsstartlat", "tsendlat", "tsstartlon", "tsendlon"), c("startlat", "endlat", "startlon", "endlon"))

## For most sims
saveRDS(ddataforsim, "Data/nonvarmiss-15022023.RDS")





