source("shared_code/file_reading_functions.R")
source("shared_code/file_merging_functions.R")
source("shared_code/file_cleaning_functions.R")
source("shared_code/visual_speed_inspection_utils.R")

# Set up reference data ---------------------------------------------------

data <- readInData(getNewFilenames(as.if.day = "2018-12-15"))

# Remove unneessary columns
data[, created_at := NULL]
data[, id := NULL]
data[, distance_between_previous_position := NULL]

# Remove duplicates
data <- copy(unique(data, by = c("device_id", "latitude", "longitude", "timestamp")))

# Remove data from cell towers
data <- data[accuracy < 200]

# We have to do the ordering a number of times
setorder(data, device_id, timestamp)


# These are all standard data loading I've used for two years
devices   <- readInData(getNewFilenames(type = "device", as.if.day = "2018-12-16"))
users     <- readInData(getNewFilenames(type = "user", as.if.day = "2018-12-16"))
data      <- mergeDataDevices(data, devices)
data      <- mergeDataUsers(data, users)
data      <- keepParticipants(data)
data <- data[!is.na(device_id)]
data <- data[!is.na(timestamp)]

data[, c("desired_accuracy", "heading") := list(NULL)]


# Sets to test ------------------------------------------------------------
data[, id := .I]
data[, ttn := shift(timestamp,n = -1) - timestamp, device_id]
data[, tfp := shift(ttn), device_id]
data[, nextset := FALSE]
data[tfp > hours(12), nextset := TRUE]
data[, setid := cumsum(nextset), device_id]
data[, personset := paste(device_id, setid, sep = "-"), device_id]


# Instantaneous speed -----------------------------------------------------

data[, ttn2 :=  shift(timestamp,n = -1) - timestamp, personset]
data[, tfp2 := shift(ttn2), personset]
data[, kmfp := geodist_vec(longitude, latitude, sequential = TRUE, pad = TRUE, measure = "haversine")/1000, .(personset)]
data[, kph := kmfp/time_length(tfp2, "hours")]
data[, easytfp2 := tfp2 + 2]
data[, easykph := kmfp/(time_length(easytfp2, "hours"))]


# Manual outlier removal --------------------------------------------------

## This part was done by hand, and no mutation list is available for which the 
## decisions made on which points to filter or not.

data <- readRDS("Data/datacleaning2.RDS") # Interim state
data[personset == "149-0", filter := FALSE] # airplane

recalculateSpeedsFiltered()
flagSpeeds()

updateForCurrent(flaggedids[1])
flaggedids <- data[flag_sev == 4 & filter == FALSE & !personset %in% c("149-0", "39-1"), .(personset, id, filter, slope, rolvar)]
selectByTimeLon(data, flaggedids[1], buffer = 1000)
selectByTimeLon(data, flaggedids[1], buffer = 100)
selectByTimeLon(data, flaggedids[1], buffer = 10)

selectByGps(data, flaggedids[1], buffer = 1000)
selectByLatLon(data, flaggedids[1], buffer = 50)
selectByTimeLat(data, flaggedids[1], buffer = 100)
checkMap(data, flaggedids[1], 50)
mapByGps(data, flaggedid = flaggedids[1], buffer = 50)
removeZeroSpeeds(data, flaggedids[1], buffer = 50)
test <- justData(data, flaggedids[1], 500)
data
saveRDS(data, "Data/datacleaning.RDS")

