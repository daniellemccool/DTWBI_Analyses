library(data.table)
library(lubridate)
library(rio)


tabi_input_folder <<- "Input"

getNewFilenames <- function(type = "location", reaggregate.all = FALSE, as.if.day = today(), input_folder = tabi_input_folder){
  ####
  # Currently gets the filenames for the location files containing all the time location
  # data. Filters out the folders that aren't from during the experiment.
  ####
  ## Parameters
  ##### (character) type: Select from: (location, device, user, config, motives, stops, stopnames)
  ##### (logical) reaggregate.all: unused
  ##### (Date) as.if.day: defaults to today()
  ## Returns
  ##### (character/character[vector])
  ## Side effects
  ##### None
  currentdir <- getwd()
  setwd(input_folder) 
  
  # Unimportant, wanted to leave room to expand this function, but it was more
  # trouble than it was worth.
  
  switch (type,
    "location"  = filenames <- list.files(recursive = TRUE, pattern = "*position_entries.csv"),
    "device"    = filenames <- list.files(recursive = TRUE, pattern = "*devices.csv"),
    "user"      = filenames <- list.files(recursive = TRUE, pattern = "*users.csv"),
    "config"    = filenames <- list.files(recursive = TRUE, pattern = "*user_device_config.csv"),
    "motives"   = filenames <- list.files(recursive = TRUE, pattern = "*stop_motives.csv"),
    "stops"     = filenames <- list.files(recursive = TRUE, pattern = "*stop_visits.csv"),
    "stopnames" = filenames <- list.files(recursive = TRUE, pattern = "*user_stops.csv"),
    "tracks"    = filenames <- list.files(recursive = TRUE, pattern = "*tracks.csv"),
    "modes"     = filenames <- list.files(recursive = TRUE, pattern = "*transportation_modes.csv"),
    "logs"      = filenames <- list.files(recursive = TRUE, pattern = "*logs.csv"),
    "questions" = filenames <- list.files(recursive = TRUE, pattern = "*questions_answers.csv"),
    "battery"   = filenames <- list.files(recursive = TRUE, pattern = "*battery_infos.csv")
    
  )  
  
  # Don't include data from last several years
  filenames <- filenames[!(filenames %like% "2018/" |
                             filenames %like% "2018-10-04/" |
                             filenames %like% "1900")]
  
  # For pretending we're a different day -- is very sensitive to the current
  # filename structure. Expect this to break.
  if (as.if.day != today()){
    dates <- substr(filenames, 1, 10)
    filenames <- filenames[dates < as.if.day]
  }
  
  # Brings all files together again, disregarding ones that have already been
  # handled. Defunct.
  # if(reaggregate.all == TRUE){
  #   return(filenames)
  # } 
  #handled.files <- readRDS(saved.handled.filenames)
  #filenames[!filenames %in% handled.files]
  setwd(currentdir)
  filenames
}
getOldFilenames <- function(type = "location", reaggregate.all = FALSE, input_folder = tabi_input_folder){
  ####
  # Currently gets the filenames for the location files containing all the time location
  # data. Filters out the folders that aren't from during the experiment.
  ####
  ## Parameters
  ##### (character) type: Select from: (location, device, user, config, motives, stops, stopnames)
  ##### (logical) reaggregate.all: unused
  ##### (Date) as.if.day: defaults to today()
  ## Returns
  ##### (character/character[vector])
  ## Side effects
  ##### None
  ##### 
  currentdir <- getwd()
  setwd(input_folder) 
  
  # Unimportant, wanted to leave room to expand this function, but it was more
  # trouble than it was worth.
  
  switch (type,
          "location"  = filenames <- list.files(recursive = TRUE, pattern = "*position_entries.csv"),
          "device"    = filenames <- list.files(recursive = TRUE, pattern = "*devices.csv"),
          "user"      = filenames <- list.files(recursive = TRUE, pattern = "*users.csv"),
          "config"    = filenames <- list.files(recursive = TRUE, pattern = "*user_device_config.csv"),
          "motives"   = filenames <- list.files(recursive = TRUE, pattern = "*stop_motives.csv"),
          "stops"     = filenames <- list.files(recursive = TRUE, pattern = "*stop_visits.csv"),
          "stopnames" = filenames <- list.files(recursive = TRUE, pattern = "*user_stops.csv"),
          "tracks"    = filenames <- list.files(recursive = TRUE, pattern = "*tracks.csv"),
          "modes"     = filenames <- list.files(recursive = TRUE, pattern = "*transportation_modes.csv"),
          "logs"      = filenames <- list.files(recursive = TRUE, pattern = "*logs.csv"),
          "questions" = filenames <- list.files(recursive = TRUE, pattern = "*questions_answers.csv")
  )  
  
  # Don't include data from last several years
  filenames <- filenames[filenames %like% "2018/" |
                           filenames %like% "2018-10-04/"]
  
  # For pretending we're a different day -- is very sensitive to the current
  # filename structure. Expect this to break.
  # if (as.if.day != today()){
  #   dates <- substr(filenames, 1, 10)
  #   filenames <- filenames[dates < as.if.day]
  # }
  # 
  # Brings all files together again, disregarding ones that have already been
  # handled. Defunct.
  # if(reaggregate.all == TRUE){
  #   return(filenames)
  # } 
  #handled.files <- readRDS(saved.handled.filenames)
  #filenames[!filenames %in% handled.files]
  setwd(currentdir)
  filenames
}

readInData <- function(filenames,
                         dates = "all",
                         input_folder = tabi_input_folder,
                         save_as_rds = FALSE,
                         save_loc = "~/TravelAppData/Data/mergedfiles.RDS"){
  currentdir <- getwd()
  setwd(input_folder)
  
  if(filenames[1] %like% "user_device_config"){
    library(jsonlite)
    cat("Config file read identified. It's different. \n")
    filenames <- filenames[1]
    cat("Starting data read of: ", filenames)
    config.with.json <- read.table(filenames,
                             colClasses = c(rep("character", 3), rep("NULL", 3)),
                             header = TRUE,
                             sep = ",")
    jsondf <- stream_in(textConnection(config.with.json$config))
    jsondf <- jsondf[[1]]
    config.unjsoned         <- cbind(config.with.json, jsondf)
    setDT(config.unjsoned)
    return(config.unjsoned)
  }
  
  cat("Starting data read of:  \n", paste(filenames, "\n"))
  if(dates != "all"){
    stop(paste0("Error: Date selection '", dates, "' not yet implemented"))
  }
  
  if(!all(file.exists(filenames))){
    stop("Can't find files: ", filenames[!file.exists(filenames)])
  } else {
    cat("All files exist.\n")
  }
  
  mergedfiles <- do.call(rbindlist, list(l = lapply(filenames, fread), use.names = FALSE))
  cat("Merge finished. \n")
  mergedfiles <- unique(mergedfiles)
  cat("Removed duplicates. \n")


    if(save_as_rds){
    saveRDS(mergedfiles, saveloc)
    cat("Saved to ", saveloc)
  }
  
  setwd(currentdir)
  mergedfiles
}

isLikeTimestamp <- function(column){
  test_rows <- sample(column, size = 10, replace = TRUE)
  if(is.character(column)){
    return(sum(is.na((fast_strptime(test_rows, format = "%Y-%m-%d %H:%M:%OS%z")))) < 1)
  } else {
    return(FALSE)
  }
  
}

getSpssData <- function(method = "haven"){
  currentdir <- getwd()
  setwd("Steekproef")
  
  if(method == "haven"){
    library(haven)
    odin2018 <- read_spss(file = "Steekproef AVA. - compleet.sav", user_na = TRUE)
  } else if(method == "foreign"){
    library(foreign)
    odin2018 <- read.spss(file = "ODiN 2018 responsdata.sav", use.value.labels = TRUE, to.data.frame = TRUE)
  }
  setwd(currentdir)
  odin2018
}


