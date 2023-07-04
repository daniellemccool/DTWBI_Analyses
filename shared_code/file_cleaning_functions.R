keepParticipants <- function(data){
  if(is.null(data$username)){
    cat("First merge with devices and users")
  } else{
    data <- data[nchar(username) == 4]
    data <- data[!username == 3141]
  }
  data
}


createTimestamps <- function(data, timestamp_col){
  print("Depricated, fread now reads dates as POSIXct")
}

