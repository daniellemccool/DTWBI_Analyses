mergeDataDevices <- function(data, devices, wanted.device.columns = c("user_id", "os", "os_version")){
  setkey(data, device_id)
  setkey(devices, id)
  data <- data[devices[, .SD, .SDcols = c("id", wanted.device.columns)]]
  data
}

mergeDataUsers <- function(data, users, wanted.user.columns = "username"){
  setkey(users, id)
  setkey(data, user_id)
  data <- data[users[, .SD, .SDcols = c("id", wanted.user.columns)]]
  data
}

mergeDataSpss <- function(data, spss){
  spss[, username := as.character(username)]
  setkey(spss, username)
  setkey(data, username)
  data <- data[spss]
  data  
}

