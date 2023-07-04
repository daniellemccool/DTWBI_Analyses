gapHours <- function(gaplength, minlead, minlag, hours = 24 * 4) {
  window <- hours - minlead - minlag - gaplength
  if (window < 0) {
    warning(
      paste0(
        "Can't make gap of length ",
        gaplength,
        " with minlead: ",
        minlead,
        " minlag: ",
        minlag,
        " hours: ",
        hours
      )
    )
    return(NA)
  }
  start <- sample(0:window, 1) + minlead + 1
  start:(start + (gaplength - 1))
}

loadSimData <- function(file, folder = data_folder, set = data_set){
  readRDS(paste0(folder, "/d", set, "-data", file, ".RDS"))
}

globalSetImpParams <- function(i, mb, tw, cs, dist = .000001){
  n_imps <<- i
  match.buffer <<- mb
  time.window <<- tw
  candidate.specificity <<- cs
  extradist <<- dist
}
saveSimRes <- function(data, folder = data_folder, set = data_set){
  saveRDS(data,
          paste0(folder,
                 "/data", set,
                 "-missing", gaplength,
                 "-sc", condition,
                 "-",
                 "i", n_imps,
                 "b", match.buffer,
                 "w", time_length(time.window, "hours"),
                 "c", candidate.specificity,
                 ".RDS"))
  
}


