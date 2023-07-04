
rollDistCppNme <- function(rs, ismissing, nmiss, comparison_segment, comparison_nme){
  prevlat <- rs[[1L]][ismissing][1L]
  prevlon <- rs[[2L]][ismissing][1L]
  nextlat <- rs[[3L]][ismissing][nmiss]
  nextlon <- rs[[4L]][ismissing][nmiss]
  n_move_events <- rs[[5L]]

  btd <- haversine_cpp(prevlat, prevlon, nextlat, nextlon)
  n_move_events[ismissing] <- btd > .5 ## This is move threshold
  refnme <- n_move_events[comparison_segment]

  sum(abs(comparison_nme - refnme))
}

addColsForInterdistance <- function(refs){
  refs[, c("nextlat", "nextlon") := shift(.SD, -1, fill = NA), .SDcols = c("startlat", "startlon"), entity_id]
  refs[, c("prevlat", "prevlon") := shift(.SD, fill = NA), .SDcols = c("endlat", "endlon"), entity_id]
}


imputationCandidatesNme <- function(refs, querydata, time.window, candidate.specificity){
  # Add the distances
  refs[, eucDistStartingHere := frollapply(.SD, n = querydata$comparison_segment_ln,
                                           by.column = FALSE,
                                           simplify = TRUE,
                                           FUN = rollDistCppNme,
                                           align = "left",
                                           ismissing = querydata$ismissing,
                                           nmiss = querydata$nmiss,
                                           comparison_segment = querydata$comparison_segment,
                                           comparison_nme = querydata$comparison_nme),
       .SDcols =  c("prevlat", "prevlon", "nextlat", "nextlon", "n_move_events"),
       entity_id]

  # Exclude off times
  refs[, meets_time := abs(time - querydata$t1) <= time.window]

  # Calculate probabilities (calculates for all matches instead of one per person, so removed)
  # refs[(meets_time & !is.na(eucDistStartingHere)),
  #      .(entity_id, .id, eucDistStartingHere, "prob" = (1/(eucDistStartingHere^candidate.specificity))/sum(1/(eucDistStartingHere^candidate.specificity)))]

  # Calculate dists
  refs[(meets_time & !is.na(eucDistStartingHere)),
       .(entity_id, .id, eucDistStartingHere)]
}
