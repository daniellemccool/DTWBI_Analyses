# rollDist <- function(rs, querydata){
#   ismissing          <- querydata$ismissing
#   nmiss              <- querydata$nmiss
#   comparison_segment <- querydata$comparison_segment
# 
# 
#   # btd <- 3
#   btd <- rs[, geodist_vec(prevlon[ismissing][1L],
#                           prevlat[ismissing][1L],
#                           nextlon[ismissing][nmiss],
#                           nextlat[ismissing][nmiss],
#                           paired = TRUE, measure = "haversine")]
#   testdist <- rs[, dist]
#   testdist[ismissing] <- btd
#   refdist <- testdist[comparison_segment]
# 
#   sum(abs(querydata$comparison_dist - refdist))
# }

rollDistCpp <- function(rs, ismissing, nmiss, comparison_segment, comparison_dist){
  prevlat <- rs[[1L]][ismissing][1L]
  prevlon <- rs[[2L]][ismissing][1L]
  nextlat <- rs[[3L]][ismissing][nmiss]
  nextlon <- rs[[4L]][ismissing][nmiss]
  dist <- rs[[5L]]

  btd <- haversine_cpp(prevlat, prevlon, nextlat, nextlon)
  dist[ismissing] <- btd
  refdist <- dist[comparison_segment]

  sum(abs(comparison_dist - refdist))
}

addColsForInterdistance <- function(refs){
  refs[, c("nextlat", "nextlon") := shift(.SD, -1, fill = NA), .SDcols = c("startlat", "startlon"), entity_id]
  refs[, c("prevlat", "prevlon") := shift(.SD, fill = NA), .SDcols = c("endlat", "endlon"), entity_id]
}


imputationCandidates <- function(refs, querydata, time.window, candidate.specificity){
  # Add the distances
  refs[, eucDistStartingHere := frollapply(.SD, n = querydata$comparison_segment_ln,
                                           by.column = FALSE,
                                           simplify = TRUE,
                                           FUN = rollDistCpp,
                                           align = "left",
                                           ismissing = querydata$ismissing,
                                           nmiss = querydata$nmiss,
                                           comparison_segment = querydata$comparison_segment,
                                           comparison_dist = querydata$comparison_dist),
       .SDcols =  c("prevlat", "prevlon", "nextlat", "nextlon", "dist"),
       entity_id]

  # Exclude off times
  refs[, meets_time := abs(time - querydata$t1) <= time.window]

  # Calculate dists
  refs[(meets_time & !is.na(eucDistStartingHere)),
       .(entity_id, .id, eucDistStartingHere)]
}



