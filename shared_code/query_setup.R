trueBuffers <- function(query_dist, desired.buffer) {

  missing_idx_range   <- range(which(is.na(query_dist)))
  query_length  <- length(query_dist)
  forebuffer    <- min(missing_idx_range[1] - 1, desired.buffer)
  endbuffer     <- min(query_length - missing_idx_range[2], desired.buffer)
  list(forebuffer, endbuffer)
}

buffersAreValid <- function(forebuffer, endbuffer){
  forebuffer != 0 & endbuffer != 0
}

windowAny <- function(boolvector, lead, lag) {
  rowSums(Reduce("cbind", data.table::shift(boolvector, (lead[1]*-1):lag[1], fill = FALSE))) != 0
}


createIndices <- function(query, desired.buffers){
  query[, missing := is.na(orig_dist)]
  query[, c("forebuffer", "endbuffer") := trueBuffers(orig_dist, desired.buffers)]
  query[, missing_with_buffer := windowAny(missing, forebuffer[1], endbuffer[1])]
  query[, first_missing := !duplicated(missing) & missing]
  query[, c("nextlat", "nextlon") := shift(.SD, -1), .SDcols = c("startlat", "startlon")]
  query[, c("prevlat", "prevlon") := shift(.SD), .SDcols = c("endlat", "endlon")]
  query[(missing), btd := haversine_cpp(prevlat[1], prevlon[1], nextlat[.N], nextlon[.N])]

  query[, comparison_segment := (missing_with_buffer & !missing) | first_missing]
  query[, comparison_dist := orig_dist]
  query[is.na(orig_dist), comparison_dist := btd]
  # query[, t1 := time[comparison_segment][1]]
  # query[(comparison_segment), comparison_dist]
  # later remove unnecessary columns
}

queryData <- function(query){
  list(
    ismissing          = query[(missing_with_buffer), missing],
    nmiss              = query[(missing_with_buffer), sum(missing)],
    comparison_segment    = query[(missing_with_buffer), comparison_segment],
    comparison_segment_ln = query[(missing_with_buffer), .N],
    t1                    = query[(comparison_segment), time[1]],
    comparison_dist       = query[(comparison_segment), comparison_dist]
  )
}


