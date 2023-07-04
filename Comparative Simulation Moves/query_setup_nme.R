trueBuffersNme <- function(query_nme, desired.buffer) {

  missing_idx_range   <- range(which(is.na(query_nme)))
  query_length  <- length(query_nme)
  forebuffer    <- min(missing_idx_range[1] - 1, desired.buffer)
  endbuffer     <- min(query_length - missing_idx_range[2], desired.buffer)
  list(forebuffer, endbuffer)
}

buffersAreValid <- function(forebuffer, endbuffer){
  forebuffer != 0 & endbuffer != 0
}

windowAny <- function(boolvector, lead, lag) {
  rowSums(Reduce("cbind", shift(boolvector, (lead[1]*-1):lag[1], fill = FALSE))) != 0
}


createIndicesNme <- function(query, desired.buffers, move.threshold){
  query[, missing := is.na(orig_nme)]
  query[, c("forebuffer", "endbuffer") := trueBuffersNme(orig_nme, desired.buffers)]
  query[, missing_with_buffer := windowAny(missing, forebuffer[1], endbuffer[1])]
  query[, first_missing := !duplicated(missing) & missing]
  query[, c("nextlat", "nextlon") := data.table::shift(.SD, -1), .SDcols = c("startlat", "startlon")]
  query[, c("prevlat", "prevlon") := data.table::shift(.SD), .SDcols = c("endlat", "endlon")]
  query[(missing), btd := haversine_cpp(prevlat[1], prevlon[1], nextlat[.N], nextlon[.N])]

  query[, comparison_segment := (missing_with_buffer & !missing) | first_missing]
  query[, comparison_nme := orig_nme]
  query[is.na(orig_nme), comparison_nme := as.integer(btd > move.threshold)]
}

queryDataNme <- function(query){
  list(
    ismissing          = query[(missing_with_buffer), missing],
    nmiss              = query[(missing_with_buffer), sum(missing)],
    comparison_segment    = query[(missing_with_buffer), comparison_segment],
    comparison_segment_ln = query[(missing_with_buffer), .N],
    t1                    = query[(comparison_segment), time[1]],
    comparison_nme       = query[(comparison_segment), comparison_nme]
  )
}

# createIndices(query)
# qd <- queryData(query)

