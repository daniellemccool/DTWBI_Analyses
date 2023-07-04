library(data.table)
library(lubridate)
library(hms)
library(chron)

# Functions ---------------------------------------------------------------

absWindow <- function(t1, t2, window){

  tdif <- abs(difftime(t2, t1, units = "hours"))
  pmin(tdif, 24 - tdif) <= window
}

sampleTimeWindowedDists <- function(timevec, ref, window, olddist){

  gaplength <- length(timevec)
  t1 <- timevec[1]
  tryCatch({
    start <- ref[absWindow(t1, ref_time, window), sample(ftchunk[maxftchunk - ftchunk > gaplength], 1)]
    stop <- start + gaplength - 1
    ref[ftchunk %between% c(start, stop), dist]
  },
  error = function(cond){
    return(olddist)
  })
}

imputeWindowOnly <- function(impdata, window.size){

  refdata <- copy(impdata[, .SD[!any(is.na(dist))], entity_id, .SDcols = c("imp", "time", "dist", "maxftchunk", "ftchunk")])
  setnames(refdata, old = c("imp", "time", "entity_id"), new = c("ref_imp", "ref_time", "ref_entity_id"))
  impdata[is.na(orig_dist),
          dist := sampleTimeWindowedDists(
            timevec = time,
            ref = refdata[
              fifelse(t1[1] > tN[1],
                      sum(!data.table::between(ref_time, tN[1], t1[1])),
                      sum(data.table::between(ref_time, t1[1], tN[1]))) > nmiss[1] &
                ref_imp == imp, .SD, ref_entity_id][ ref_entity_id %in% sample(unique(ref_entity_id), 1)],
            window = window.size,
            olddist = dist), .(entity_id, imp)]
}

# 4 missing ---------------------------------------------------------------

ddataforsim <- readRDS("Res/4missing-i1b32w1c20.RDS")
setDT(ddataforsim)
setDTthreads(1)
ddataforsim[is.na(orig_dist), dist := NA]

ddataforsim[, ftchunk := 0:(.N - 1), entity_id]
ddataforsim[, maxftchunk := max(ftchunk), entity_id]
ddataforsim[, starttime := as_datetime(tsstart)]
ddataforsim[, endtime := as_datetime(tsend)]
ddataforsim[, time := as.ITime(starttime)] # changed
ddataforsim[, nmiss := sum(is.na(dist)), entity_id]
ddataforsim[is.na(dist), t1 := time[1], entity_id]
ddataforsim[is.na(dist), tN := time[.N], entity_id]



# 1 hour window -----------------------------------------------------------

# Settings ----------------------------------------------------------------

n_imps <- 10
window.size <- lubridate::hours(1)

# Run ---------------------------------------------------------------------

impdata <-
  copy(rbindlist(replicate(n_imps, ddataforsim, simplify = FALSE), idcol = "imp"))



escape_counter <- 1
while (any(is.na(impdata$dist)) & escape_counter < 20) {
  imputeWindowOnly(impdata = impdata, window.size = window.size)
  escape_counter <- escape_counter + 1
}


# Chain 2 more times ------------------------------------------------------

escape_counter <- 1
while ((any(is.na(impdata$dist)) & escape_counter < 20) | escape_counter == 1) {
  imputeWindowOnly(impdata = impdata, window.size = window.size)
  escape_counter <- escape_counter + 1
}

escape_counter <- 1
while ((any(is.na(impdata$dist)) & escape_counter < 20) | escape_counter == 1) {
  imputeWindowOnly(impdata = impdata, window.size = window.size)
  escape_counter <- escape_counter + 1
}

saveRDS(impdata, "Res/4missing-dist_twiw1.RDS")

# 12 missing --------------------------------------------------------------

ddataforsim <- readRDS("Res/12missing-i1b32w1c20.RDS")

setDT(ddataforsim)
ddataforsim[is.na(orig_dist), dist := NA]

ddataforsim[, ftchunk := 0:(.N - 1), entity_id]
ddataforsim[, maxftchunk := max(ftchunk), entity_id]
ddataforsim[, starttime := as_datetime(tsstart)]
ddataforsim[, endtime := as_datetime(tsend)]
ddataforsim[, time := as.ITime(starttime)] # changed
ddataforsim[, nmiss := sum(is.na(dist)), entity_id]
ddataforsim[is.na(dist), t1 := time[1], entity_id]
ddataforsim[is.na(dist), tN := time[.N], entity_id]

# 1 hour window -----------------------------------------------------------

# Settings ----------------------------------------------------------------

n_imps <- 10
window.size <- lubridate::hours(1)

# Run ---------------------------------------------------------------------

impdata <-
  copy(rbindlist(replicate(n_imps, ddataforsim, simplify = FALSE), idcol = "imp"))



escape_counter <- 1
while (any(is.na(impdata$dist)) & escape_counter < 20) {
  imputeWindowOnly(impdata = impdata, window.size = window.size)
  escape_counter <- escape_counter + 1
}

# Chain 2 more times ------------------------------------------------------

escape_counter <- 1
while ((any(is.na(impdata$dist)) & escape_counter < 20) | escape_counter == 1) {
  imputeWindowOnly(impdata = impdata, window.size = window.size)
  escape_counter <- escape_counter + 1
}

escape_counter <- 1
while ((any(is.na(impdata$dist)) & escape_counter < 20) | escape_counter == 1) {
  imputeWindowOnly(impdata = impdata, window.size = window.size)
  escape_counter <- escape_counter + 1
}

saveRDS(impdata, "Res/12missing-dist_twiw1.RDS")

# 24 missing --------------------------------------------------------------

ddataforsim <- readRDS("Res/24missing-i1b32w1c20.RDS")
ddataforsim <- ddataforsim[imp == 1]

setDT(ddataforsim)
ddataforsim[is.na(orig_dist), dist := NA]

ddataforsim[, ftchunk := 0:(.N - 1), entity_id]
ddataforsim[, maxftchunk := max(ftchunk), entity_id]
ddataforsim[, starttime := as_datetime(tsstart)]
ddataforsim[, endtime := as_datetime(tsend)]
ddataforsim[, time := as.ITime(starttime)] # changed
ddataforsim[, nmiss := sum(is.na(dist)), entity_id]
ddataforsim[is.na(dist), t1 := time[1], entity_id]
ddataforsim[is.na(dist), tN := time[.N], entity_id]

# 1 hour window -----------------------------------------------------------

# Settings ----------------------------------------------------------------

n_imps <- 10
window.size <- lubridate::hours(1)

# Run ---------------------------------------------------------------------

impdata <-
  copy(rbindlist(replicate(n_imps, ddataforsim, simplify = FALSE), idcol = "imp"))



escape_counter <- 1
while (any(is.na(impdata$dist)) & escape_counter < 20) {
  imputeWindowOnly(impdata = impdata, window.size = window.size)
  escape_counter <- escape_counter + 1
}

# Chain 2 more times ------------------------------------------------------

escape_counter <- 1
while ((any(is.na(impdata$dist)) & escape_counter < 20) | escape_counter == 1) {
  imputeWindowOnly(impdata = impdata, window.size = window.size)
  escape_counter <- escape_counter + 1
}

escape_counter <- 1
while ((any(is.na(impdata$dist)) & escape_counter < 20) | escape_counter == 1) {
  imputeWindowOnly(impdata = impdata, window.size = window.size)
  escape_counter <- escape_counter + 1
}

saveRDS(impdata, "Res/24missing-dist_twiw1.RDS")

# 40 missing --------------------------------------------------------------
ddataforsim <- readRDS("Res/40missing-i1b32w1c20.RDS")
ddataforsim <- ddataforsim[imp == 1]

setDT(ddataforsim)

ddataforsim[is.na(orig_dist), dist := NA]

ddataforsim[, ftchunk := 0:(.N - 1), entity_id]
ddataforsim[, maxftchunk := max(ftchunk), entity_id]
ddataforsim[, starttime := as_datetime(tsstart)]
ddataforsim[, endtime := as_datetime(tsend)]
ddataforsim[, time := as.ITime(starttime)] # changed
ddataforsim[, nmiss := sum(is.na(dist)), entity_id]
ddataforsim[is.na(dist), t1 := time[1], entity_id]
ddataforsim[is.na(dist), tN := time[.N], entity_id]

# 1 hour window -----------------------------------------------------------

# Settings ----------------------------------------------------------------

n_imps <- 10
window.size <- lubridate::hours(1)

# Run ---------------------------------------------------------------------

impdata <-
  copy(rbindlist(replicate(n_imps, ddataforsim, simplify = FALSE), idcol = "imp"))


escape_counter <- 1
while (any(is.na(impdata$dist)) & escape_counter < 20) {
  imputeWindowOnly(impdata = impdata, window.size = window.size)
  escape_counter <- escape_counter + 1
}

# Chain 2 more times ------------------------------------------------------

escape_counter <- 1
while ((any(is.na(impdata$dist)) & escape_counter < 20) | escape_counter == 1) {
  imputeWindowOnly(impdata = impdata, window.size = window.size)
  escape_counter <- escape_counter + 1
}

escape_counter <- 1
while ((any(is.na(impdata$dist)) & escape_counter < 20) | escape_counter == 1) {
  imputeWindowOnly(impdata = impdata, window.size = window.size)
  escape_counter <- escape_counter + 1
}

saveRDS(impdata, "Res/40missing-dist_twiw1.RDS")

# 48 missing --------------------------------------------------------------
ddataforsim <- readRDS("Res/48missing-i1b32w1c20.RDS")
ddataforsim <- ddataforsim[imp == 1]

setDT(ddataforsim)

ddataforsim[is.na(orig_dist), dist := NA]

ddataforsim[, ftchunk := 0:(.N - 1), entity_id]
ddataforsim[, maxftchunk := max(ftchunk), entity_id]
ddataforsim[, starttime := as_datetime(tsstart)]
ddataforsim[, endtime := as_datetime(tsend)]
ddataforsim[, time := as.ITime(starttime)] # changed
ddataforsim[, nmiss := sum(is.na(dist)), entity_id]
ddataforsim[is.na(dist), t1 := time[1], entity_id]
ddataforsim[is.na(dist), tN := time[.N], entity_id]

# 1 hour window -----------------------------------------------------------

# Settings ----------------------------------------------------------------

n_imps <- 10
window.size <- lubridate::hours(1)

# Run ---------------------------------------------------------------------

impdata <-
  copy(rbindlist(replicate(n_imps, ddataforsim, simplify = FALSE), idcol = "imp"))


escape_counter <- 1
while (any(is.na(impdata$dist)) & escape_counter < 20) {
  imputeWindowOnly(impdata = impdata, window.size = window.size)
  escape_counter <- escape_counter + 1
}

# Chain 2 more times ------------------------------------------------------

escape_counter <- 1
while ((any(is.na(impdata$dist)) & escape_counter < 20) | escape_counter == 1) {
  imputeWindowOnly(impdata = impdata, window.size = window.size)
  escape_counter <- escape_counter + 1
}

escape_counter <- 1
while ((any(is.na(impdata$dist)) & escape_counter < 20) | escape_counter == 1) {
  imputeWindowOnly(impdata = impdata, window.size = window.size)
  escape_counter <- escape_counter + 1
}

saveRDS(impdata, "Res/48missing-dist_twiw1.RDS")
