# Single-threaded file for parameter simulation study
# Arguments describing the simulation parameters are provided when the R session is invoked
# Usage:
## Rscript single_threaded_sim_script.R <n_imps> <match.buffer> <time.window> <candidate.specificity>
# Example:
## Rscript single_threaded_sim_script.R 10 32 3 5

args <- commandArgs(trailingOnly = TRUE)

# test if all arguments are present
if (length(args)!=4) {
  stop("Please specify in order n_imps match.buffer time.window candidate.specificity.n", call.=FALSE)
}

library(lubridate)
library(data.table, verbose = )
library(hms)
library(Rcpp)

n_imps                <- as.numeric(args[1])
match.buffer          <- as.numeric(args[2])
time.window           <- hours(args[3])
candidate.specificity <- as.numeric(args[4])


source("../shared_code/ref_setup.R")
source("../shared_code/query_setup.R")
Rcpp::sourceCpp("../shared_code/haversine.cpp")

setDTthreads(1)
ddataforsim <- readRDS("Data/ddataforsim-29112022.RDS")
setDT(ddataforsim)
test <- copy(ddataforsim)
test[, starttime := as_datetime(tsstart)]
test[, time := as_hms(starttime)]
test[, dist := dist/1000]
test[, orig_dist := dist]


extradist <- 0.000001


setnames(test, c("tsstartlat", "tsendlat", "tsstartlon", "tsendlon"), c("startlat", "endlat", "startlon", "endlon"))
impdata <- copy(rbindlist(replicate(n_imps, test, simplify = FALSE), idcol = "imp"))
queries <- impdata[imp == 1, .SD[any(is.na(dist))], entity_id]
query_order <- queries[, sum(is.na(dist)), entity_id][order(V1, decreasing = FALSE), entity_id]

for (this_imp in seq_len(n_imps)) {
  for (this_chain in seq_len(3)) {
    for (i in seq_along(query_order)) {
      print(i)

      this_query <-
        copy(impdata[imp == this_imp &
                       entity_id == query_order[i]])

      createIndices(this_query, desired.buffers = match.buffer)
      qd <- queryData(this_query)

      refs <-
        copy(impdata[imp == this_imp &
                       entity_id != query_order[i] &
                       n15 >= qd$comparison_segment_ln,
                     .SD[!any(is.na(dist))],
                     entity_id])
      addColsForInterdistance(refs)

      ic <- imputationCandidates(refs, qd, time.window = time.window, candidate.specificity = candidate.specificity)
      singleic <- ic[, .(.id[eucDistStartingHere == min(eucDistStartingHere)], min(eucDistStartingHere)), entity_id]

      singleic[, prob := (1/((V2 + extradist)^candidate.specificity))/sum(1/((V2 + extradist)^candidate.specificity))]

      selected_candidate_start_id <- sample(singleic$V1, size = 1, prob = singleic$prob)
      refs[, selected := .id == selected_candidate_start_id]
      selected_candidate <- refs[windowAny(selected, 0, qd$comparison_segment_ln - 1)]


      this_query[(missing), dist := selected_candidate[qd$ismissing, dist]]
      this_query[(missing), cand.id := selected_candidate[qd$ismissing, .id]]

      impdata[this_query[(missing)], on = .(imp, entity_id, .id), dist := i.dist]

      rm(selected_candidate_start_id)
      rm(selected_candidate)
    }
  }
  print(paste0("Saving imp: ", this_imp))
  fileloc <- paste("Res/dist", match.buffer,"w", time_length(time.window, "hours"), "c", candidate.specificity, "imp", this_imp, ".RDS", sep = "")
  saveRDS(impdata, fileloc)
}

