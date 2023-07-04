library(lubridate)
library(data.table)
library(hms)
library(Rcpp)

source("ref_setup_nme.R")
source("query_setup_nme.R")
Rcpp::sourceCpp(
  "../shared_code/haversine.cpp"
)

# setDTthreads(percent = 80)
setDTthreads(percent = 80)
ddata <-
  readRDS(
    "Data/nonvarmiss-15022023.RDS"
  )

test <- copy(ddata)
test[, n_move_events_bu := n_move_events]


# Simulation parameters
set.seed(1139327692)

# Missingness parameters --------------------------------------------------

nmiss <- 100
gaptype <- 1 # everyone the same
gaplength <- 24

test <- copy(ddata)
test[, n_move_events_bu := n_move_events]

eids <- test[, unique(entity_id)]
tobemissing <- sample(eids, nmiss)

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

if (gaptype == 0) {
  nmiss <- sample(2:gaplength, nmiss, replace = TRUE)
  for (i in seq_along(tobemissing)) {
    test[entity_id == tobemissing[i], missing := .I %in% gapHours(nmiss[i], 2, 2, n15[1])]
  }
} else {
  for (i in seq_along(tobemissing)) {
    test[entity_id == tobemissing[i], missing := .I %in% gapHours(gaplength, 2, 2, n15[1])]
  }
}

test[missing == TRUE, n_move_events := NA]
test[, orig_nme := n_move_events]

# Imputation parameters ---------------------------------------------------
# DTWBMI-HI high, mb 8, tw 3, 3 imps
n_imps <- 3
match.buffer <- 32
time.window <- hours(12)
candidate.specificity <- 5
extradist <- 0.000001


test[, dist := NA]
impdata <-
  copy(rbindlist(replicate(n_imps, test, simplify = FALSE), idcol = "imp"))
queries <- impdata[imp == 1, .SD[any(is.na(n_move_events))], entity_id]

if (gaptype == 0) {
  query_order <-
    queries[, sum(is.na(dist)), entity_id][order(V1, decreasing = FALSE), entity_id]
} else query_order <- tobemissing

# this_imp <- 1
# i <- 1
for (this_imp in 1:n_imps) {
  for (this_chain in seq_len(3)) {
    for (i in seq_along(query_order)) {
      print(i)
      
      this_query <-
        copy(impdata[imp == this_imp &
                       entity_id == query_order[i]])
      
      createIndicesNme(this_query, desired.buffers = match.buffer, move.threshold = .5)
      qd <- queryDataNme(this_query)
      
      refs <-
        copy(impdata[imp == this_imp &
                       entity_id != query_order[i] &
                       n15 >= qd$comparison_segment_ln,
                     .SD[!any(is.na(n_move_events))],
                     entity_id])
      addColsForInterdistance(refs)
      
      ic <-
        imputationCandidatesNme(refs,
                                qd,
                                time.window = time.window,
                                candidate.specificity = candidate.specificity)
      singleic <-
        ic[, .(.id[eucDistStartingHere == min(eucDistStartingHere)], min(eucDistStartingHere)), entity_id]
      
      singleic[, prob := (1 / ((V2 + extradist) ^ candidate.specificity)) /
                 sum(1 / ((V2 + extradist) ^ candidate.specificity))]
      
      selected_candidate_start_id <-
        sample(singleic$V1, size = 1, prob = singleic$prob)
      refs[, selected := .id == selected_candidate_start_id]
      selected_candidate <-
        refs[windowAny(selected, 0, qd$comparison_segment_ln - 1)]
      
      
      this_query[(missing), n_move_events := selected_candidate[qd$ismissing, n_move_events]]
      this_query[(missing), cand.id := selected_candidate[qd$ismissing, .id]]
      
      impdata[this_query[(missing)], on = .(imp, entity_id, .id), n_move_events := i.n_move_events]
      
      rm(selected_candidate_start_id)
      rm(selected_candidate)
    }
  }
  print(paste0("Finished imp: ", this_imp))
}

saveRDS(impdata, "Res/24missing-moves-i3b32w12c5.RDS")

# LO parameters ---------------------------------------------------------
# med, 1hr, 1 hr, unrestricted, 3 imps
n_imps <- 10
match.buffer <- 4
time.window <- hours(3)
candidate.specificity <- 4
extradist <- 0.000001


test[, dist := NA]
impdata <-
  copy(rbindlist(replicate(n_imps, test, simplify = FALSE), idcol = "imp"))
queries <- impdata[imp == 1, .SD[any(is.na(n_move_events))], entity_id]

if (gaptype == 0) {
  query_order <-
    queries[, sum(is.na(dist)), entity_id][order(V1, decreasing = FALSE), entity_id]
} else query_order <- tobemissing

# this_imp <- 1
# i <- 1
for (this_imp in 1:n_imps) {
  for (this_chain in seq_len(3)) {
    for (i in seq_along(query_order)) {
      print(i)
      
      this_query <-
        copy(impdata[imp == this_imp &
                       entity_id == query_order[i]])
      
      createIndicesNme(this_query, desired.buffers = match.buffer, move.threshold = .5)
      qd <- queryDataNme(this_query)
      
      refs <-
        copy(impdata[imp == this_imp &
                       entity_id != query_order[i] &
                       n15 >= qd$comparison_segment_ln,
                     .SD[!any(is.na(n_move_events))],
                     entity_id])
      addColsForInterdistance(refs)
      
      ic <-
        imputationCandidatesNme(refs,
                                qd,
                                time.window = time.window,
                                candidate.specificity = candidate.specificity)
      singleic <-
        ic[, .(.id[eucDistStartingHere == min(eucDistStartingHere)], min(eucDistStartingHere)), entity_id]
      
      singleic[, prob := (1 / ((V2 + extradist) ^ candidate.specificity)) /
                 sum(1 / ((V2 + extradist) ^ candidate.specificity))]
      
      selected_candidate_start_id <-
        sample(singleic$V1, size = 1, prob = singleic$prob)
      refs[, selected := .id == selected_candidate_start_id]
      selected_candidate <-
        refs[windowAny(selected, 0, qd$comparison_segment_ln - 1)]
      
      
      this_query[(missing), n_move_events := selected_candidate[qd$ismissing, n_move_events]]
      this_query[(missing), cand.id := selected_candidate[qd$ismissing, .id]]
      
      impdata[this_query[(missing)], on = .(imp, entity_id, .id), n_move_events := i.n_move_events]
      
      rm(selected_candidate_start_id)
      rm(selected_candidate)
    }
  }
  print(paste0("Finished imp: ", this_imp))
}
saveRDS(impdata, "Res/24missing-moves-i10b4w3c4.RDS")


# DTWBI parameters --------------------------------------------------------

# always (20), 8hr, 3 hrs, 1 imp
n_imps <- 1
match.buffer <- 32
time.window <- hours(1)
candidate.specificity <- 20
extradist <- 0.000001


impdata <-
  copy(rbindlist(replicate(n_imps, test, simplify = FALSE), idcol = "imp"))
queries <- impdata[imp == 1, .SD[any(is.na(n_move_events))], entity_id]

if (gaptype == 0) {
  query_order <-
    queries[, sum(is.na(dist)), entity_id][order(V1, decreasing = FALSE), entity_id]
} else query_order <- tobemissing

# this_imp <- 1
# i <- 1
for (this_imp in 1:n_imps) {
  for (this_chain in seq_len(3)) {
    for (i in seq_along(query_order)) {
      print(i)
      
      this_query <-
        copy(impdata[imp == this_imp &
                       entity_id == query_order[i]])
      
      createIndicesNme(this_query, desired.buffers = match.buffer, move.threshold = .5)
      qd <- queryDataNme(this_query)
      
      refs <-
        copy(impdata[imp == this_imp &
                       entity_id != query_order[i] &
                       n15 >= qd$comparison_segment_ln,
                     .SD[!any(is.na(n_move_events))],
                     entity_id])
      addColsForInterdistance(refs)
      
      ic <-
        imputationCandidatesNme(refs,
                                qd,
                                time.window = time.window,
                                candidate.specificity = candidate.specificity)
      singleic <-
        ic[, .(.id[eucDistStartingHere == min(eucDistStartingHere)], min(eucDistStartingHere)), entity_id]
      
      singleic[, prob := (1 / ((V2 + extradist) ^ candidate.specificity)) /
                 sum(1 / ((V2 + extradist) ^ candidate.specificity))]
      
      selected_candidate_start_id <-
        sample(singleic$V1, size = 1, prob = singleic$prob)
      refs[, selected := .id == selected_candidate_start_id]
      selected_candidate <-
        refs[windowAny(selected, 0, qd$comparison_segment_ln - 1)]
      
      
      this_query[(missing), n_move_events := selected_candidate[qd$ismissing, n_move_events]]
      this_query[(missing), cand.id := selected_candidate[qd$ismissing, .id]]
      
      impdata[this_query[(missing)], on = .(imp, entity_id, .id), n_move_events := i.n_move_events]
      
      rm(selected_candidate_start_id)
      rm(selected_candidate)
    }
  }
  print(paste0("Finished imp: ", this_imp))
}

saveRDS(impdata, "Res/24missing-moves-i1b32w1c20.RDS")

