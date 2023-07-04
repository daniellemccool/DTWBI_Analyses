library(lubridate)
library(data.table)
library(hms)
library(Rcpp)

source(
  "../shared_code/ref_setup.R"
)
source(
  "../shared_code/query_setup.R"
)
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

# Missingness parameters --------------------------------------------------

nmiss <- 100
gaptype <- 1 # everyone the same
gaplength <- 12

test <- copy(ddata)
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

test[missing == TRUE, dist := NA]
test[, orig_dist := dist]

# Imputation parameters ---------------------------------------------------
# DTWBMI-HI high, mb 8, tw 3, 3 imps
n_imps <- 3
match.buffer <- 32
time.window <- hours(12)
candidate.specificity <- 5
extradist <- 0.000001


impdata <-
  copy(rbindlist(replicate(n_imps, test, simplify = FALSE), idcol = "imp"))
queries <- impdata[imp == 1, .SD[any(is.na(dist))], entity_id]

if (gaptype == 0) {
  query_order <-
    queries[, sum(is.na(dist)), entity_id][order(V1, decreasing = FALSE), entity_id]
} else query_order <- tobemissing

for (this_imp in 1:n_imps) {
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
      
      ic <-
        imputationCandidates(refs,
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
      
      
      this_query[(missing), dist := selected_candidate[qd$ismissing, dist]]
      this_query[(missing), cand.id := selected_candidate[qd$ismissing, .id]]
      
      impdata[this_query[(missing)], on = .(imp, entity_id, .id), dist := i.dist]
      
      rm(selected_candidate_start_id)
      rm(selected_candidate)
    }
  }
  print(paste0("Finished imp: ", this_imp))
}

saveRDS(impdata, "Res/12missing-i3b32w12c5.RDS")


# LO parameters ---------------------------------------------------------
# med, 1hr, 1 hr, unrestricted, 3 imps
n_imps <- 10
match.buffer <- 4
time.window <- hours(3)
candidate.specificity <- 4
extradist <- 0.000001


impdata <-
  copy(rbindlist(replicate(n_imps, test, simplify = FALSE), idcol = "imp"))
queries <- impdata[imp == 1, .SD[any(is.na(dist))], entity_id]


for (this_imp in 1:n_imps) {
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
      
      ic <-
        imputationCandidates(refs,
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
      
      
      this_query[(missing), dist := selected_candidate[qd$ismissing, dist]]
      this_query[(missing), cand.id := selected_candidate[qd$ismissing, .id]]
      
      impdata[this_query[(missing)], on = .(imp, entity_id, .id), dist := i.dist]
      
      rm(selected_candidate_start_id)
      rm(selected_candidate)
    }
  }
  print(paste0("Finished imp: ", this_imp))
}

saveRDS(impdata, "Res/12missing-i10b4w3c4.RDS")


# DTWBI parameters --------------------------------------------------------

# always (20), 8hr, 3 hrs, 1 imp
n_imps <- 1
match.buffer <- 32
time.window <- hours(1)
candidate.specificity <- 20
extradist <- 0.000001


impdata <-
  copy(rbindlist(replicate(n_imps, test, simplify = FALSE), idcol = "imp"))
queries <- impdata[imp == 1, .SD[any(is.na(dist))], entity_id]


for (this_imp in 1:n_imps) {
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
      
      ic <-
        imputationCandidates(refs,
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
      
      
      this_query[(missing), dist := selected_candidate[qd$ismissing, dist]]
      this_query[(missing), cand.id := selected_candidate[qd$ismissing, .id]]
      
      impdata[this_query[(missing)], on = .(imp, entity_id, .id), dist := i.dist]
      
      rm(selected_candidate_start_id)
      rm(selected_candidate)
    }
  }
  print(paste0("Finished imp: ", this_imp))
}

saveRDS(impdata, "Res/12missing-i1b32w1c20.RDS")
