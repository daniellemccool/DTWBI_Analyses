library(lubridate)
library(data.table)
library(hms)
library(Rcpp)

USE_SERVER <- "alp"

path.from.server <-
  function(path_from_R_code,
           server = USE_SERVER) {
    alp.prefix <-
      ""
    ds.prefix <-
      ""
    selected.prefix <- switch (server,
                               "alp" = alp.prefix,
                               "ds"  = ds.prefix)
    file.path(selected.prefix, path_from_R_code)
  }



# Setup -------------------------------------------------------------------


## Sim settings ------------------------------------------------------------

data_set    <- 8L
gaplength   <- 12L
this_seed <- 2323232L


## Imputation parameter settings -------------------------------------------

globalSetImpParamsHI <-
  function()
    globalSetImpParams(i = 3L,
                       mb = 32L,
                       tw = hours(12),
                       cs = 5L)
globalSetImpParamsLO <-
  function()
    globalSetImpParams(i = 10L,
                       mb = 4L,
                       tw = hours(3),
                       cs = 4L)
globalSetImpParamsSING <-
  function()
    globalSetImpParams(i = 1L,
                       mb = 32L,
                       tw = hours(1),
                       cs = 20L)


## Folder settings ---------------------------------------------------------

sim_folder  <-
  "ExtraSetsDrilldown"
data_folder <-
  path.from.server(file.path(sim_folder, paste0("Data ", data_set)))


## Source simulation functions ---------------------------------------------

source(path.from.server("../../shared_code/ref_setup.R"))
source(path.from.server("../../shared_code/query_setup.R"))
Rcpp::sourceCpp(path.from.server("../../shared_code/haversine.cpp"))
source(path.from.server(file.path(sim_folder, "../simulation_functions.R")))

setDTthreads(percent = 80)


# Data --------------------------------------------------------------------
## Load Data ---------------------------------------------------------------

data1 <- loadSimData(1)
data2 <- loadSimData(2)
data3 <- loadSimData(3)
data4 <- loadSimData(4)

## Separate out queries ----------------------------------------------------

with_missing <- copy(data1[n_sets >= 4 & order == 1])
missing_eids <- with_missing[, unique(entity_id)]

t1 <- data1[!entity_id %in% missing_eids]
t2 <- data2[!entity_id %in% missing_eids]
t3 <- data3[!entity_id %in% missing_eids]
t4 <- data4[!entity_id %in% missing_eids]

## Create missingness ------------------------------------------------------

set.seed(this_seed)

for (i in seq_along(missing_eids)) {
  with_missing[entity_id == missing_eids[i], missing := .I %in% gapHours(gaplength, 2, 2, n15[1])]
}

with_missing[missing == TRUE, dist := NA]
with_missing[, orig_dist := dist]
query_order <- missing_eids


# Simulation --------------------------------------------------------------

## Condition 1 (No extra sets)---------------------------------------------

condition <- 1
test <-
  rbindlist(list(with_missing, t1), fill = TRUE, use.names = TRUE)


### HI parameters -----------------------------------------------------------

globalSetImpParamsHI()

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
        imputationCandidates(
          refs = refs,
          querydata = qd,
          time.window = time.window,
          candidate.specificity = candidate.specificity
        )
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

saveSimRes(impdata)


### LO parameters ---------------------------------------------------------

globalSetImpParamsLO()

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

saveSimRes(impdata)


### DTWBI parameters --------------------------------------------------------

globalSetImpParamsSING()

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

saveSimRes(impdata)


## Condition 2 (One extra set) ---------------------------------------------

condition <- 2
test <-
  rbindlist(list(with_missing, t2), fill = TRUE, use.names = TRUE)


### HI parameters -----------------------------------------------------------

globalSetImpParamsHI()

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
        imputationCandidates(
          refs = refs,
          querydata = qd,
          time.window = time.window,
          candidate.specificity = candidate.specificity
        )
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

saveSimRes(impdata)


### LO parameters -----------------------------------------------------------

globalSetImpParamsLO()

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

saveSimRes(impdata)


### DTWBI parameters --------------------------------------------------------

globalSetImpParamsSING()

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

saveSimRes(impdata)


## Condition 3 (Two extra sets) ---------------------------------------------

condition <- 3
test <-
  rbindlist(list(with_missing, t3), fill = TRUE, use.names = TRUE)


### HI parameters -----------------------------------------------------------

globalSetImpParamsHI()

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
        imputationCandidates(
          refs = refs,
          querydata = qd,
          time.window = time.window,
          candidate.specificity = candidate.specificity
        )
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

saveSimRes(impdata)


### LO parameters -----------------------------------------------------------

globalSetImpParamsLO()

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

saveSimRes(impdata)


### DTWBI parameters --------------------------------------------------------

globalSetImpParamsSING()

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

saveSimRes(impdata)


## Condition 4 (Three extra sets) ------------------------------------------

condition <- 4
test <-
  rbindlist(list(with_missing, t4), fill = TRUE, use.names = TRUE)


### HI parameters -----------------------------------------------------------

globalSetImpParamsHI()

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
        imputationCandidates(
          refs = refs,
          querydata = qd,
          time.window = time.window,
          candidate.specificity = candidate.specificity
        )
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

saveSimRes(impdata)


### LO parameters ---------------------------------------------------------

globalSetImpParamsLO()

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

saveSimRes(impdata)


### DTWBI parameters --------------------------------------------------------

globalSetImpParamsSING()

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

saveSimRes(impdata)
