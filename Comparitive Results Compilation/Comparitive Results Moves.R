
# Functions ---------------------------------------------------------------
percentTravelPeriodsMissed <- function(true, imp, min_dist){
  if (sum(true >= min_dist) == 0) return(0)
  max((sum(true >= min_dist) - sum(imp >= min_dist)), 0)/sum(true >= min_dist) * 100
}

percentTravelPeriodsOverestimated <- function(true, imp, min_dist){
  if (sum(imp >= min_dist) == 0) return(0)
  max((sum(imp >= min_dist) - sum(true >= min_dist)), 0)/sum(imp >= min_dist) * 100
}


impTravelPeriodAccuracy <- function(true, imp, min_dist){
  (1 - abs(sum(true >= min_dist) - sum(imp >= min_dist))/length(true)) * 100
}

distanceOverestimated <- function(true, imp){
  max(sum(imp) - sum(true), 0)
}

distanceUnderestimated <- function(true, imp){
  max(sum(true) - sum(imp), 0)
}



# DTWBI-Burst -------------------------------------------------------------

DTWBMI_HI <- rbindlist(
  list(readRDS(file = "Res/4missing-moves-i3b32w12c5.RDS"),
       readRDS(file = "Res/12missing-moves-i3b32w12c5.RDS"),
       readRDS(file = "Res/24missing-moves-i3b32w12c5.RDS"),
       readRDS(file = "Res/40missing-moves-i3b32w12c5.RDS"),
       readRDS(file = "Res/48missing-moves-i3b32w12c5.RDS")
       ), idcol =  "file", use.names = TRUE, fill = TRUE)
DTWBMI_HI[, nmiss := sum(is.na(orig_nme)), .(file, entity_id, imp)]

DTWBMI_HI[, c("devid", "set") := tstrsplit(entity_id, "-")]
DTWBMI_HI[, n_sets := uniqueN(entity_id), .(file, devid)]
DTWBMI_HI[, night := FALSE]
DTWBMI_HI[hour(starttime) >= 21 | hour(starttime) <= 4, night := TRUE]
DTWBMI_HI[is.na(orig_nme), perc_night := sum(night)/.N, .(file, entity_id, imp)]
DTWBMI_HI[, perc_night := perc_night[is.na(orig_nme)][1], .(file, entity_id)]


# Join moves --------------------------------------------------------------
DTWBMI_HI[, no_imp_moves := all(n_move_events == 0), .(entity_id, file, imp)]
DTWBMI_HI[(no_imp_moves), impnme := 0]


DTWBMI_HI[, no_true_moves := all(n_move_events_bu == 0), .(entity_id, file, imp)]
DTWBMI_HI[(no_true_moves), truenme := 0]



DTWBMI_HI[, imp_act_id := rleid(n_move_events), .(entity_id, file, imp)]
DTWBMI_HI[, true_act_id := rleid(n_move_events_bu), .(entity_id, file, imp)]

DTWBMI_HI[n_move_events > 0, impnme := cumsum(n_move_events[1]), .(entity_id, file, imp, imp_act_id)]

DTWBMI_HI[n_move_events_bu > 0, truenme := cumsum(n_move_events_bu[1]), .(entity_id, file, imp, true_act_id)]

DTWBMI_HI[, impnme := na.exclude(impnme)[1], .(entity_id, file, imp)]
DTWBMI_HI[, truenme := na.exclude(truenme)[1], .(entity_id, file, imp)]

DTWBMI_HI <- DTWBMI_HI[is.na(orig_nme)]
DTWBMI_HI[, rmse := compute.rmse(n_move_events, n_move_events_bu), .(file, imp, entity_id)]
DTWBMI_HI[, rawbias := truenme - impnme]
DTWBMI_HI[, percTPM := percentTravelPeriodsMissed(n_move_events_bu, n_move_events, 1), .(file, imp, entity_id)]
DTWBMI_HI[, percTPO := percentTravelPeriodsOverestimated(n_move_events_bu, n_move_events, 1), .(file, imp, entity_id)]
DTWBMI_HI[, impTPAcc := impTravelPeriodAccuracy(n_move_events_bu, n_move_events, 1), .(file, imp, entity_id)]
DTWBMI_HI[, movesOver := max(impnme - truenme, 0), .(file, imp, entity_id)]
DTWBMI_HI[, movesUnder := max(truenme - impnme, 0), .(file, imp, entity_id)]

m1 <- DTWBMI_HI[, .(impnme = mean(impnme),
                  truenme = mean(truenme),
                  RMSE = mean(rmse),
                  Bias = mean(rawbias),
                  percTPO = mean(percTPO),
                  percTPM = mean(percTPM),
                  impTPAcc = mean(impTPAcc),
                  movesOver = mean(movesOver),
                  movesUnder = mean(movesUnder),
                  method = "DTWBMI-HI"), .(entity_id, nmiss, devid, n_sets, perc_night)]

# DTWBI-Shape -------------------------------------------------------------

DTWBMI_LO <- rbindlist(
  list(readRDS(file = "Res/4missing-moves-i10b4w3c4.RDS"),
       readRDS(file = "Res/12missing-moves-i10b4w3c4.RDS"),
       readRDS(file = "Res/24missing-moves-i10b4w3c4.RDS"),
       readRDS(file = "Res/40missing-moves-i10b4w3c4.RDS"),
       readRDS(file = "Res/48missing-moves-i10b4w3c4.RDS")
  ), idcol =  "file", use.names = TRUE, fill = TRUE)
DTWBMI_LO[, nmiss := sum(is.na(orig_nme)), .(file, entity_id, imp)]
DTWBMI_LO[, n_move_events_bu := na.exclude(n_move_events_bu)[1], .(.id)]
DTWBMI_LO[, c("devid", "set") := tstrsplit(entity_id, "-")]
DTWBMI_LO[, n_sets := uniqueN(entity_id), .(file, devid)]
DTWBMI_LO[, night := FALSE]
DTWBMI_LO[hour(starttime) >= 21 | hour(starttime) <= 4, night := TRUE]
DTWBMI_LO[is.na(orig_nme), perc_night := sum(night)/.N, .(file, entity_id, imp)]
DTWBMI_LO[, perc_night := perc_night[is.na(orig_nme)][1], .(file, entity_id)]


# Join moves --------------------------------------------------------------
DTWBMI_LO[, no_imp_moves := all(n_move_events == 0), .(entity_id, file, imp)]
DTWBMI_LO[(no_imp_moves), impnme := 0]


DTWBMI_LO[, no_true_moves := all(n_move_events_bu == 0), .(entity_id, file, imp)]
DTWBMI_LO[(no_true_moves), truenme := 0]



DTWBMI_LO[, imp_act_id := rleid(n_move_events), .(entity_id, file, imp)]
DTWBMI_LO[, true_act_id := rleid(n_move_events_bu), .(entity_id, file, imp)]

DTWBMI_LO[n_move_events > 0, impnme := cumsum(n_move_events[1]), .(entity_id, file, imp, imp_act_id)]

DTWBMI_LO[n_move_events_bu > 0, truenme := cumsum(n_move_events_bu[1]), .(entity_id, file, imp, true_act_id)]

DTWBMI_LO[, impnme := na.exclude(impnme)[1], .(entity_id, file, imp)]
DTWBMI_LO[, truenme := na.exclude(truenme)[1], .(entity_id, file, imp)]

DTWBMI_LO <- DTWBMI_LO[is.na(orig_nme)]
DTWBMI_LO[, rmse := compute.rmse(n_move_events, n_move_events_bu), .(file, imp, entity_id)]
DTWBMI_LO[, rawbias := truenme - impnme]
DTWBMI_LO[, percTPM := percentTravelPeriodsMissed(n_move_events_bu, n_move_events, 1), .(file, imp, entity_id)]
DTWBMI_LO[, percTPO := percentTravelPeriodsOverestimated(n_move_events_bu, n_move_events, 1), .(file, imp, entity_id)]
DTWBMI_LO[, impTPAcc := impTravelPeriodAccuracy(n_move_events_bu, n_move_events, 1), .(file, imp, entity_id)]
DTWBMI_LO[, movesOver := max(impnme - truenme, 0), .(file, imp, entity_id)]
DTWBMI_LO[, movesUnder := max(truenme - impnme, 0), .(file, imp, entity_id)]

m2 <- DTWBMI_LO[, .(impnme = mean(impnme),
                  truenme = mean(truenme),
                  RMSE = mean(rmse),
                  Bias = mean(rawbias),
                  percTPO = mean(percTPO),
                  percTPM = mean(percTPM),
                  impTPAcc = mean(impTPAcc),
                  movesOver = mean(movesOver),
                  movesUnder = mean(movesUnder),
                  method = "DTWBMI-LO"), .(entity_id, nmiss, devid, n_sets, perc_night)]


# DTWBI -------------------------------------------------------------------


DTWBI <- rbindlist(list(
  readRDS(file = "Res/4missing-moves-i1b32w1c20.RDS"),
  readRDS(file = "Res/12missing-moves-i1b32w1c20.RDS"),
  readRDS(file = "Res/24missing-moves-i1b32w1c20.RDS"),
  readRDS(file = "Res/40missing-moves-i1b32w1c20.RDS"),
  readRDS(file = "Res/48missing-moves-i1b32w1c20.RDS")
), idcol = "file", use.names = TRUE, fill = TRUE)
DTWBI[, nmiss := sum(is.na(orig_nme)), .(file, entity_id, imp)]
DTWBI[, n_move_events_bu := na.exclude(n_move_events_bu)[1], .(.id)]

DTWBI[, c("devid", "set") := tstrsplit(entity_id, "-")]
DTWBI[, n_sets := uniqueN(entity_id), .(file, devid)]
DTWBI[, night := FALSE]
DTWBI[hour(starttime) >= 21 | hour(starttime) <= 4, night := TRUE]
DTWBI[is.na(orig_nme), perc_night := sum(night)/.N, .(file, entity_id, imp)]
DTWBI[, perc_night := perc_night[is.na(orig_nme)][1], .(file, entity_id)]


# Join moves --------------------------------------------------------------
DTWBI[, no_imp_moves := all(n_move_events == 0), .(entity_id, file, imp)]
DTWBI[(no_imp_moves), impnme := 0]


DTWBI[, no_true_moves := all(n_move_events_bu == 0), .(entity_id, file, imp)]
DTWBI[(no_true_moves), truenme := 0]



DTWBI[, imp_act_id := rleid(n_move_events), .(entity_id, file, imp)]
DTWBI[, true_act_id := rleid(n_move_events_bu), .(entity_id, file, imp)]

DTWBI[n_move_events > 0, impnme := cumsum(n_move_events[1]), .(entity_id, file, imp, imp_act_id)]

DTWBI[n_move_events_bu > 0, truenme := cumsum(n_move_events_bu[1]), .(entity_id, file, imp, true_act_id)]

DTWBI[, impnme := na.exclude(impnme)[1], .(entity_id, file, imp)]
DTWBI[, truenme := na.exclude(truenme)[1], .(entity_id, file, imp)]

DTWBI <- DTWBI[is.na(orig_nme)]
DTWBI[, rmse := compute.rmse(n_move_events, n_move_events_bu), .(file, imp, entity_id)]
DTWBI[, rawbias := truenme - impnme]
DTWBI[, percTPM := percentTravelPeriodsMissed(n_move_events_bu, n_move_events, 1), .(file, imp, entity_id)]
DTWBI[, percTPO := percentTravelPeriodsOverestimated(n_move_events_bu, n_move_events, 1), .(file, imp, entity_id)]
DTWBI[, impTPAcc := impTravelPeriodAccuracy(n_move_events_bu, n_move_events, 1), .(file, imp, entity_id)]
DTWBI[, movesOver := max(impnme - truenme, 0), .(file, imp, entity_id)]
DTWBI[, movesUnder := max(truenme - impnme, 0), .(file, imp, entity_id)]

m3 <- DTWBI[, .(impnme = mean(impnme),
                   truenme = mean(truenme),
                   RMSE = mean(rmse),
                   Bias = mean(rawbias),
                   percTPO = mean(percTPO),
                   percTPM = mean(percTPM),
                   impTPAcc = mean(impTPAcc),
                   movesOver = mean(movesOver),
                   movesUnder = mean(movesUnder),
                   method = "DTWBI"), .(entity_id, nmiss, devid, n_sets, perc_night)]


# TWI ---------------------------------------------------------------------

TWI <- rbindlist(list(
  readRDS(file = "Res/4missing-moves_twiw1.RDS"),
  readRDS(file = "Res/12missing-moves_twiw1.RDS"),
  readRDS(file = "Res/24missing-moves_twiw1.RDS"),
  readRDS(file = "Res/40missing-moves_twiw1.RDS"),
  readRDS(file = "Res/48missing-moves_twiw1.RDS")
  ), idcol = "file", use.names = TRUE, fill = TRUE)

TWI[, nmiss := sum(is.na(orig_nme)), .(file, entity_id, imp)]
TWI[, n_move_events_bu := na.exclude(n_move_events_bu)[1], .(.id)]

TWI[, c("devid", "set") := tstrsplit(entity_id, "-")]
TWI[, n_sets := uniqueN(entity_id), .(file, devid)]
TWI[, night := FALSE]
TWI[hour(starttime) >= 21 | hour(starttime) <= 4, night := TRUE]
TWI[is.na(orig_nme), perc_night := sum(night)/.N, .(file, entity_id, imp)]
TWI[, perc_night := perc_night[is.na(orig_nme)][1], .(file, entity_id)]


# Join moves --------------------------------------------------------------
TWI[, no_imp_moves := all(n_move_events == 0), .(entity_id, file, imp)]
TWI[(no_imp_moves), impnme := 0]


TWI[, no_true_moves := all(n_move_events_bu == 0), .(entity_id, file, imp)]
TWI[(no_true_moves), truenme := 0]


TWI[, imp_act_id := rleid(n_move_events), .(entity_id, file, imp)]
TWI[, true_act_id := rleid(n_move_events_bu), .(entity_id, file, imp)]

TWI[n_move_events > 0, impnme := cumsum(n_move_events[1]), .(entity_id, file, imp, imp_act_id)]

TWI[n_move_events_bu > 0, truenme := cumsum(n_move_events_bu[1]), .(entity_id, file, imp, true_act_id)]

TWI[, impnme := na.exclude(impnme)[1], .(entity_id, file, imp)]
TWI[, truenme := na.exclude(truenme)[1], .(entity_id, file, imp)]

TWI <- TWI[is.na(orig_nme)]
TWI[, rmse := compute.rmse(n_move_events, n_move_events_bu), .(file, imp, entity_id)]
TWI[, rawbias := truenme - impnme]
TWI[, percTPM := percentTravelPeriodsMissed(n_move_events_bu, n_move_events, 1), .(file, imp, entity_id)]
TWI[, percTPO := percentTravelPeriodsOverestimated(n_move_events_bu, n_move_events, 1), .(file, imp, entity_id)]
TWI[, impTPAcc := impTravelPeriodAccuracy(n_move_events_bu, n_move_events, 1), .(file, imp, entity_id)]
TWI[, movesOver := max(impnme - truenme, 0), .(file, imp, entity_id)]
TWI[, movesUnder := max(truenme - impnme, 0), .(file, imp, entity_id)]

m4 <- TWI[, .(impnme = mean(impnme),
                   truenme = mean(truenme),
                   RMSE = mean(rmse),
                   Bias = mean(rawbias),
                   percTPO = mean(percTPO),
                   percTPM = mean(percTPM),
                   impTPAcc = mean(impTPAcc),
                   movesOver = mean(movesOver),
                   movesUnder = mean(movesUnder),
                   method = "TWI"), .(entity_id, nmiss, devid, n_sets, perc_night)]

# linear interpolation ----------------------------------------------------


LI <- rbindlist(list(
  readRDS(file = "Res/4missing-moves-i1b32w1c20.RDS"),
  readRDS(file = "Res/12missing-moves-i1b32w1c20.RDS"),
  readRDS(file = "Res/24missing-moves-i1b32w1c20.RDS"),
  readRDS(file = "Res/40missing-moves-i1b32w1c20.RDS"),
  readRDS(file = "Res/48missing-moves-i1b32w1c20.RDS")
), idcol = "file", use.names = TRUE, fill = TRUE)

# LI <- readRDS("Res/4missing-i3b4w12c4")
LI <- copy(LI[imp == 1])
LI[, n_move_events_bu := na.exclude(n_move_events_bu)[1], .(.id)]
LI[is.na(orig_nme), n_move_events := NA]


LI[is.na(orig_nme), temp_dist := geodist::geodist_vec(x1 = startlon[1], y1 = startlat[1], x2 = endlon[.N], y2 = endlat[.N], measure = "haversine")/1000, .(file, entity_id)]
LI[is.na(orig_nme), n_move_events := ifelse(temp_dist > .2, 1, 0)]
LI[, nmiss := sum(is.na(orig_nme)), .(file, entity_id)]
LI[, c("devid", "set") := tstrsplit(entity_id, "-")]
LI[, n_sets := uniqueN(entity_id), .(file, devid)]
LI[, night := FALSE]
LI[hour(starttime) >= 21 | hour(starttime) <= 4, night := TRUE]
LI[is.na(orig_nme), perc_night := sum(night)/.N, .(file, entity_id)]
LI[, perc_night := perc_night[is.na(orig_nme)][1], .(file, entity_id)]


# Join moves --------------------------------------------------------------

LI[, no_imp_moves := all(n_move_events == 0), .(entity_id, file, imp)]
LI[(no_imp_moves), impnme := 0]
LI[, no_true_moves := all(n_move_events_bu == 0), .(entity_id, file, imp)]
LI[(no_true_moves), truenme := 0]
LI[, imp_act_id := rleid(n_move_events), .(entity_id, file, imp)]
LI[, true_act_id := rleid(n_move_events_bu), .(entity_id, file, imp)]

LI[n_move_events > 0, impnme := cumsum(n_move_events[1]), .(entity_id, file, imp, imp_act_id)]

LI[n_move_events_bu > 0, truenme := cumsum(n_move_events_bu[1]), .(entity_id, file, imp, true_act_id)]

LI[, impnme := na.exclude(impnme)[1], .(entity_id, file, imp)]
LI[, truenme := na.exclude(truenme)[1], .(entity_id, file, imp)]

LI <- LI[is.na(orig_nme)]

LI[, rmse := compute.rmse(n_move_events, n_move_events_bu), .(file, imp, entity_id)]
LI[, rawbias := truenme - impnme]
LI[, percTPM := percentTravelPeriodsMissed(n_move_events_bu, n_move_events, 1), .(file, imp, entity_id)]
LI[, percTPO := percentTravelPeriodsOverestimated(n_move_events_bu, n_move_events, 1), .(file, imp, entity_id)]
LI[, impTPAcc := impTravelPeriodAccuracy(n_move_events_bu, n_move_events, 1), .(file, imp, entity_id)]
LI[, movesOver := max(impnme - truenme, 0), .(file, imp, entity_id)]
LI[, movesUnder := max(truenme - impnme, 0), .(file, imp, entity_id)]

m5 <- LI[, .(impnme = mean(impnme),
              truenme = mean(truenme),
              RMSE = mean(rmse),
              Bias = mean(rawbias),
              percTPO = mean(percTPO),
              percTPM = mean(percTPM),
              impTPAcc = mean(impTPAcc),
              movesOver = mean(movesOver),
              movesUnder = mean(movesUnder),
              method = "LI"), .(entity_id, nmiss, devid, n_sets, perc_night)]



# combine -----------------------------------------------------------------

methods <- rbindlist(list(
  m1, m2, m3, m4, m5
), use.names = TRUE, fill = TRUE)

methods[, bias := impnme - truenme]
methods[abs(bias) < .0001, bias := 0]

methods[n_sets == 1, `Own Sets` := "No extra data"]
methods[n_sets %in% c(2, 3), `Own Sets` := "2-3 sets"]
methods[n_sets %in% c(4, 5, 6, 7, 8, 9), `Own Sets` := "4+ sets"]

msmethods <- methods[nmiss != 0]

msmethods[, .(`Mean difference` = mean(abs(bias)),
              `Median difference` = median(bias),
              RMSE = mean(RMSE),
              TPO = mean(percTPO),
              TPM = mean(percTPM),
              TPAcc = mean(impTPAcc)
              ), .(method)]
msmethods[, .(`Mean difference` = mean(abs(bias)),
              `Median difference` = median(bias),
              RMSE = mean(RMSE),
              TPO = mean(percTPO),
              TPM = mean(percTPM),
              TPAcc = mean(impTPAcc)
              ), .(nmiss, method)][order(nmiss)]
msmethods[, .(`Mean difference` = mean(abs(bias)),
              `Median difference` = median(bias),
              RMSE = mean(RMSE),
              TPO = mean(percTPO),
              TPM = mean(percTPM),
              TPAcc = mean(impTPAcc),
              distOver = mean(distOver),
              distUnder = mean(distUnder)), .(perc_night, method)][order(perc_night)]

saveRDS(msmethods, "Res/Computed/paper3.fullcompmove-20230405.RDS")


msmethods[, .(`Abs Bias` = mean(abs(bias)),
              `Med Bias` = median(bias),
              RMSE = mean(RMSE),
              `TP Over` = mean(percTPO),
              `TP Under` = mean(percTPM),
              `TP Acc.` = mean(impTPAcc)
), .(method)] %>%
  gt(rowname_col = "method") %>%
  gt::sub_missing() %>%
  gt::fmt_number(columns = c("Abs Bias",  "Med Bias"), decimals = 2) %>%
  gt::fmt_number(columns = "RMSE", decimals = 2) %>%
  gt::fmt_percent(columns = c("TP Over",  "TP Under",  "TP Acc."), decimals = 1, scale_values = FALSE) %>%
  gt::tab_header(title = "Method comparison across all cases", subtitle =  "Imputing number of trips")


msmethods[, `Gap Length` := factor(nmiss, labels = c("1 hr", "3 hrs", "6 hrs", "10 hrs", "12 hrs"), ordered = TRUE)]

msmethods[, .(`Abs Bias` = abs(mean(bias)),
              `Trips` = mean(truenme),
              `Med Bias` = median(bias),
              RMSE = mean(RMSE),
              `TP Over` = mean(percTPO),
              `TP Under` = mean(percTPM),
              `TP Acc.` = mean(impTPAcc)
), .(method, `Gap Length`)] %>%
  gt(groupname_col = "Gap Length", rowname_col = "method") %>%
  gt::sub_missing() %>%
  gt::fmt_number(columns = c("Abs Bias",  "Med Bias"), decimals = 3) %>%
  gt::fmt_number(columns = "RMSE", decimals = 2) %>%
  gt::fmt_percent(columns = c("TP Over",  "TP Under",  "TP Acc."), decimals = 1, scale_values = FALSE) %>%
  gt::tab_header(title = "Method comparison across varying gap lengths", subtitle =  "Imputing number of trips")


msmethods[`Percent Bias` == min(`Percent Bias`)]


msmethods[, .(`Abs Bias` = abs(mean((bias))),
              `Med Bias` = median(bias),
              RMSE = mean(RMSE),
              `TP Over` = mean(percTPO),
              `TP Under` = mean(percTPM),
              `TP Acc.` = mean(impTPAcc),
              `Dist Over` = mean(distOver),
              `Dist Under` = mean(distUnder)), .(method, `Own Sets`)] %>%
  gt(groupname_col = "Own Sets", rowname_col = "method") %>%
  gt::row_group_order(c("No extra data", "2-3 sets", "4+ sets")) %>%
  gt::sub_missing() %>%
  gt::fmt_number(columns = c("Abs Bias", "Dist Over", "Dist Under",  "Med Bias"), decimals = 1, pattern = "{x} Km") %>%
  gt::fmt_number(columns = "RMSE", decimals = 2) %>%
  gt::fmt_percent(columns = c("TP Over",  "TP Under",  "TP Acc."), decimals = 1, scale_values = FALSE) %>%
  gt::tab_header(title = "Method comparison across number of reference sets")

# msmethods[, `Gap Length` := cut(nmiss, breaks = c(, 16, 24, 49), include.lowest = TRUE, ordered_result = TRUE,
# labels = c(".5-4h", "4h-8h", "8h-12h"))]
msmethods[, `Gap Length` := factor(nmiss, labels = c("1 hr", "3 hrs", "6 hrs", "10 hrs", "12 hrs"), ordered = TRUE)]

msmethods[, .(`Abs Bias` = abs(mean((bias))),
              `Med Bias` = median(bias),
              RMSE = mean(RMSE),
              `TP Over` = mean(percTPO),
              `TP Under` = mean(percTPM),
              `TP Acc.` = mean(impTPAcc),
              `Dist Over` = mean(distOver),
              `Dist Under` = mean(distUnder)), .(method, `Gap Length`)] %>%
  gt(groupname_col = "Gap Length", rowname_col = "method") %>%
  # gt::row_group_order(c("1h-4h", "4h-8h", "8h-12h")) %>%
  gt::sub_missing() %>%
  gt::fmt_number(columns = c("Abs Bias", "Dist Over", "Dist Under",  "Med Bias"), decimals = 1, pattern = "{x} Km") %>%
  gt::fmt_number(columns = "RMSE", decimals = 2) %>%
  gt::fmt_percent(columns = c("TP Over",  "TP Under",  "TP Acc."), decimals = 1, scale_values = FALSE) %>%
  gt::tab_header(title = "Method comparison across gap length")



msmethods[, `Percent Night` := cut(perc_night, breaks = c(0, .9, 1.1), include.lowest = TRUE,
                                   labels = c("Daytime Missing", "Night Only"))]

msmethods[nmiss == 12,
          .(`Abs Bias` = abs(mean((bias))),
            `Med Bias` = median(bias),
            RMSE = mean(RMSE),
            `TP Over` = mean(percTPO),
            `TP Under` = mean(percTPM),
            `TP Acc.` = mean(impTPAcc),
            `Dist Over` = mean(distOver),
            `Dist Under` = mean(distUnder)), .(method, `Percent Night`)] %>%
  gt(groupname_col = "Percent Night", rowname_col = "method") %>%
  gt::row_group_order(c("Daytime Missing", "Night Only")) %>%
  # gt::row_group_order(c("1h-4h", "4h-8h", "8h-12h")) %>%
  gt::sub_missing() %>%
  gt::fmt_number(columns = c("Abs Bias", "Dist Over", "Dist Under",  "Med Bias"), decimals = 1, pattern = "{x} Km") %>%
  gt::fmt_number(columns = "RMSE", decimals = 2) %>%
  gt::fmt_percent(columns = c("TP Over",  "TP Under",  "TP Acc."), decimals = 1, scale_values = FALSE) %>%
  gt::tab_header(title = "Method comparison across night only vs day", subtitle = "3 hour gap condition")





