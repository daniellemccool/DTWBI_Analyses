
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

library(DTWBI)

distanceUnderestimated <- function(true, imp){
  max(sum(true) - sum(imp), 0)
}



# DTWBI-Burst -------------------------------------------------------------

DTWBMI_HI <- rbindlist(
  list(readRDS(file = "Res/4missing-i3b32w12c5.RDS"),
       readRDS(file = "Res/12missing-i3b32w12c5.RDS"),
       readRDS(file = "Res/24missing-i3b32w12c5.RDS"),
       readRDS(file = "Res/40missing-i3b32w12c5.RDS"),
       readRDS(file = "Res/48missing-i3b32w12c5.RDS")), idcol = TRUE)
DTWBMI_HI[, nmiss := sum(is.na(orig_dist)), .(.id, entity_id, imp)]
DTWBMI_HI[, c("devid", "set") := tstrsplit(entity_id, "-")]
DTWBMI_HI[, n_sets := uniqueN(entity_id), .(.id, devid)]
DTWBMI_HI[, night := FALSE]
DTWBMI_HI[hour(starttime) >= 21 | hour(starttime) <= 4, night := TRUE]
DTWBMI_HI[is.na(orig_dist), perc_night := sum(night)/.N, .(.id, entity_id, imp)]
DTWBMI_HI[, perc_night := perc_night[is.na(orig_dist)][1], .(.id, entity_id)]
DTWBMI_HI <- DTWBMI_HI[is.na(orig_dist)]
DTWBMI_HI[, rmse := compute.rmse(dist, dist_bu), .(.id, imp, entity_id)]
DTWBMI_HI[, truedist := sum(dist_bu), .(.id, imp, entity_id)]
DTWBMI_HI[, impdist := sum(dist), .(.id, imp, entity_id)]
DTWBMI_HI[, rawbias := truedist - impdist]
DTWBMI_HI[, percTPM := percentTravelPeriodsMissed(dist_bu, dist, 1), .(.id, imp, entity_id)]
DTWBMI_HI[, percTPO := percentTravelPeriodsOverestimated(dist_bu, dist, 1), .(.id, imp, entity_id)]
DTWBMI_HI[, impTPAcc := impTravelPeriodAccuracy(dist_bu, dist, 1), .(.id, imp, entity_id)]
DTWBMI_HI[, distOver := distanceOverestimated(dist_bu, dist), .(.id, imp, entity_id)]
DTWBMI_HI[, distUnder := distanceUnderestimated(dist_bu, dist), .(.id, imp, entity_id)]
m1 <- DTWBMI_HI[, .(impdist = mean(impdist),
                  truedist = mean(truedist),
                  RMSE = mean(rmse),
                  Bias = mean(abs(rawbias)),
                  percTPO = mean(percTPO),
                  percTPM = mean(percTPM),
                  impTPAcc = mean(impTPAcc),
                  distOver = mean(distOver),
                  distUnder = mean(distUnder),
                  method = "DTWBMI-HI"), .(entity_id, nmiss, devid, n_sets, perc_night)]

# DTWBI-Shape -------------------------------------------------------------

DTWBMI_LO <- rbindlist(list(
  readRDS(file = "Res/4missing-i10b4w3c4.RDS"),
  readRDS(file = "Res/12missing-i10b4w3c4.RDS"),
  readRDS(file = "Res/24missing-i10b4w3c4.RDS"),
  readRDS(file = "Res/40missing-i10b4w3c4.RDS"),
  readRDS(file = "Res/48missing-i10b4w3c4.RDS")), idcol = TRUE)
DTWBMI_LO[, nmiss := sum(is.na(orig_dist)), .(.id, entity_id, imp)]
DTWBMI_LO[, c("devid", "set") := tstrsplit(entity_id, "-")]
DTWBMI_LO[, n_sets := uniqueN(entity_id), .(.id, devid)]
DTWBMI_LO[, night := FALSE]
DTWBMI_LO[hour(starttime) >= 21 | hour(starttime) <= 4, night := TRUE]
DTWBMI_LO[is.na(orig_dist), perc_night := sum(night)/.N, .(.id, entity_id, imp)]
DTWBMI_LO[, perc_night := perc_night[is.na(orig_dist)][1], .(.id, entity_id)]
DTWBMI_LO <- DTWBMI_LO[is.na(orig_dist)]
DTWBMI_LO[, rmse := compute.rmse(dist, dist_bu), .(.id, imp, entity_id)]
DTWBMI_LO[, truedist := sum(dist_bu), .(.id, imp, entity_id)]
DTWBMI_LO[, impdist := sum(dist), .(.id, imp, entity_id)]
DTWBMI_LO[, rawbias := truedist - impdist]
DTWBMI_LO[, percTPM := percentTravelPeriodsMissed(dist_bu, dist, 1), .(.id, imp, entity_id)]
DTWBMI_LO[, percTPO := percentTravelPeriodsOverestimated(dist_bu, dist, 1), .(.id, imp, entity_id)]
DTWBMI_LO[, impTPAcc := impTravelPeriodAccuracy(dist_bu, dist, 1), .(.id, imp, entity_id)]
DTWBMI_LO[, distOver := distanceOverestimated(dist_bu, dist), .(.id, imp, entity_id)]
DTWBMI_LO[, distUnder := distanceUnderestimated(dist_bu, dist), .(.id, imp, entity_id)]
m2 <- DTWBMI_LO[, .(impdist = mean(impdist),
                  truedist = mean(truedist),
                  RMSE = mean(rmse),
                  Bias = mean(abs(rawbias)),
                  percTPO = mean(percTPO),
                  percTPM = mean(percTPM),
                  impTPAcc = mean(impTPAcc),
                  distOver = mean(distOver),
                  distUnder = mean(distUnder),
                  method = "DTWBMI-LO"), .(entity_id, nmiss, devid, n_sets, perc_night)]


# DTWBI -------------------------------------------------------------------


DTWBI <- rbindlist(list(
  readRDS(file = "Res/4missing-i1b32w1c20.RDS"),
  readRDS(file = "Res/12missing-i1b32w1c20.RDS"),
  readRDS(file = "Res/24missing-i1b32w1c20.RDS"),
  readRDS(file = "Res/40missing-i1b32w1c20.RDS"),
  readRDS(file = "Res/48missing-i1b32w1c20.RDS")), idcol = TRUE)
DTWBI[, nmiss := sum(is.na(orig_dist)), .(.id, entity_id, imp)]
DTWBI[, c("devid", "set") := tstrsplit(entity_id, "-")]
DTWBI[, n_sets := uniqueN(entity_id), .(.id, devid)]
DTWBI[, night := FALSE]
DTWBI[hour(starttime) >= 21 | hour(starttime) <= 4, night := TRUE]
DTWBI[is.na(orig_dist), perc_night := sum(night)/.N, .(.id, entity_id, imp)]
DTWBI[, perc_night := perc_night[is.na(orig_dist)][1], .(.id, entity_id)]
DTWBI <- DTWBI[is.na(orig_dist)]
DTWBI[, rmse := compute.rmse(dist, dist_bu), .(.id, imp, entity_id)]
DTWBI[, truedist := sum(dist_bu), .(.id, imp, entity_id)]
DTWBI[, impdist := sum(dist), .(.id, imp, entity_id)]
DTWBI[, rawbias := truedist - impdist]
DTWBI[, percTPM := percentTravelPeriodsMissed(dist_bu, dist, 1), .(.id, imp, entity_id)]
DTWBI[, percTPO := percentTravelPeriodsOverestimated(dist_bu, dist, 1), .(.id, imp, entity_id)]
DTWBI[, impTPAcc := impTravelPeriodAccuracy(dist_bu, dist, 1), .(.id, imp, entity_id)]
DTWBI[, distOver := distanceOverestimated(dist_bu, dist), .(.id, imp, entity_id)]
DTWBI[, distUnder := distanceUnderestimated(dist_bu, dist), .(.id, imp, entity_id)]
m3 <- DTWBI[, .(impdist = mean(impdist),
                  truedist = mean(truedist),
                  RMSE = mean(rmse),
                  Bias = mean(abs(rawbias)),
                  percTPO = mean(percTPO),
                  percTPM = mean(percTPM),
                  impTPAcc = mean(impTPAcc),
                  distOver = mean(distOver),
                  distUnder = mean(distUnder),
                  method = "DTWBI"), .(entity_id, nmiss, devid, n_sets, perc_night)]


# TWI ---------------------------------------------------------------------

TWI <- rbindlist(list(
  readRDS(file = "Res/4missing-dist_TWIw1.RDS"),
  readRDS(file = "Res/12missing-dist_TWIw1.RDS"),
  readRDS(file = "Res/24missing-dist_TWIw1.RDS"),
  readRDS(file = "Res/40missing-dist_TWIw1.RDS"),
  readRDS(file = "Res/48missing-dist_TWIw1.RDS")), idcol = "file")

TWI[, nmiss := sum(is.na(orig_dist)), .(file, entity_id, imp)]
TWI[, c("devid", "set") := tstrsplit(entity_id, "-")]
TWI[, n_sets := uniqueN(entity_id), .(file, devid)]
TWI[, night := FALSE]
TWI[hour(starttime) >= 21 | hour(starttime) <= 4, night := TRUE]
TWI[is.na(orig_dist), perc_night := sum(night)/.N, .(file, entity_id, imp)]
TWI[, perc_night := perc_night[is.na(orig_dist)][1], .(file, entity_id)]
TWI <- TWI[is.na(orig_dist)]
TWI[, rmse := compute.rmse(dist, dist_bu), .(file, imp, entity_id)]
TWI[, truedist := sum(dist_bu), .(file, imp, entity_id)]
TWI[, impdist := sum(dist), .(file, imp, entity_id)]
TWI[, rawbias := truedist - impdist]
TWI[, percTPM := percentTravelPeriodsMissed(dist_bu, dist, 1), .(file, imp, entity_id)]
TWI[, percTPO := percentTravelPeriodsOverestimated(dist_bu, dist, 1), .(file, imp, entity_id)]
TWI[, impTPAcc := impTravelPeriodAccuracy(dist_bu, dist, 1), .(file, imp, entity_id)]
TWI[, distOver := distanceOverestimated(dist_bu, dist), .(file, imp, entity_id)]
TWI[, distUnder := distanceUnderestimated(dist_bu, dist), .(file, imp, entity_id)]

m4 <- TWI[, .(impdist = mean(impdist),
                truedist = mean(truedist),
                RMSE = mean(rmse),
                Bias = mean(abs(rawbias)),
                percTPO = mean(percTPO),
                percTPM = mean(percTPM),
                impTPAcc = mean(impTPAcc),
                distOver = mean(distOver),
                distUnder = mean(distUnder),
                method = "TWI"), .(entity_id, nmiss, devid, n_sets, perc_night)]

# linear interpolation ----------------------------------------------------

LI <- rbindlist(list(
  readRDS("Res/4missing-i1b32w1c20.RDS"),
  readRDS("Res/12missing-i1b32w1c20.RDS"),
  readRDS("Res/24missing-i1b32w1c20.RDS"),
  readRDS("Res/40missing-i1b32w1c20.RDS"),
  readRDS("Res/48missing-i1b32w1c20.RDS")), idcol = TRUE)

LI <- copy(LI[imp == 1])
LI[is.na(orig_dist), dist := NA]

LI[is.na(dist), dist := geodist::geodist_vec(x1 = startlon[1], y1 = startlat[1], x2 = endlon[.N], y2 = endlat[.N], measure = "haversine")/1000/.N, .(.id, entity_id)]
LI[, nmiss := sum(is.na(orig_dist)), .(.id, entity_id)]
LI[, c("devid", "set") := tstrsplit(entity_id, "-")]
LI[, n_sets := uniqueN(entity_id), .(.id, devid)]
LI[, night := FALSE]
LI[hour(starttime) >= 21 | hour(starttime) <= 4, night := TRUE]
LI[is.na(orig_dist), perc_night := sum(night)/.N, .(.id, entity_id)]
LI[, perc_night := perc_night[is.na(orig_dist)][1], .(.id, entity_id)]
LI <- LI[is.na(orig_dist)]

LI[, rmse := compute.rmse(dist, dist_bu), .(.id, entity_id)]
LI[, truedist := sum(dist_bu), .(.id, entity_id)]
LI[, impdist := sum(dist), .(.id, entity_id)]
LI[, rawbias := truedist - impdist]
LI[, percTPM := percentTravelPeriodsMissed(dist_bu, dist, 1), .(.id, entity_id)]
LI[, percTPO := percentTravelPeriodsOverestimated(dist_bu, dist, 1), .(.id, entity_id)]
LI[, impTPAcc := impTravelPeriodAccuracy(dist_bu, dist, 1), .(.id, entity_id)]
LI[, distOver := distanceOverestimated(dist_bu, dist), .(.id, entity_id)]
LI[, distUnder := distanceUnderestimated(dist_bu, dist), .(.id, entity_id)]
m5 <- LI[, .(impdist = mean(impdist),
              truedist = mean(truedist),
              RMSE = mean(rmse),
              Bias = mean(abs(rawbias)),
              percTPO = mean(percTPO),
              percTPM = mean(percTPM),
              impTPAcc = mean(impTPAcc),
              distOver = mean(distOver),
              distUnder = mean(distUnder),
              method = "LI"), .(entity_id, nmiss, devid, n_sets, perc_night)]

# Mean imputation ---------------------------------------------------------

IMPAGG <- rbindlist(list(
  readRDS("Res/4missing-i1b32w1c20.RDS"),
  readRDS("Res/12missing-i1b32w1c20.RDS"),
  readRDS("Res/24missing-i1b32w1c20.RDS"),
  readRDS("Res/40missing-i1b32w1c20.RDS"),
  readRDS("Res/48missing-i1b32w1c20.RDS")), idcol = TRUE)

IMPAGG[is.na(orig_dist), dist := NA]

mean15mindist <- IMPAGG[!is.na(dist), .(daydist = sum(dist)/.N), .(.id, entity_id)][, mean(daydist), .(.id)]
IMPAGG[mean15mindist, on = .(.id), temp_dist := i.V1]
IMPAGG[is.na(dist), dist := temp_dist]
IMPAGG[, temp_dist := NULL]
IMPAGG[, nmiss := sum(is.na(orig_dist)), .(.id, entity_id)]
IMPAGG[, c("devid", "set") := tstrsplit(entity_id, "-")]
IMPAGG[, n_sets := uniqueN(entity_id), .(.id, devid)]
IMPAGG[, night := FALSE]
IMPAGG[hour(starttime) >= 21 | hour(starttime) <= 4, night := TRUE]
IMPAGG[is.na(orig_dist), perc_night := sum(night)/.N, .(.id, entity_id)]
IMPAGG[, perc_night := perc_night[is.na(orig_dist)][1], .(.id, entity_id)]
IMPAGG <- IMPAGG[is.na(orig_dist)]
IMPAGG[, rmse := compute.rmse(dist, dist_bu), .(.id, entity_id)]
IMPAGG[, truedist := sum(dist_bu), .(.id, entity_id)]
IMPAGG[, impdist := sum(dist), .(.id, entity_id)]
IMPAGG[, rawbias := truedist - impdist]
IMPAGG[, percTPM := percentTravelPeriodsMissed(dist_bu, dist, 1), .(.id, entity_id)]
IMPAGG[, percTPO := percentTravelPeriodsOverestimated(dist_bu, dist, 1), .(.id, entity_id)]
IMPAGG[, impTPAcc := impTravelPeriodAccuracy(dist_bu, dist, 1), .(.id, entity_id)]
IMPAGG[, distOver := distanceOverestimated(dist_bu, dist), .(.id, entity_id)]
IMPAGG[, distUnder := distanceUnderestimated(dist_bu, dist), .(.id, entity_id)]
m6 <- IMPAGG[, .(impdist = mean(impdist),
             truedist = mean(truedist),
             RMSE = mean(rmse),
             Bias = mean(abs(rawbias)),
             percTPO = mean(percTPO),
             percTPM = mean(percTPM),
             impTPAcc = mean(impTPAcc),
             distOver = mean(distOver),
             distUnder = mean(distUnder),
             method = "MI"), .(entity_id, nmiss, devid, n_sets, perc_night)]



# combine -----------------------------------------------------------------

methods <- rbindlist(list(
  m1, m2, m3, m4, m5, m6
), use.names = TRUE, fill = TRUE)

methods[, bias := impdist - truedist]
methods[abs(bias) < .0001, bias := 0]

methods[n_sets == 1, `Own Sets` := "No extra data"]
methods[n_sets %in% c(2, 3), `Own Sets` := "2-3 sets"]
methods[n_sets %in% c(4, 5, 6, 7, 8, 9), `Own Sets` := "4+ sets"]

msmethods <- methods[nmiss != 0]

msmethods[, .(`Mean difference` = abs(mean((bias))),
              `Median difference` = median(bias),
              RMSE = mean(RMSE),
              TPO = mean(percTPO),
              TPM = mean(percTPM),
              TPAcc = mean(impTPAcc),
              distOver = mean(distOver),
              distUnder = mean(distUnder)), .(method)]
msmethods[, .(`Mean difference` = abs(mean(bias)),
              `Median difference` = median(bias),
              RMSE = mean(RMSE),
              TPO = mean(percTPO),
              TPM = mean(percTPM),
              TPAcc = mean(impTPAcc),
              distOver = mean(distOver),
              distUnder = mean(distUnder)), .(nmiss, method)][order(nmiss)]
msmethods[, .(`Mean difference` = mean(abs(bias)),
              `Median difference` = median(bias),
              RMSE = mean(RMSE),
              TPO = mean(percTPO),
              TPM = mean(percTPM),
              TPAcc = mean(impTPAcc),
              distOver = mean(distOver),
              distUnder = mean(distUnder)), .(perc_night, method)][order(perc_night)]

saveRDS(msmethods, "Data/Computed/paper3.fullcomp-20230403.RDS")


msmethods[, .(`Abs Bias` = abs(mean((bias))),
              `Med Bias` = median(bias),
              RMSE = mean(RMSE),
              `TP Over` = mean(percTPO),
              `TP Under` = mean(percTPM),
              `TP Acc.` = mean(impTPAcc),
              `Dist Over` = mean(distOver),
              `Dist Under` = mean(distUnder)), .(method)] %>% 
  gt(rowname_col = "method") %>%
  gt::sub_missing() %>%
  gt::fmt_number(columns = c("Abs Bias", "Dist Over", "Dist Under",  "Med Bias"), decimals = 1, pattern = "{x} Km") %>% 
  gt::fmt_number(columns = "RMSE", decimals = 2) %>%
  gt::fmt_percent(columns = c("TP Over",  "TP Under",  "TP Acc."), decimals = 1, scale_values = FALSE) %>% 
  gt::tab_header(title = "Method comparison across all cases")


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
  gt::sub_missing() %>%
  gt::fmt_number(columns = c("Abs Bias", "Dist Over", "Dist Under",  "Med Bias"), decimals = 1, pattern = "{x} Km") %>% 
  gt::fmt_number(columns = "RMSE", decimals = 2) %>%
  gt::fmt_percent(columns = c("TP Over",  "TP Under",  "TP Acc."), decimals = 1, scale_values = FALSE) %>% 
  gt::tab_header(title = "Method comparison across night only vs day", subtitle = "3 hour gap condition")





