USE_SERVER <- "ds"
library(DTWBI)
library(data.table)


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


dtwbmi_hi_files <- list.files(pattern = "*i3b32w12c5*", include.dirs = TRUE, full.names = TRUE, recursive = TRUE)
templs <- lapply(dtwbmi_hi_files, readRDS)
names(templs) <- dtwbmi_hi_files

DTWBMI_HI <- rbindlist(templs, idcol =  "file", use.names = TRUE, fill = TRUE)
DTWBMI_HI[file %like% "missing4", n_missing := 4]
DTWBMI_HI[file %like% "missing12", n_missing := 12]
DTWBMI_HI[file %like% "missing24", n_missing := 24]
DTWBMI_HI[file %like% "missing40", n_missing := 40]
DTWBMI_HI[file %like% "missing48", n_missing := 48]

DTWBMI_HI[file %like% "sc1", sets_as_candidates := 0]
DTWBMI_HI[file %like% "sc2", sets_as_candidates := 1]
DTWBMI_HI[file %like% "sc3", sets_as_candidates := 2]
DTWBMI_HI[file %like% "sc4", sets_as_candidates := 3]

DTWBMI_HI[file %like%  "data1-", dataset := 1]
DTWBMI_HI[file %like%  "data2-", dataset := 2]
DTWBMI_HI[file %like%  "data3-", dataset := 3]
DTWBMI_HI[file %like%  "data4-", dataset := 4]
DTWBMI_HI[file %like%  "data5-", dataset := 5]
DTWBMI_HI[file %like%  "data6-", dataset := 6]
DTWBMI_HI[file %like%  "data7-", dataset := 7]
DTWBMI_HI[file %like%  "data8-", dataset := 8]
DTWBMI_HI[file %like%  "data9-", dataset := 9]
DTWBMI_HI[file %like%  "data10-", dataset := 10]



DTWBMI_HI <- DTWBMI_HI[is.na(orig_dist)]
DTWBMI_HI[, rmse := compute.rmse(dist, dist_bu), .(file, imp, entity_id)]
DTWBMI_HI[, truedist := sum(dist_bu), .(file, imp, entity_id)]
DTWBMI_HI[, impdist := sum(dist), .(file, imp, entity_id)]
DTWBMI_HI[, rawbias := truedist - impdist]
DTWBMI_HI[, percTPM := percentTravelPeriodsMissed(dist_bu, dist, 1), .(file, imp, entity_id)]
DTWBMI_HI[, percTPO := percentTravelPeriodsOverestimated(dist_bu, dist, 1), .(file, imp, entity_id)]
DTWBMI_HI[, impTPAcc := impTravelPeriodAccuracy(dist_bu, dist, 1), .(file, imp, entity_id)]
DTWBMI_HI[, distOver := distanceOverestimated(dist_bu, dist), .(file, imp, entity_id)]
DTWBMI_HI[, distUnder := distanceUnderestimated(dist_bu, dist), .(file, imp, entity_id)]

DTWBMI_HI[, mean(rawbias), .(n_missing, sets_as_candidates)][order(n_missing, sets_as_candidates)]
DTWBMI_HI[, RMSE := sqrt(mean(rawbias^2)), .(n_missing, sets_as_candidates) ]

m1 <- DTWBMI_HI[, .(impdist = mean(impdist),
                    truedist = mean(truedist),
                    RMSE = mean(RMSE),
                    Bias = abs(mean(rawbias)),
                    MedBias = median(rawbias),
                    percTPO = mean(percTPO),
                    percTPM = mean(percTPM),
                    impTPAcc = mean(impTPAcc),
                    distOver = mean(distOver),
                    distUnder = mean(distUnder),
                    method = "DTWBMI-HI"), .(n_missing, sets_as_candidates)][order(n_missing, sets_as_candidates)]


dtwbmi_lo_files <- list.files(pattern = "i10b4w3c4", include.dirs = TRUE, full.names = TRUE, recursive = TRUE)
templs <- lapply(dtwbmi_lo_files, readRDS)
names(templs) <- dtwbmi_lo_files
DTWBMI_LO <- rbindlist(templs, idcol =  "file", use.names = TRUE, fill = TRUE)
DTWBMI_LO[file %like% "missing4", n_missing := 4]
DTWBMI_LO[file %like% "missing12", n_missing := 12]
DTWBMI_LO[file %like% "missing24", n_missing := 24]
DTWBMI_LO[file %like% "missing40", n_missing := 40]
DTWBMI_LO[file %like% "missing48", n_missing := 48]

DTWBMI_LO[file %like% "sc1", sets_as_candidates := 0]
DTWBMI_LO[file %like% "sc2", sets_as_candidates := 1]
DTWBMI_LO[file %like% "sc3", sets_as_candidates := 2]
DTWBMI_LO[file %like% "sc4", sets_as_candidates := 3]

DTWBMI_LO[file %like%  "data1-", dataset := 1]
DTWBMI_LO[file %like%  "data2-", dataset := 2]
DTWBMI_LO[file %like%  "data3-", dataset := 3]
DTWBMI_LO[file %like%  "data4-", dataset := 4]
DTWBMI_LO[file %like%  "data5-", dataset := 5]
DTWBMI_LO[file %like%  "data6-", dataset := 6]
DTWBMI_LO[file %like%  "data7-", dataset := 7]
DTWBMI_LO[file %like%  "data8-", dataset := 8]
DTWBMI_LO[file %like%  "data9-", dataset := 9]
DTWBMI_LO[file %like%  "data10-", dataset := 10]


DTWBMI_LO <- DTWBMI_LO[is.na(orig_dist)]
DTWBMI_LO[, rmse := compute.rmse(dist, dist_bu), .(file, imp, entity_id)]
DTWBMI_LO[, truedist := sum(dist_bu), .(file, imp, entity_id)]
DTWBMI_LO[, impdist := sum(dist), .(file, imp, entity_id)]
DTWBMI_LO[, rawbias := truedist - impdist]
DTWBMI_LO[, percTPM := percentTravelPeriodsMissed(dist_bu, dist, 1), .(file, imp, entity_id)]
DTWBMI_LO[, percTPO := percentTravelPeriodsOverestimated(dist_bu, dist, 1), .(file, imp, entity_id)]
DTWBMI_LO[, impTPAcc := impTravelPeriodAccuracy(dist_bu, dist, 1), .(file, imp, entity_id)]
DTWBMI_LO[, distOver := distanceOverestimated(dist_bu, dist), .(file, imp, entity_id)]
DTWBMI_LO[, distUnder := distanceUnderestimated(dist_bu, dist), .(file, imp, entity_id)]

DTWBMI_LO[, mean(rawbias), .(sets_as_candidates)][order(sets_as_candidates)]
DTWBMI_LO[, mean(rawbias), .(n_missing, sets_as_candidates)][order(n_missing, sets_as_candidates)]

DTWBMI_LO[, RMSE := sqrt(mean(rawbias^2)), .(n_missing, sets_as_candidates) ]

m2 <- DTWBMI_LO[, .(impdist = mean(impdist),
                    truedist = mean(truedist),
                    RMSE = mean(RMSE),
                    Bias = abs(mean(rawbias)),
                    MedBias = median(rawbias),
                    percTPO = mean(percTPO),
                    percTPM = mean(percTPM),
                    impTPAcc = mean(impTPAcc),
                    distOver = mean(distOver),
                    distUnder = mean(distUnder),
                    method = "DTWBMI-LO"), .(n_missing, sets_as_candidates)][order(n_missing, sets_as_candidates)]

dtwbi_files <- list.files(pattern = "i1b32w1c20", include.dirs = TRUE, full.names = TRUE, recursive = TRUE)
templs <- lapply(dtwbi_files, readRDS)
names(templs) <- dtwbi_files
DTWBI <- rbindlist(templs, idcol =  "file", use.names = TRUE, fill = TRUE)
rm(templs)
DTWBI <- DTWBI[is.na(orig_dist)]
DTWBI[file %like% "missing4", n_missing := 4]
DTWBI[file %like% "missing12", n_missing := 12]
DTWBI[file %like% "missing24", n_missing := 24]
DTWBI[file %like% "missing40", n_missing := 40]
DTWBI[file %like% "missing48", n_missing := 48]

DTWBI[file %like% "sc1", sets_as_candidates := 0]
DTWBI[file %like% "sc2", sets_as_candidates := 1]
DTWBI[file %like% "sc3", sets_as_candidates := 2]
DTWBI[file %like% "sc4", sets_as_candidates := 3]

DTWBI[file %like%  "data1-", dataset := 1]
DTWBI[file %like%  "data2-", dataset := 2]
DTWBI[file %like%  "data3-", dataset := 3]
DTWBI[file %like%  "data4-", dataset := 4]
DTWBI[file %like%  "data5-", dataset := 5]
DTWBI[file %like%  "data6-", dataset := 6]
DTWBI[file %like%  "data7-", dataset := 7]
DTWBI[file %like%  "data8-", dataset := 8]
DTWBI[file %like%  "data9-", dataset := 9]
DTWBI[file %like%  "data10-", dataset := 10]

# install.packages("DTWBI")

# DTWBI <- DTWBI[is.na(orig_dist)]
DTWBI[, rmse := compute.rmse(dist, dist_bu), .(file, imp, entity_id)]
DTWBI[, truedist := sum(dist_bu), .(file, imp, entity_id)]
DTWBI[, impdist := sum(dist), .(file, imp, entity_id)]
DTWBI[, rawbias := truedist - impdist]
DTWBI[, percTPM := percentTravelPeriodsMissed(dist_bu, dist, 1), .(file, imp, entity_id)]
DTWBI[, percTPO := percentTravelPeriodsOverestimated(dist_bu, dist, 1), .(file, imp, entity_id)]
DTWBI[, impTPAcc := impTravelPeriodAccuracy(dist_bu, dist, 1), .(file, imp, entity_id)]
DTWBI[, distOver := distanceOverestimated(dist_bu, dist), .(file, imp, entity_id)]
DTWBI[, distUnder := distanceUnderestimated(dist_bu, dist), .(file, imp, entity_id)]

DTWBI[, mean(rawbias), .(sets_as_candidates, n_missing)][order(n_missing, sets_as_candidates)]
DTWBI[, RMSE := sqrt(mean(rawbias^2)), .(n_missing, sets_as_candidates) ]

m3 <- DTWBI[, .(impdist = mean(impdist),
                    truedist = mean(truedist),
                    RMSE = mean(RMSE),
                    Bias = mean(abs(rawbias)),
                    percTPO = mean(percTPO),
                    percTPM = mean(percTPM),
                    impTPAcc = mean(impTPAcc),
                    distOver = mean(distOver),
                    distUnder = mean(distUnder),
                    method = "DTWBI"), .(n_missing, sets_as_candidates)][order(n_missing, sets_as_candidates)]

methods <- rbindlist(list(
  m1, m2
), use.names = TRUE, fill = TRUE)

methods[, bias := impdist - truedist]
methods[abs(bias) < .0001, bias := 0]
msmethods <- methods
saveRDS(msmethods, "Res/Computed/paper3.extrasets-20230602.RDS")

library(gt)
install.packages("gt")
library(dplyr)

msmethods[, .(`Abs Bias` = mean((Bias)),
              `Med Bias` = mean(MedBias),
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

msmethods[, `Own Sets` := sets_as_candidates]
msmethods[, .(`Abs Bias` = mean((Bias)),
              RMSE = mean(RMSE),
              `TP Over` = mean(percTPO),
              `TP Under` = mean(percTPM),
              `TP Acc.` = mean(impTPAcc),
              `Dist Over` = mean(distOver),
              `Dist Under` = mean(distUnder)), .(method, `Own Sets`)] %>% 
  gt(groupname_col = "Own Sets", rowname_col = "method") %>%
  gt::sub_missing() %>%
  gt::fmt_number(columns = c("Abs Bias", "Dist Over", "Dist Under"), decimals = 1, pattern = "{x} Km") %>% 
  gt::fmt_number(columns = "RMSE", decimals = 2) %>%
  gt::fmt_percent(columns = c("TP Over",  "TP Under",  "TP Acc."), decimals = 1, scale_values = FALSE) %>% 
  gt::tab_header(title = "Method comparison across number of own sets added to reference")

msmethods[, `Gap Length` := factor(n_missing, levels = c(4, 12, 24, 40, 48), labels = c("1h", "3h", "6h", "8h", "12h"))]

msmethods[, .(`Abs Bias` = mean((Bias)),
              `Med Bias` = mean(MedBias),
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


msmethods[, .(`Abs Bias` = mean((Bias)),
              RMSE = mean(RMSE),
              `TP Over` = mean(percTPO),
              `TP Under` = mean(percTPM),
              `TP Acc.` = mean(impTPAcc),
              `Dist Over` = mean(distOver),
              `Dist Under` = mean(distUnder)), .(method, `Gap Length`, `Own Sets`)] %>% 
  gt(groupname_col = "Gap Length", rowname_col = "method") %>%
  gt::sub_missing() %>%
  gt::fmt_number(columns = c("Abs Bias", "Dist Over", "Dist Under"), decimals = 1, pattern = "{x} Km") %>% 
  gt::fmt_number(columns = "RMSE", decimals = 1) %>%
  gt::fmt_percent(columns = c("TP Over",  "TP Under",  "TP Acc."), decimals = 1, scale_values = FALSE) %>% 
  gt::tab_header(title = "Method comparison across gap length")


saveRDS(msmethods, "Res/Computed/paper3.extrasets-20230602.RDS")
