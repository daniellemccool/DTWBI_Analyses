library(data.table)
library(ggplot2)
library(DTWBI)
setDTthreads(threads = 2)

source("../shared_code/utils.R")
setwd("Res/")
files <- list.files(path = '.', pattern = 'imp10.RDS')

setattr(files, 'names', files)
sims <- rbindlist(lapply(files, readRDS), idcol = "file", fill = TRUE)

sims[file %like% "w12", window := "no restriction"]
sims[file %like% "w3", window := "within 3 hrs"]
sims[file %like% "w1c", window := "within 1 hr"]
sims[file %like% "m4", match.buffer := "1 hr"]
sims[file %like% "m16", match.buffer := "4 hrs"]
sims[file %like% "m32", match.buffer := "8 hrs"]
sims[file %like% "c3", candidate.specificity := "low"]
sims[file %like% "c4", candidate.specificity := "medium"]
sims[file %like% "c5", candidate.specificity := "high"]
sims[file %like% "c14", candidate.specificity := "always"]


sims[, dist_bu_km := dist_bu/1000]
sims[, c("devid", "set") := tstrsplit(entity_id, "-")]
sims[, n_sets := uniqueN(entity_id), .(devid)]
# sims[sets, on = .(devid == V1), n_sets := N]

missing <- sims[is.na(orig_dist)]
# missing <- missing[!is.na(dist)]
missing[, diff := dist_bu_km - dist]
msres1[, window := factor(window, levels = c("within 1 hr", "within 3 hrs", "no restriction"))]
missing[, sum(dist), .(file, imp, entity_id)][, uniqueN(V1), .(file, entity_id)][, mean(V1), file][order(V1)]



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



missing[, rmse := compute.rmse(dist, dist_bu_km), .(file, imp, entity_id)]
missing[, truedist := sum(dist_bu_km), .(file, imp, entity_id)]
missing[, impdist := sum(dist), .(file, imp, entity_id)]
missing[, rawbias := truedist - impdist]
missing[, rawabias := abs(truedist - impdist)]
missing[, percTPM := percentTravelPeriodsMissed(dist_bu_km, dist, 1), .(file, imp, entity_id)]
missing[, percTPO := percentTravelPeriodsOverestimated(dist_bu_km, dist, 1), .(file, imp, entity_id)]
missing[, impTPAcc := impTravelPeriodAccuracy(dist_bu_km, dist, 1), .(file, imp, entity_id)]
missing[, distOver := distanceOverestimated(dist_bu_km, dist), .(file, imp, entity_id)]
missing[, distUnder := distanceUnderestimated(dist_bu_km, dist), .(file, imp, entity_id)]

missing



res <- missing[, .SD[1], .(candidate.specificity, match.buffer, window, imp, entity_id),
               .SDcols = c("rmse", "rawbias", "rawabias", "percTPM", "percTPO", "impTPAcc", "distOver", "distUnder")]

oneimp <- res[imp == 1,
              .(mrmse = mean(rmse),
                mrbias = abs(mean(rawbias)),
                mperctpm = mean(percTPM),
                mperctpo = mean(percTPO),
                mimptpacc = mean(impTPAcc),
                meddistover = median(distOver),
                meddistunder = median(distUnder),
                meandistover = mean(distOver),
                meandistunder = mean(distUnder),
                imps = 1),
              .(candidate.specificity, match.buffer, window)]

threeimps <- res[imp %in% 1:3,
              .(mrmse = mean(rmse),
                mrbias = abs(mean(rawbias)),
                mperctpm = mean(percTPM),
                mperctpo = mean(percTPO),
                mimptpacc = mean(impTPAcc),
                meddistover = median(distOver),
                meddistunder = median(distUnder),
                meandistover = mean(distOver),
                meandistunder = mean(distUnder),
                imps = 3),
              .(candidate.specificity, match.buffer, window)]

fiveimps <- res[imp %in% 1:5,
                 .(mrmse = mean(rmse),
                   mrbias = abs(mean(rawbias)),
                   mperctpm = mean(percTPM),
                   mperctpo = mean(percTPO),
                   mimptpacc = mean(impTPAcc),
                   meddistover = median(distOver),
                   meddistunder = median(distUnder),
                   meandistover = mean(distOver),
                   meandistunder = mean(distUnder),
                   imps = 5),
                 .(candidate.specificity, match.buffer, window)]

tenimps <- res[,
               .(mrmse = mean(rmse),
                 mrbias = abs(mean(rawbias)),
                 mperctpm = mean(percTPM),
                 mperctpo = mean(percTPO),
                 mimptpacc = mean(impTPAcc),
                 meddistover = median(distOver),
                 meddistunder = median(distUnder),
                 meandistover = mean(distOver),
                 meandistunder = mean(distUnder),
                 imps = 10),
               .(candidate.specificity, match.buffer, window)]


simres1 <- rbindlist(list(
  oneimp, threeimps, fiveimps, tenimps
))

msres1 <- melt(simres1, id.vars = c("candidate.specificity", "match.buffer", "window", "imps"))
msres1 <- msres1[!variable %in% c("meddistover", "meddistunder")]
msres1 <- msres1[!candidate.specificity == "always"]

msres1[, candidate.specificity := factor(candidate.specificity, levels = c("low", "medium", "high"))]

# saveRDS(msres1, "./data/appfig1-20220109.RDS")
saveRDS(msres1, "Res/Computed/appfig1-20230215.RDS")
# New facet label names for dose variable
mb.labs <- c("MB: 1hr", "MB: 4hrs", "MB: 8hrs")
names(mb.labs) <- c("1 hr", "4 hrs", "8 hrs")

cs.labs <- c("CS: low", "CS: med", "CS: high")
names(cs.labs) <- c("low", "medium", "high")

# var.labs <- c("RMSE", "Bias", "TPM%", "TPO%", "TPAcc", "MedDO", "MedDU", "MDO", "MDU")
# names(var.labs) <- c("mrmse", "mrbias", "mperctpm", "mperctpo", "mimptpacc", "meddistover", "meddistunder", "meandistover", "meandistunder")

var.labs <- c("RMSE", "AbsBias", "TPM%", "TPO%", "TPAcc", "DistO", "DistU")
names(var.labs) <- c("mrmse", "mrbias", "mperctpm", "mperctpo", "mimptpacc", "meandistover", "meandistunder")


ggplot(msres1[imps == 10], aes(x = as.factor(match.buffer), y = value, color =candidate.specificity)) +
  geom_point() +
  geom_line(aes(group = candidate.specificity)) +
  theme_minimal() +
  scale_x_discrete("Match Buffer") +
  scale_y_continuous("") +
  facet_grid(variable ~ window,
             scales = "free_y", switch = "y",
             labeller = labeller(variable = var.labs
                                 # match.buffer = mb.labs,
                                 # candidate.specificity = cs.labs
                                 ))+
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0))


comp <- msres1[, .(candidate.specificity, match.buffer, window, imps, variable, value)]
comp[variable == "mrmse", best := min(value)]
comp[variable == "mrbias", best := min(value)]
comp[variable == "mperctpm", best := min(value)]
comp[variable == "mperctpo", best := min((value))]
comp[variable == "mimptpacc", best := max((value))]
comp[variable == "meandistover", best := min(value)]
comp[variable == "meandistunder", best := min(value)]


# Appendix figure with full results ---------------------------------------


ggplot(msres1, aes(x = as.factor(imps), y = value, color = window)) +
  geom_hline(data = comp, aes(yintercept = best), color = "grey33", linetype = "dotted",  size = 1) +
  geom_point() +
  geom_line(aes(group = window)) +
  theme_minimal() +
  scale_x_discrete("N imputations") +
  scale_y_continuous("") +
  scale_color_discrete("Time Window") +
  facet_grid(variable ~ match.buffer + candidate.specificity,
             scales = "free_y", switch = "y",
             labeller = labeller(variable = var.labs,
                                 match.buffer = mb.labs,
                                 candidate.specificity = cs.labs))+
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0),
        panel.spacing.y = unit(1.5, "lines"))

library(magrittr)

selected.vars <- c("mrmse", "mrbias", "mimptpacc")
selected.imps <- c(3, 5, 10)
selected.cs <- c("medium", "high")
selected.window <- c("within 1 hr", "within 3 hrs")

msres1[variable %in% selected.vars &
       imps %in% selected.imps &
       candidate.specificity %in% selected.cs &
       window %in% selected.window] %>%
  ggplot( aes(x = as.factor(imps), y = value, color = window)) +
  geom_hline(data = comp[variable %in% selected.vars &
                           imps %in% selected.imps &
                           candidate.specificity %in% selected.cs &
                           window %in% selected.window], aes(yintercept = best), color = "grey33", linetype = "dotted",  size = 1) +
  geom_point() +
  geom_line(aes(group = window)) +
  theme_minimal() +
  scale_x_discrete("N imputations") +
  scale_y_continuous("") +
  scale_color_discrete("Time Window") +
  facet_grid(variable ~ match.buffer + candidate.specificity,
             scales = "free_y", switch = "y",
             labeller = labeller(variable = var.labs,
                                 match.buffer = mb.labs,
                                 candidate.specificity = cs.labs))+
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0),
        panel.spacing.y = unit(1.5, "lines"))

ggplot(msres1, aes(x = factor(imps), y = value, color = window)) +
  geom_hline(data = comp, aes(yintercept = best), color = "grey", size = 1) +
  geom_point() +
  facet_grid(variable ~ match.buffer + candidate.specificity, scales = "free_y") +
  scale_x_discrete("Number of imputations")+
  theme_minimal()


# N sets ------------------------------------------------------------------

#

missing <- sims[is.na(orig_dist)]
# missing <- missing[file != "distm4w12c4imp10.RDS"]
missing <- missing[!is.na(dist)]
missing[, diff := dist_bu_km - dist]
missing[, n_sets_cut := factor(n_sets, levels = c(1, 2, 3, 4, 5, 6, 8), labels = c("1", "2-3", "2-3", "4+", "4+", "4+", "4+"))]

missing[, rmse := compute.rmse(dist, dist_bu_km), .(file, imp, entity_id, n_sets_cut)]
missing[, truedist := sum(dist_bu_km), .(file, imp, entity_id, n_sets_cut)]
missing[, impdist := sum(dist), .(file, imp, entity_id, n_sets_cut)]
missing[, rawbias := truedist - impdist]
missing[, rawabias := abs(truedist - impdist)]
missing[, percTPM := percentTravelPeriodsMissed(dist_bu_km, dist, 1), .(file, imp, entity_id, n_sets_cut)]
missing[, percTPO := percentTravelPeriodsOverestimated(dist_bu_km, dist, 1), .(file, imp, entity_id, n_sets_cut)]
missing[, impTPAcc := impTravelPeriodAccuracy(dist_bu_km, dist, 1), .(file, imp, entity_id, n_sets_cut)]
missing[, distOver := distanceOverestimated(dist_bu_km, dist), .(file, imp, entity_id, n_sets_cut)]
missing[, distUnder := distanceUnderestimated(dist_bu_km, dist), .(file, imp, entity_id, n_sets_cut)]


missing[n_sets_cut ==  "4+"][abs(rawbias) > 100, .N, devid]
missing[, .SD[1], entity_id][, mean(truedist), n_sets]
missing[, average_travel_dist := mean(truedist), devid]
missing[n_sets > 1, .(cut(average_travel_dist, breaks = 3, ))]


res <- missing[devid !=  "353", .SD[1], .(candidate.specificity, match.buffer, window, imp, entity_id, n_sets_cut),
               .SDcols = c("rmse", "rawbias", "rawabias", "percTPM", "percTPO", "impTPAcc", "distOver", "distUnder")]



oneimp <- res[imp == 1,
              .(mrmse = mean(rmse),
                mrbias = abs(mean(rawbias)),
                # mrabias = mean(rawabias),
                mperctpm = mean(percTPM),
                mperctpo = mean(percTPO),
                mimptpacc = mean(impTPAcc),
                meddistover = median(distOver),
                meddistunder = median(distUnder),
                meandistover = mean(distOver),
                meandistunder = mean(distUnder),
                imps = 1),
              .(candidate.specificity, match.buffer, window, n_sets_cut)]

threeimps <- res[imp %in% 1:3,
                 .(mrmse = mean(rmse),
                   mrbias = abs(mean(rawbias)),
                   mperctpm = mean(percTPM),
                   mperctpo = mean(percTPO),
                   mimptpacc = mean(impTPAcc),
                   meddistover = median(distOver),
                   meddistunder = median(distUnder),
                   meandistover = mean(distOver),
                   meandistunder = mean(distUnder),
                   imps = 3),
                 .(candidate.specificity, match.buffer, window, n_sets_cut)]

fiveimps <- res[imp %in% 1:5,
                .(mrmse = mean(rmse),
                  mrbias = abs(mean(rawbias)),
                  mperctpm = mean(percTPM),
                  mperctpo = mean(percTPO),
                  mimptpacc = mean(impTPAcc),
                  meddistover = median(distOver),
                  meddistunder = median(distUnder),
                  meandistover = mean(distOver),
                  meandistunder = mean(distUnder),
                  imps = 5),
                .(candidate.specificity, match.buffer, window, n_sets_cut)]

tenimps <- res[,
               .(mrmse = mean(rmse),
                 mrbias = abs(mean(rawbias)),
                 mperctpm = mean(percTPM),
                 mperctpo = mean(percTPO),
                 mimptpacc = mean(impTPAcc),
                 meddistover = median(distOver),
                 meddistunder = median(distUnder),
                 meandistover = mean(distOver),
                 meandistunder = mean(distUnder),
                 imps = 10),
               .(candidate.specificity, match.buffer, window, n_sets_cut)]


simres1 <- rbindlist(list(
  oneimp, threeimps, fiveimps, tenimps
))

msres1 <- melt(simres1, id.vars = c("candidate.specificity", "match.buffer", "window", "imps", "n_sets_cut"))
msres1 <- msres1[!variable %in% c("mnmae", "mpbiasmov")]
msres1 <- msres1[!candidate.specificity == "always"]

msres1[, candidate.specificity := factor(candidate.specificity, levels = c("low", "medium", "high"))]
msres1[, window := factor(window, levels = c("within 1 hr", "within 3 hrs", "no restriction"), labels = c("1 hr", "3 hrs", "none"))]

selected.vars <- c("mrbias",  "mimptpacc",  "mperctpo", "meandistover", "mperctpm", "meandistunder")
selected.imps <- c(1)
selected.cs <- c("medium", "high")
selected.window <- c("within 1 hr", "within 3 hrs", "no restriction")

comp <- msres1[, .(candidate.specificity, match.buffer, window, imps, variable, value, n_sets_cut)]
comp[variable == "mrbias" &
       imps %in% selected.imps &
       candidate.specificity %in% selected.cs &
       window %in% selected.window, best := min(value)]
comp[variable == "mimptpacc" &
       imps %in% selected.imps &
       candidate.specificity %in% selected.cs &
       window %in% selected.window, best := max(value)]
comp[variable == "mperctpm" &
       imps %in% selected.imps &
       candidate.specificity %in% selected.cs &
       window %in% selected.window, best := min((value))]



# Number of sets figure ---------------------------------------------------


library(viridis)
msres1[
  variable %in% selected.vars &
    imps %in% selected.imps &
    candidate.specificity %in% selected.cs] %>%
  ggplot( aes(x = window, y = value, color = candidate.specificity)) +
  geom_hline(data = comp[
    variable %in% selected.vars &
      imps %in% selected.imps &
      candidate.specificity %in% selected.cs], aes(yintercept = best), color = "grey33", linetype = "dotted",  size = 1) +
  geom_point(size = 3) +
  geom_line(aes(group = candidate.specificity)) +
  theme_minimal(base_size = 12) +
  scale_x_discrete("Time Window") +
  scale_y_continuous("") +
  scale_color_viridis("Candidate Specificity", discrete = TRUE, option = "D") +
  facet_grid(variable ~ n_sets_cut + match.buffer,
             scales = "free_y", switch = "y",
             labeller = labeller(variable = var.labs,
                                 match.buffer = mb.labs,
                                 candidate.specificity = cs.labs))+
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0),
        panel.spacing.y = unit(1.5, "lines"))

msres1[
  variable %in% selected.vars &
    imps %in% selected.imps &
    candidate.specificity %in% selected.cs &
    window %in% selected.window] %>%
  ggplot( aes(x = window, y = value, color = n_sets_cut)) +
  geom_hline(data = comp[
    variable %in% selected.vars &
      imps %in% selected.imps &
      candidate.specificity %in% selected.cs &
      window %in% selected.window], aes(yintercept = best), color = "grey33", linetype = "dotted",  size = 1) +
  geom_point(size = 3) +
  geom_line(aes(group = n_sets_cut)) +
  theme_minimal(base_size = 12) +
  scale_x_discrete("Time Window") +
  scale_y_continuous("") +
  scale_color_viridis("Sets", discrete = TRUE, option = "D") +
  facet_grid(variable ~ match.buffer + candidate.specificity,
             scales = "free_y", switch = "y",
             labeller = labeller(variable = var.labs,
                                 match.buffer = mb.labs,
                                 candidate.specificity = cs.labs))+
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0),
        panel.spacing.y = unit(1.5, "lines"))



# Length of missingness ---------------------------------------------------

missing <- sims[is.na(orig_dist)]
# missing <- missing[file != "distm4w12c4imp10.RDS"]
missing <- missing[!is.na(dist)]
missing[, diff := dist_bu_km - dist]
missing[, nmiss := .N, .(entity_id, file, imp)]
missing[, nmiss_cut := cut(nmiss, breaks = c(2, 8, 16, 24, 40, 49), include.lowest = TRUE, ordered_result = TRUE,
                           labels = c(".5-2h", "2h-4h", "4h-8h", "8h-10h", "10-12h"))]

missing[, fa2 := compute.fa2(dist, dist_bu_km), .(file, imp, entity_id, nmiss_cut)]
missing[, fb := compute.fb(dist, dist_bu_km), .(file, imp, entity_id, nmiss_cut)]
missing[, fsd := new_compute.fsd(dist, dist_bu_km), .(file, imp, entity_id, nmiss_cut)]
missing[, nmae := compute.nmae(dist, dist_bu_km), .(file, imp, entity_id, nmiss_cut)]
missing[, rmse := compute.rmse(dist, dist_bu_km), .(file, imp, entity_id, nmiss_cut)]
missing[, rmse2 := sqrt(1/.N * sum((dist_bu_km - dist) ^ 2)), .(file, imp, entity_id, nmiss_cut) ]
missing[, mae := sum(abs(dist_bu_km - dist))/.N, .(file, imp, entity_id, nmiss_cut) ]
missing[, sim := compute.sim(dist, dist_bu_km), .(file, imp, entity_id, nmiss_cut)]
missing[, truedist := sum(dist_bu_km), .(file, imp, entity_id, nmiss_cut)]
missing[, rawbias := sum(dist) - sum(dist_bu_km), .(file, imp, entity_id, nmiss_cut)]
missing[truedist > 2, percbiasMOVING := 100 * (abs(rawbias)/truedist), .(file, imp, entity_id, nmiss_cut)]
missing[rawbias == 0, percbiasMOVING := 0]
res <- missing[, .SD[1], .(candidate.specificity, match.buffer, window, imp, entity_id),
               .SDcols = c("imp", "entity_id", "candidate.specificity", "match.buffer", "window", "fa2",
                           "fb", "fsd", "nmae", "rmse", "mae", "sim", "rawbias", "percbiasMOVING", "nmiss_cut")]

oneimp <- res[imp == 1,
              .(mfa2 = mean(fa2),
                mfb = mean(fb),
                mfsd = mean(fsd),
                mnmae = mean(nmae),
                mrmse = mean(rmse),
                msim = mean(sim),
                mrbias = mean(rawbias),
                mpbiasmov = mean(percbiasMOVING, na.rm = TRUE),
                imps = 1),
              .(candidate.specificity, match.buffer, window, nmiss_cut)]

threeimps <- res[imp %in% 1:3,
                 .(mfa2 = mean(fa2),
                   mfb = mean(fb),
                   mfsd = mean(fsd),
                   mnmae = mean(nmae),
                   mrmse = mean(rmse),
                   msim = mean(sim),
                   mrbias = mean(rawbias),
                   mpbiasmov = mean(percbiasMOVING, na.rm = TRUE),
                   imps = 3),
                 .(candidate.specificity, match.buffer, window, nmiss_cut)]

fiveimps <- res[imp %in% 1:5,
                .(mfa2 = mean(fa2),
                  mfb = mean(fb),
                  mfsd = mean(fsd),
                  mnmae = mean(nmae),
                  mrmse = mean(rmse),
                  msim = mean(sim),
                  mrbias = mean(rawbias),
                  mpbiasmov = mean(percbiasMOVING, na.rm = TRUE),
                  imps = 5),
                .(candidate.specificity, match.buffer, window, nmiss_cut)]


tenimps <- res[,
               .(mfa2 = mean(fa2),
                 mfb = mean(fb),
                 mfsd = mean(fsd),
                 mnmae = mean(nmae),
                 mrmse = mean(rmse),
                 msim = mean(sim),
                 mrbias = mean(rawbias),
                 mpbiasmov = mean(percbiasMOVING, na.rm = TRUE),
                 imps = 10),
               .(candidate.specificity, match.buffer, window, nmiss_cut)]


simres1 <- rbindlist(list(
  oneimp, threeimps, fiveimps, tenimps
))

msres1 <- melt(simres1, id.vars = c("candidate.specificity", "match.buffer", "window", "imps", "nmiss_cut"))
msres1 <- msres1[!variable %in% c("mnmae", "mpbiasmov")]
msres1 <- msres1[!candidate.specificity == "always"]

msres1[, candidate.specificity := factor(candidate.specificity, levels = c("low", "medium", "high"))]



selected.vars <- c("mrbias", "mrmse", "mfa2")
selected.imps <- c(5)
selected.cs <- c("medium", "high")
selected.window <- c("within 1 hr", "within 3 hrs")

comp <- msres1[, .(candidate.specificity, match.buffer, window, imps, variable, value, nmiss_cut)]
comp[
  variable == "mfa2" &
    imps %in% selected.imps &
    candidate.specificity %in% selected.cs &
    window %in% selected.window,
  best := max(value)]
comp[
  variable == "mrmse" &
    imps %in% selected.imps &
    candidate.specificity %in% selected.cs &
    window %in% selected.window,
  best := min(value)]
comp[
  variable == "mrbias" &
       imps %in% selected.imps &
       candidate.specificity %in% selected.cs &
       window %in% selected.window,
  best := min(abs(value))]
msres1[
  variable %in% selected.vars &
    imps %in% selected.imps &
    candidate.specificity %in% selected.cs
    # window %in% selected.window
    ] %>%
  ggplot( aes(x = window, y = value, color = nmiss_cut)) +
  geom_hline(data = comp[
    variable %in% selected.vars &
      imps %in% selected.imps &
      candidate.specificity %in% selected.cs &
      window %in% selected.window], aes(yintercept = best), color = "grey33", linetype = "dotted",  size = 1) +
  geom_point(size = 3) +
  geom_line(aes(group = nmiss_cut)) +
  theme_minimal(base_size = 12) +
  scale_x_discrete("Time Window") +
  scale_y_continuous("") +
  scale_color_viridis("Gap length", discrete = TRUE, option = "D") +
  facet_grid(variable ~ match.buffer + candidate.specificity,
             scales = "free_y", switch = "y",
             labeller = labeller(variable = var.labs,
                                 match.buffer = mb.labs,
                                 candidate.specificity = cs.labs))+
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0),
        panel.spacing.y = unit(1.5, "lines"),
        axis.text.x.bottom = element_text(angle = 90))


# Percent night -----------------------------------------------------------


missing <- sims[is.na(orig_dist)]
missing <- missing[!is.na(dist)]
missing[, diff := dist_bu_km - dist]
missing[, night := FALSE]
missing[hour(starttime) >= 20 | hour(starttime) <= 5, night := TRUE]
missing[, perc_night := sum(night)/.N, .(file, entity_id, imp)]
missing[, perc_night_cut := cut(perc_night, breaks = c(0, .1, .4,  .6, .9, 1.001), ordered = TRUE, include.lowest = TRUE, labels = c("Day", "Mostly day", "Day and night", "Mostly night", "Night"))]

missing[, fa2 := compute.fa2(dist, dist_bu_km), .(file, imp, entity_id, perc_night_cut)]
missing[, fb := compute.fb(dist, dist_bu_km), .(file, imp, entity_id, perc_night_cut)]
missing[, fsd := new_compute.fsd(dist, dist_bu_km), .(file, imp, entity_id, perc_night_cut)]
missing[, nmae := compute.nmae(dist, dist_bu_km), .(file, imp, entity_id, perc_night_cut)]
missing[, rmse := compute.rmse(dist, dist_bu_km), .(file, imp, entity_id, perc_night_cut)]
missing[, rmse2 := sqrt(1/.N * sum((dist_bu_km - dist) ^ 2)), .(file, imp, entity_id, perc_night_cut) ]
missing[, mae := sum(abs(dist_bu_km - dist))/.N, .(file, imp, entity_id, perc_night_cut) ]
missing[, sim := compute.sim(dist, dist_bu_km), .(file, imp, entity_id, perc_night_cut)]
missing[, truedist := sum(dist_bu_km), .(file, imp, entity_id, perc_night_cut)]
missing[, rawbias := sum(dist) - sum(dist_bu_km), .(file, imp, entity_id, perc_night_cut)]
missing[truedist > 2, percbiasMOVING := 100 * (abs(rawbias)/truedist), .(file, imp, entity_id, perc_night_cut)]
missing[rawbias == 0, percbiasMOVING := 0]
res <- missing[, .SD[1], .(candidate.specificity, match.buffer, window, imp, entity_id),
               .SDcols = c("imp", "entity_id", "candidate.specificity", "match.buffer", "window", "fa2",
                           "fb", "fsd", "nmae", "rmse", "mae", "sim", "rawbias", "percbiasMOVING", "perc_night_cut")]

oneimp <- res[imp == 1,
              .(mfa2 = mean(fa2),
                mfb = mean(fb),
                mfsd = mean(fsd),
                mnmae = mean(nmae),
                mrmse = mean(rmse),
                msim = mean(sim),
                mrbias = mean(rawbias),
                mpbiasmov = mean(percbiasMOVING, na.rm = TRUE),
                imps = 1),
              .(candidate.specificity, match.buffer, window, perc_night_cut)]

threeimps <- res[imp %in% 1:3,
                 .(mfa2 = mean(fa2),
                   mfb = mean(fb),
                   mfsd = mean(fsd),
                   mnmae = mean(nmae),
                   mrmse = mean(rmse),
                   msim = mean(sim),
                   mrbias = mean(rawbias),
                   mpbiasmov = mean(percbiasMOVING, na.rm = TRUE),
                   imps = 3),
                 .(candidate.specificity, match.buffer, window, perc_night_cut)]

fiveimps <- res[imp %in% 1:5,
                .(mfa2 = mean(fa2),
                  mfb = mean(fb),
                  mfsd = mean(fsd),
                  mnmae = mean(nmae),
                  mrmse = mean(rmse),
                  msim = mean(sim),
                  mrbias = mean(rawbias),
                  mpbiasmov = mean(percbiasMOVING, na.rm = TRUE),
                  imps = 5),
                .(candidate.specificity, match.buffer, window, perc_night_cut)]


tenimps <- res[,
               .(mfa2 = mean(fa2),
                 mfb = mean(fb),
                 mfsd = mean(fsd),
                 mnmae = mean(nmae),
                 mrmse = mean(rmse),
                 msim = mean(sim),
                 mrbias = mean(rawbias),
                 mpbiasmov = mean(percbiasMOVING, na.rm = TRUE),
                 imps = 10),
               .(candidate.specificity, match.buffer, window, perc_night_cut)]


simres1 <- rbindlist(list(
  oneimp, threeimps, fiveimps, tenimps
))

msres1 <- melt(simres1, id.vars = c("candidate.specificity", "match.buffer", "window", "imps", "perc_night_cut"))
msres1 <- msres1[!variable %in% c("mnmae", "mpbiasmov")]
msres1 <- msres1[!candidate.specificity == "always"]

msres1[, candidate.specificity := factor(candidate.specificity, levels = c("low", "medium", "high"))]



selected.vars <- c("mrbias", "mrmse")
selected.imps <- c(10)
selected.cs <- c("medium", "high")
selected.window <- c("within 1 hr", "within 3 hrs")

comp <- msres1[, .(candidate.specificity, match.buffer, window, imps, variable, value, perc_night_cut)]
comp[
  variable == "mfa2" &
    imps %in% selected.imps &
    candidate.specificity %in% selected.cs &
    window %in% selected.window,
  best := max(value)]
comp[
  variable == "mrmse" &
    imps %in% selected.imps &
    candidate.specificity %in% selected.cs &
    window %in% selected.window,
  best := min(value)]
comp[
  variable == "mrbias" &
    imps %in% selected.imps &
    candidate.specificity %in% selected.cs &
    window %in% selected.window,
  best := min(abs(value))]
msres1[
  variable %in% selected.vars &
    imps %in% selected.imps &
    candidate.specificity %in% selected.cs &
  # window %in% selected.window
] %>%
  ggplot( aes(x = window, y = value, color = perc_night_cut)) +
  geom_hline(data = comp[
    variable %in% selected.vars &
      imps %in% selected.imps &
      candidate.specificity %in% selected.cs &
      window %in% selected.window], aes(yintercept = best), color = "grey33", linetype = "dotted",  size = 1) +
  geom_point(size = 3) +
  geom_line(aes(group = perc_night_cut)) +
  theme_minimal(base_size = 12) +
  scale_x_discrete("Time Window") +
  scale_y_continuous("") +
  scale_color_viridis("Time of missingness", discrete = TRUE, option = "D") +
  facet_grid(variable ~ match.buffer + candidate.specificity,
             scales = "free_y", switch = "y",
             labeller = labeller(variable = var.labs,
                                 match.buffer = mb.labs,
                                 candidate.specificity = cs.labs))+
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0),
        panel.spacing.y = unit(1.5, "lines"),
        axis.text.x.bottom = element_text(angle = 90))




# Tables ------------------------------------------------------------------

sing <- readRDS("Data/paper3.appendsing.RDS")

missing <- sims[is.na(orig_dist)]
missing <- missing[!is.na(dist)]
missing[, diff := dist_bu_km - dist]
msres1[, window := factor(window, levels = c("within 1 hr", "within 3 hrs", "no restriction"))]

missing[, fa2 := compute.fa2(dist, dist_bu_km), .(file, imp, entity_id)]
missing[, fb := compute.fb(dist, dist_bu_km), .(file, imp, entity_id)]
missing[, fsd := new_compute.fsd(dist, dist_bu_km), .(file, imp, entity_id)]
missing[, nmae := compute.nmae(dist, dist_bu_km), .(file, imp, entity_id)]
missing[, rmse := compute.rmse(dist, dist_bu_km), .(file, imp, entity_id)]
missing[, rmse2 := sqrt(1/.N * sum((dist_bu_km - dist) ^ 2)), .(file, imp, entity_id) ]
missing[, mae := sum(abs(dist_bu_km - dist))/.N, .(file, imp, entity_id) ]
missing[, sim := compute.sim(dist, dist_bu_km), .(file, imp, entity_id)]
missing[, truedist := sum(dist_bu_km), .(file, imp, entity_id)]
missing[, impdist := sum(dist), .(file, imp, entity_id)]
missing[, rawbias := impdist - truedist]

missing[truedist > 2, percbiasMOVING := 100 * (abs(rawbias)/truedist), .(file, imp, entity_id)]
missing[rawbias == 0, percbiasMOVING := 0]
res <- missing[, .SD[1], .(candidate.specificity, match.buffer, window, imp, entity_id),
               .SDcols = c("imp", "entity_id", "candidate.specificity", "match.buffer", "window", "fa2",
                           "fb", "fsd", "nmae", "rmse", "mae", "sim", "rawbias", "percbiasMOVING")]

oneimp <- res[imp == 1,
              .(mfa2 = mean(fa2),
                mfb = mean(fb),
                mfsd = mean(fsd),
                mnmae = mean(nmae),
                mrmse = mean(rmse),
                msim = mean(sim),
                mrbias = mean(rawbias),
                mpbiasmov = mean(percbiasMOVING, na.rm = TRUE),
                imps = 1),
              .(candidate.specificity, match.buffer, window)]

threeimps <- res[imp %in% 1:3,
                 .(mfa2 = mean(fa2),
                   mfb = mean(fb),
                   mfsd = mean(fsd),
                   mnmae = mean(nmae),
                   mrmse = mean(rmse),
                   msim = mean(sim),
                   mrbias = mean(rawbias),
                   mpbiasmov = mean(percbiasMOVING, na.rm = TRUE),
                   imps = 3),
                 .(candidate.specificity, match.buffer, window)]

fiveimps <- res[imp %in% 1:5,
                .(mfa2 = mean(fa2),
                  mfb = mean(fb),
                  mfsd = mean(fsd),
                  mnmae = mean(nmae),
                  mrmse = mean(rmse),
                  msim = mean(sim),
                  mrbias = mean(rawbias),
                  mpbiasmov = mean(percbiasMOVING, na.rm = TRUE),
                  imps = 5),
                .(candidate.specificity, match.buffer, window)]


tenimps <- res[,
               .(mfa2 = mean(fa2),
                 mfb = mean(fb),
                 mfsd = mean(fsd),
                 mnmae = mean(nmae),
                 mrmse = mean(rmse),
                 msim = mean(sim),
                 mrbias = mean(rawbias),
                 mpbiasmov = mean(percbiasMOVING, na.rm = TRUE),
                 imps = 10),
               .(candidate.specificity, match.buffer, window)]


simres1 <- rbindlist(list(
  oneimp, threeimps, fiveimps, tenimps
))
simres1 <- simres1[candidate.specificity != "always"]
simres1[, .(candidate.specificity, match.buffer, window, imps, mfa2, mfsd, mnmae, mrmse, msim, mrbias)] %>%
  melt(id.vars = c("mfa2", "mfsd", "mrmse", "mnmae", "msim", "mrbias")) -> mtab
mtab2 <- mtab[, .(
  fa2 = mean(mfa2),
  fsd = mean(mfsd),
  rmse = mean(mrmse),
  nmae = mean(mnmae),
  sim = mean(msim),
  bias = abs(mean(mrbias))
), .(variable, value)]
setnames(mtab2, c(".id", "Parameter", "fa2", "fsd", "rmse", "nmae", "sim", "Bias (km)"))
mtab2[, .id := c("Candidate Specificity", "Candidate Specificity", "Candidate Specificity",
                      "Match Buffer", "Match Buffer", "Match Buffer",
                      "Time Window", "Time Window", "Time Window",
                      "Imputations", "Imputations", "Imputations", "Imputations")]

mtab2[, Parameter := c("Low", "Medium", "High", "4 hours", "8 hours", "1 hour",
                       "No Window", "< 1 hour", "< 3 hours", 1, 3, 5, 10)]
mtab2[, Parameter := factor(Parameter, levels = c("Low", "Medium", "High", "1 hour", "4 hours", "8 hours", "< 1 hour", "< 3 hours", "No Window", "1", "3", "5", "10"), ordered = TRUE)]
saveRDS(mtab2, "Res/Computed/mtab2-20220109.RDS")
# library(gt)
gt(mtab2[order(Parameter), .SD, .SDcols = c(".id", "Parameter", "fa2", "fsd", "rmse", "sim", "Bias (km)")], groupname_col = ".id", rowname_col = "Parameter") %>%
  gt::fmt_number(columns = c("fa2", "fsd"), decimals = 3) %>%
  gt::fmt_number(columns = c("rmse"), decimals = 3) %>%
  gt::fmt_number(columns = c("Bias (km)"), decimals = 1) %>%
  gt::fmt_percent(columns = "sim", decimals = 1) %>%
  gt::tab_style(locations = cells_body(rows = c(3, 4, 9, 10), columns = "fa2"), style = cell_fill()) %>%
  gt::tab_style(locations = cells_body(rows = c(3, 4, 9, 10), columns = "fsd"), style = cell_fill()) %>%
  gt::tab_style(locations = cells_body(rows = c(2, 6, 7, 10), columns = "rmse"), style = cell_fill()) %>%
  gt::tab_style(locations = cells_body(rows = c(2, 4, 7, 10), columns = "sim"), style = cell_fill()) %>%
  gt::tab_style(locations = cells_body(rows = c(3, 4, 9, 11), columns = "Bias (km)"), style = cell_fill()) %>%
  tab_header("Table 4: Comparison among possible parameter values")

msres1 <- melt(simres1, id.vars = c("candidate.specificity", "match.buffer", "window", "imps"))
msres1 <- msres1[!variable %in% c("mnmae", "mpbiasmov")]
msres1 <- msres1[!candidate.specificity == "always"]

msres1[, candidate.specificity := factor(candidate.specificity, levels = c("low", "medium", "high"))]


# New facet label names for dose variable
mb.labs <- c("MB: 1hr", "MB: 4hrs", "MB: 8hrs")
names(mb.labs) <- c("1 hr", "4 hrs", "8 hrs")

cs.labs <- c("CS: low", "CS: med", "CS: high")
names(cs.labs) <- c("low", "medium", "high")

var.labs <- c("FA2", "FB", "FSD", "RMSE", "sim.", "bias")
names(var.labs) <- c("mfa2", "mfb", "mfsd", "mrmse", "msim", "mrbias")


