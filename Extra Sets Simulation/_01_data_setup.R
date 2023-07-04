library(data.table)

setDTthreads(percent = 80)

# Load original data ------------------------------------------------------

ddata <-
  readRDS(
    "data/nonvarmiss-15022023.RDS"
  )

ddata[, missing := FALSE]
ddata[, orig_dist := dist]
ddata[, c("devid", "set") := tstrsplit(entity_id, "-")]
ddata[, n_sets := uniqueN(entity_id), .(.id, devid)]
ddata[, n_sets := uniqueN(entity_id), devid]

# Data 1 ------------------------------------------------------------------

set.seed(46731204)

extrasets <- ddata[n_sets < 4, sample(unique(entity_id), 16*7)]
extrasets1 <- extrasets[1:16] # Available reference for sims 1, 2, 3
extrasets2 <- extrasets[17:32] # Available reference for sims 1, 2
extrasets3 <- extrasets[33:48] # Available reference for sim 1
alwayspresent <- extrasets[-c(1:48)] # Available reference for sims 1, 2, 3, 4

ordered_sets <- ddata[n_sets >= 4, sample(unique(entity_id), 4, replace = FALSE), devid]
ordered_sets[, order := 1:.N, devid]
ddata[ordered_sets, on = .(entity_id == V1), order := i.order]

data1 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1, extrasets2, extrasets3) | order == 1)])
data2 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1, extrasets2) | order %in% c(1, 2))])
data3 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1) | order %in% c(1, 2, 3))])
data4 <- copy(ddata[(entity_id %in% c(alwayspresent)) | (order %in% c(1, 2, 3, 4))])

saveRDS(data1,  "Data 1/d1-data1.RDS")
saveRDS(data2,  "Data 1/d1-data2.RDS")
saveRDS(data3,  "Data 1/d1-data3.RDS")
saveRDS(data4,  "Data 1/d1-data4.RDS")



# Data 2 ------------------------------------------------------------------

ddata[, order := NULL]

set.seed(75747572)
extrasets <- ddata[n_sets < 4, sample(unique(entity_id), 16*7)]
extrasets1 <- extrasets[1:16] # Available reference for sims 1, 2, 3
extrasets2 <- extrasets[17:32] # Available reference for sims 1, 2
extrasets3 <- extrasets[33:48] # Available reference for sim 1
alwayspresent <- extrasets[-c(1:48)] # Available reference for sims 1, 2, 3, 4

ordered_sets <- ddata[n_sets >= 4, sample(unique(entity_id), 4, replace = FALSE), devid]
ordered_sets[, order := 1:.N, devid]
ddata[ordered_sets, on = .(entity_id == V1), order := i.order]

data1 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1, extrasets2, extrasets3) | order == 1)])
data2 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1, extrasets2) | order %in% c(1, 2))])
data3 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1) | order %in% c(1, 2, 3))])
data4 <- copy(ddata[(entity_id %in% c(alwayspresent)) | (order %in% c(1, 2, 3, 4))])

saveRDS(data1,  "Data 2/d2-data1.RDS")
saveRDS(data2,  "Data 2/d2-data2.RDS")
saveRDS(data3,  "Data 2/d2-data3.RDS")
saveRDS(data4,  "Data 2/d2-data4.RDS")


# Data 3 ------------------------------------------------------------------

ddata[, order := NULL]

set.seed(3193048120)
extrasets <- ddata[n_sets < 4, sample(unique(entity_id), 16*7)]
extrasets1 <- extrasets[1:16] # Available reference for sims 1, 2, 3
extrasets2 <- extrasets[17:32] # Available reference for sims 1, 2
extrasets3 <- extrasets[33:48] # Available reference for sim 1
alwayspresent <- extrasets[-c(1:48)] # Available reference for sims 1, 2, 3, 4

ordered_sets <- ddata[n_sets >= 4, sample(unique(entity_id), 4, replace = FALSE), devid]
ordered_sets[, order := 1:.N, devid]
ddata[ordered_sets, on = .(entity_id == V1), order := i.order]

data1 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1, extrasets2, extrasets3) | order == 1)])
data2 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1, extrasets2) | order %in% c(1, 2))])
data3 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1) | order %in% c(1, 2, 3))])
data4 <- copy(ddata[(entity_id %in% c(alwayspresent)) | (order %in% c(1, 2, 3, 4))])

saveRDS(data1,  "Data 3/d3-data1.RDS")
saveRDS(data2,  "Data 3/d3-data2.RDS")
saveRDS(data3,  "Data 3/d3-data3.RDS")
saveRDS(data4,  "Data 3/d3-data4.RDS")

# Data 4 ------------------------------------------------------------------

ddata[, order := NULL]

set.seed(51405140)
extrasets <- ddata[n_sets < 4, sample(unique(entity_id), 16*7)]
extrasets1 <- extrasets[1:16] # Available reference for sims 1, 2, 3
extrasets2 <- extrasets[17:32] # Available reference for sims 1, 2
extrasets3 <- extrasets[33:48] # Available reference for sim 1
alwayspresent <- extrasets[-c(1:48)] # Available reference for sims 1, 2, 3, 4

ordered_sets <- ddata[n_sets >= 4, sample(unique(entity_id), 4, replace = FALSE), devid]
ordered_sets[, order := 1:.N, devid]
ddata[ordered_sets, on = .(entity_id == V1), order := i.order]

data1 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1, extrasets2, extrasets3) | order == 1)])
data2 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1, extrasets2) | order %in% c(1, 2))])
data3 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1) | order %in% c(1, 2, 3))])
data4 <- copy(ddata[(entity_id %in% c(alwayspresent)) | (order %in% c(1, 2, 3, 4))])

saveRDS(data1,  "Data 4/d4-data1.RDS")
saveRDS(data2,  "Data 4/d4-data2.RDS")
saveRDS(data3,  "Data 4/d4-data3.RDS")
saveRDS(data4,  "Data 4/d4-data4.RDS")

# Data 5 ------------------------------------------------------------------

ddata[, order := NULL]

set.seed(424018502)
extrasets <- ddata[n_sets < 4, sample(unique(entity_id), 16*7)]
extrasets1 <- extrasets[1:16] # Available reference for sims 1, 2, 3
extrasets2 <- extrasets[17:32] # Available reference for sims 1, 2
extrasets3 <- extrasets[33:48] # Available reference for sim 1
alwayspresent <- extrasets[-c(1:48)] # Available reference for sims 1, 2, 3, 4

ordered_sets <- ddata[n_sets >= 4, sample(unique(entity_id), 4, replace = FALSE), devid]
ordered_sets[, order := 1:.N, devid]
ddata[ordered_sets, on = .(entity_id == V1), order := i.order]

data1 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1, extrasets2, extrasets3) | order == 1)])
data2 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1, extrasets2) | order %in% c(1, 2))])
data3 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1) | order %in% c(1, 2, 3))])
data4 <- copy(ddata[(entity_id %in% c(alwayspresent)) | (order %in% c(1, 2, 3, 4))])

saveRDS(data1,  "Data 5/d5-data1.RDS")
saveRDS(data2,  "Data 5/d5-data2.RDS")
saveRDS(data3,  "Data 5/d5-data3.RDS")
saveRDS(data4,  "Data 5/d5-data4.RDS")

# Data 6 ------------------------------------------------------------------

ddata[, order := NULL]

set.seed(3420058394)
extrasets <- ddata[n_sets < 4, sample(unique(entity_id), 16*7)]
extrasets1 <- extrasets[1:16] # Available reference for sims 1, 2, 3
extrasets2 <- extrasets[17:32] # Available reference for sims 1, 2
extrasets3 <- extrasets[33:48] # Available reference for sim 1
alwayspresent <- extrasets[-c(1:48)] # Available reference for sims 1, 2, 3, 4

ordered_sets <- ddata[n_sets >= 4, sample(unique(entity_id), 4, replace = FALSE), devid]
ordered_sets[, order := 1:.N, devid]
ddata[ordered_sets, on = .(entity_id == V1), order := i.order]

data1 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1, extrasets2, extrasets3) | order == 1)])
data2 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1, extrasets2) | order %in% c(1, 2))])
data3 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1) | order %in% c(1, 2, 3))])
data4 <- copy(ddata[(entity_id %in% c(alwayspresent)) | (order %in% c(1, 2, 3, 4))])

saveRDS(data1,  "Data 6/d6-data1.RDS")
saveRDS(data2,  "Data 6/d6-data2.RDS")
saveRDS(data3,  "Data 6/d6-data3.RDS")
saveRDS(data4,  "Data 6/d6-data4.RDS")

# Data 7 ------------------------------------------------------------------

ddata[, order := NULL]

set.seed(34810731132)
extrasets <- ddata[n_sets < 4, sample(unique(entity_id), 16*7)]
extrasets1 <- extrasets[1:16] # Available reference for sims 1, 2, 3
extrasets2 <- extrasets[17:32] # Available reference for sims 1, 2
extrasets3 <- extrasets[33:48] # Available reference for sim 1
alwayspresent <- extrasets[-c(1:48)] # Available reference for sims 1, 2, 3, 4

ordered_sets <- ddata[n_sets >= 4, sample(unique(entity_id), 4, replace = FALSE), devid]
ordered_sets[, order := 1:.N, devid]
ddata[ordered_sets, on = .(entity_id == V1), order := i.order]

data1 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1, extrasets2, extrasets3) | order == 1)])
data2 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1, extrasets2) | order %in% c(1, 2))])
data3 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1) | order %in% c(1, 2, 3))])
data4 <- copy(ddata[(entity_id %in% c(alwayspresent)) | (order %in% c(1, 2, 3, 4))])

saveRDS(data1,  "Data 7/d7-data1.RDS")
saveRDS(data2,  "Data 7/d7-data2.RDS")
saveRDS(data3,  "Data 7/d7-data3.RDS")
saveRDS(data4,  "Data 7/d7-data4.RDS")


# Data 8 ------------------------------------------------------------------

ddata[, order := NULL]

set.seed(800858923)
extrasets <- ddata[n_sets < 4, sample(unique(entity_id), 16*7)]
extrasets1 <- extrasets[1:16] # Available reference for sims 1, 2, 3
extrasets2 <- extrasets[17:32] # Available reference for sims 1, 2
extrasets3 <- extrasets[33:48] # Available reference for sim 1
alwayspresent <- extrasets[-c(1:48)] # Available reference for sims 1, 2, 3, 4

ordered_sets <- ddata[n_sets >= 4, sample(unique(entity_id), 4, replace = FALSE), devid]
ordered_sets[, order := 1:.N, devid]
ddata[ordered_sets, on = .(entity_id == V1), order := i.order]

data1 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1, extrasets2, extrasets3) | order == 1)])
data2 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1, extrasets2) | order %in% c(1, 2))])
data3 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1) | order %in% c(1, 2, 3))])
data4 <- copy(ddata[(entity_id %in% c(alwayspresent)) | (order %in% c(1, 2, 3, 4))])

saveRDS(data1,  "Data 8/d8-data1.RDS")
saveRDS(data2,  "Data 8/d8-data2.RDS")
saveRDS(data3,  "Data 8/d8-data3.RDS")
saveRDS(data4,  "Data 8/d8-data4.RDS")


# Data 9 ------------------------------------------------------------------

ddata[, order := NULL]

set.seed(57939383)
extrasets <- ddata[n_sets < 4, sample(unique(entity_id), 16*7)]
extrasets1 <- extrasets[1:16] # Available reference for sims 1, 2, 3
extrasets2 <- extrasets[17:32] # Available reference for sims 1, 2
extrasets3 <- extrasets[33:48] # Available reference for sim 1
alwayspresent <- extrasets[-c(1:48)] # Available reference for sims 1, 2, 3, 4

ordered_sets <- ddata[n_sets >= 4, sample(unique(entity_id), 4, replace = FALSE), devid]
ordered_sets[, order := 1:.N, devid]
ddata[ordered_sets, on = .(entity_id == V1), order := i.order]

data1 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1, extrasets2, extrasets3) | order == 1)])
data2 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1, extrasets2) | order %in% c(1, 2))])
data3 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1) | order %in% c(1, 2, 3))])
data4 <- copy(ddata[(entity_id %in% c(alwayspresent)) | (order %in% c(1, 2, 3, 4))])

saveRDS(data1,  "Data 9/d9-data1.RDS")
saveRDS(data2,  "Data 9/d9-data2.RDS")
saveRDS(data3,  "Data 9/d9-data3.RDS")
saveRDS(data4,  "Data 9/d9-data4.RDS")

# Data 10 -----------------------------------------------------------------

ddata[, order := NULL]

set.seed(5647382911)
extrasets <- ddata[n_sets < 4, sample(unique(entity_id), 16*7)]
extrasets1 <- extrasets[1:16] # Available reference for sims 1, 2, 3
extrasets2 <- extrasets[17:32] # Available reference for sims 1, 2
extrasets3 <- extrasets[33:48] # Available reference for sim 1
alwayspresent <- extrasets[-c(1:48)] # Available reference for sims 1, 2, 3, 4

ordered_sets <- ddata[n_sets >= 4, sample(unique(entity_id), 4, replace = FALSE), devid]
ordered_sets[, order := 1:.N, devid]
ddata[ordered_sets, on = .(entity_id == V1), order := i.order]

data1 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1, extrasets2, extrasets3) | order == 1)])
data2 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1, extrasets2) | order %in% c(1, 2))])
data3 <- copy(ddata[(entity_id %in% c(alwayspresent, extrasets1) | order %in% c(1, 2, 3))])
data4 <- copy(ddata[(entity_id %in% c(alwayspresent)) | (order %in% c(1, 2, 3, 4))])

saveRDS(data1,  "Data 10/d10-data1.RDS")
saveRDS(data2,  "Data 10/d10-data2.RDS")
saveRDS(data3,  "Data 10/d10-data3.RDS")
saveRDS(data4,  "Data 10/d10-data4.RDS")