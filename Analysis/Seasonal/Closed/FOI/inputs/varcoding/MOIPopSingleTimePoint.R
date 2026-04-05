rm(list = ls())
suppressPackageStartupMessages({
  library(dplyr)
})
roundIndex <- 3
arrivalType <- "exponential"
seasonality <- "seasonal"
openness <- "closed"
folder <- "eLifeSubAugust2024"
readDir0 <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/", folder, "/round", roundIndex, "/files/", arrivalType, "/FOI/inputs/MOIIndVarcoding/MOIEst/")
readDir1 <- paste0(readDir0, seasonality, "/")
readDir2 <- paste0(readDir1, openness, "/")

saveDir0 <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/", folder, "/round", roundIndex, "/files/", arrivalType, "/FOI/inputs/MOIPopVarcodingSingleTimePoint/")
if (!dir.exists(saveDir0)) {
  dir.create(saveDir0)
}
saveDir1 <- paste0(saveDir0, seasonality, "/")
if (!dir.exists(saveDir1)) {
  dir.create(saveDir1)
}
saveDir2 <- paste0(saveDir1, openness, "/")
if (!dir.exists(saveDir2)) {
  dir.create(saveDir2)
}

prefix <- "sim"
nums <- 1:3
preIRS <- 200
T_YEAR <- 360
ageCutoff <- 5
ageGroupLabel <- "0-5yrs"
nums_w_reps <- NULL
nRealizations <- 200
for (i in 1:length(nums)) {
  num <- nums[i]
  saveDir3 <- paste0(saveDir2, prefix, "_", num, "/")
  if (!dir.exists(saveDir3)) {
    dir.create(saveDir3)
  }
  if (num %in% nums_w_reps) {
    reps <- 0:2
  } else {
    reps <- 0
  }
  if (num == min(nums)) {
    layersList <- list(c((preIRS - 2)*T_YEAR + 300, (preIRS - 1)*T_YEAR + 180), 
                       c((preIRS + 1)*T_YEAR + 300, (preIRS + 2)*T_YEAR + 180))
    layersLabels <- list(c("Pre-IRS Wet Season", "Pre-IRS Dry Season"),
                         c("IRS Wet Season", "IRS Dry Season"))
  } else {
    layersList <- list(c((preIRS + 1)*T_YEAR + 300, (preIRS + 2)*T_YEAR + 180))
    layersLabels <- list(c("IRS Wet Season", "IRS Dry Season"))
  }
  for (r in reps) {
    for (j in 1:length(layersList)) {
      layers <- layersList[[j]]
      layersLabel <- layersLabels[[j]]
      for (k in 1:length(layers)) {
        layer <- layers[k]
        layerLabel <- layersLabel[k]
        MOIAll <- NULL
        for (nRealization in 1:nRealizations) {
          load(paste0(readDir2, "MOI_", num, "_rep", r, "_realization", nRealization, ".RData"))
          MOIAll <- bind_rows(MOIAll, dfAll %>% filter(time %in% layer, age <= ageCutoff))
        }
        MOIAllFinal <- MOIAll %>% group_by(MOI) %>% summarise(n = n()) %>% ungroup() %>% 
          mutate(n_total = sum(n), Prob = n/n_total, N = n_total/nRealizations) %>% select(-c(n, n_total))
        write.csv(MOIAllFinal, file = paste0(saveDir3, "r", r, "_", layerLabel, "_", ageGroupLabel, ".csv"), row.names = F)
      }
    }
  }
}
