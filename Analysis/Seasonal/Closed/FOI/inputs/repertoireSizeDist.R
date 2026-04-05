rm(list = ls())
suppressPackageStartupMessages({
  library(dplyr)
})
roundIndex <- 3
arrivalType <- "exponential"
folder <- "eLifeSubAugust2024"
dir <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/", folder, "/round", roundIndex, "/files/", arrivalType, "/FOI/inputs/MOIInd/")
saveDir <- paste0(dir, "repertoireSizeDist/")
if (!dir.exists(saveDir)) {
  dir.create(saveDir)
}

df <- read.table(paste0(dir, "measurementError/NbVarGenes_MOI1_upsBC_AllSurveys_Weight.txt"), header = T)

repertoireSizeDist <- df %>% select(DBLa_upsBC_rep_size, n)
write.csv(repertoireSizeDist, file = paste0(saveDir, "repertoireSizeDistribution.csv"), row.names = F)

