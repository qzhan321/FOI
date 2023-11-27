# tesing for deviation from Poisson homogeneity.
rm(list = ls())
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(stringr)
  library(ggpubr)
  library(cowplot)
  library(RSQLite)
  library(readxl)
  library(xlsx)
})
readDir <- "/project2/pascualmm/QZ/PhD/projects/FOI/files"
runs <- c(5, 7, 8)
runsLabels <- c("Exponential", "Gamma", "Uniform")
seasonality <- c("seasonal", "non-seasonal")
openness <- c("closed", "semi-open", "regionally-open")
MOI_states <- c("true")
numsList <- list(1,2,3,4,5,6,7,8,9)
numsLabels <- c("Equal Size", "Equal Size", "Equal Size",
                "Equal Size", "Equal Size", "Equal Size",
                "Unequal Size", "Unequal Size", "Unequal Size")
heterogeneities <- c("Homogeneous Risk", "Homogeneous Risk", "Homogeneous Risk",
                     "Heterogeneous Risk", "Heterogeneous Risk", "Heterogeneous Risk",
                     "Heterogeneous Risk", "Heterogeneous Risk", "Heterogeneous Risk")
type <- "actualRuns"
method <- "twoMoments"
prefix <- "sim"
rep <- 0
T_YEAR <- 360
ageCutoff <- 5
sizeV <- 18
sizeVFactor <- 1.175
saveDir0 <- "/project2/pascualmm/QZ/PhD/projects/FOI/writings/"
dir.create(saveDir0)
round <- "round1"
saveDir1 <- paste0(saveDir0, round, "/files/devFromPoisson/")
dir.create(saveDir1)
pvalue_dfAll <- NULL
for (a in 1:length(seasonality)) {
  s <- seasonality[a]
  for (b in 1:length(openness)) {
    o <- openness[b]
    if (o == "closed") {
      preIRS <- 200
    } else {
      preIRS <- 150
    }
    for (i in 1:length(runs)) {
      run <- runs[i]
      runLabel <- runsLabels[i]
      if (s == "non-seasonal" & runLabel != "Exponential") next
      for (j in 1:length(numsList)) {
        nums <- numsList[[j]]
        numsLabel <- numsLabels[j]
        heterogeneity <- heterogeneities[j]
        if (o != "semi-open" & heterogeneity == "Heterogeneous Risk") next
        for (k in 1:length(nums)) {
          num <- nums[k]
          numLabel <- numsLabel[k]
          if (num %in% c(1,4,7)) {
            labels <- c("Pre-IRS", "Low-coverage IRS")
            labelsSimple <- c("Pre-IRS", "IRS")
          } else {
            if (num %in% c(2,5,8)) {
              labels <- c("Mid-coverage IRS")
              labelsSimple <- c("IRS")
            } else {
              labels <- c("High-coverage IRS")
              labelsSimple <- c("IRS")
            }
          }
          for (m in 1:length(labels)) {
            l <- labels[m]
            ls <- labelsSimple[m]
            for (n in 1:length(MOI_states)) {
              MOI_state <- MOI_states[n]
              file <- paste0(readDir, run, "/", type, "/FOI/", method, "/", s, "/",
                             o, "/", MOI_state, "/", prefix, "_", num, "_rep", rep, "_", ls, "_", MOI_state, ".RData")
              if (!file.exists(file)) {
                file <- paste0(readDir, run, "/", type, "/FOI/", method, "/", s, "/",
                               o, "/", MOI_state, "/", prefix, "_", num, "rep_", rep, "_", ls, "_", MOI_state, ".RData")
              }
              load(file)
              
              df1_temp <- dursEmpi
              N <- length(df1_temp)
              score <- (N-1)*var(df1_temp)/mean(df1_temp)
              score
              pvalue <- pchisq(score, N-1, lower.tail = FALSE)
              pvalue_df <- data.frame("p-value" = pvalue, "type" = "simulation", "seasonality" = s, 
                                      "openness" = o, "Arrivals" = runLabel, 
                                      "RiskGroupSize" = numsLabel, "TransmissionHeterogeneity" = heterogeneity,
                                      "IRSPhase" = l)
              pvalue_dfAll <- rbind(pvalue_dfAll, pvalue_df)
            }
          }
        }
      }
    }
  }
}
write.xlsx(pvalue_dfAll, file = paste0(saveDir1, "deviationFromPoissonTest.xlsx"),
           sheetName = "Sheet3", col.names = T, row.names = F, append = T)


# Ghana
rm(list = ls())
saveDir0 <- "/project2/pascualmm/QZ/PhD/projects/FOI/writings/"
dir.create(saveDir0)
round <- "round1"
saveDir1 <- paste0(saveDir0, round, "/files/devFromPoisson/")
dir.create(saveDir1)
dur_excel <- read_excel("~/others/PhD/projects/FOI/utils/MalariaTherapy_Smith_OpenMalaria_Subset.xlsx")
dursEmpi <- dur_excel$LASTPOSDAY
N <- length(dursEmpi)
score <- (N-1)*var(dursEmpi)/mean(dursEmpi)
score
pvalue <- pchisq(score, N-1, lower.tail = FALSE)
pvalue_df <- data.frame("p-value" = pvalue, "type" = "Ghana")
write.xlsx(pvalue_df, file = paste0(saveDir1, "deviationFromPoissonTest.xlsx"),
           sheetName = "Sheet4", col.names = T, row.names = F, append = T)

