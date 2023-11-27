rm(list = ls())
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(ggpubr)
  library(cowplot)
  library(ggsci)
  library(Hmisc)
  library(tidyr)
  library(colorspace)
})
# sim
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
runs <- c("", 3, 4)
runsLabels <- c("Exponential", "Gamma High Variance", "Uniform")
readDir <- "/project2/pascualmm/QZ/PhD/projects/FOI/files"
seasonality <- c("seasonal", "non-seasonal")
openness <- c("closed", "semi-open", "regionally-open")
MOI_states <- c("true", "withME")
numsList <- list(1,2,3,4,5,6,7,8,9)
numsLabels <- c("Equal Size", "Equal Size", "Equal Size",
                "Equal Size", "Equal Size", "Equal Size",
                "Unequal Size", "Unequal Size", "Unequal Size")
heterogeneities <- c("Homogeneous Risk", "Homogeneous Risk", "Homogeneous Risk",
                     "Heterogeneous Risk", "Heterogeneous Risk", "Heterogeneous Risk",
                     "Heterogeneous Risk", "Heterogeneous Risk", "Heterogeneous Risk")
type <- "actualRuns"
methods <- c("twoMoments")
prefix <- "sim"
rep <- 0
N <- 10000
T_YEAR <- 360
ageCutoff <- 5
sizeV <- 18
sizeVFactor <- 1.175
saveDir0 <- "/project2/pascualmm/QZ/PhD/projects/FOI/writings/"
dir.create(saveDir0)
round <- "round1"
saveDir1 <- paste0(saveDir0, round, "/files/varianceTwoMomentsChebyshevInequality/")
dir.create(saveDir1)
vardfAll_sim <- NULL
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
      if (s == "non-seasonal" & run != runs[1]) next
      for (j in 1:length(numsList)) {
        nums <- numsList[[j]]
        numsLabel <- numsLabels[j]
        heterogeneity <- heterogeneities[j]
        
        for (k in 1:length(nums)) {
          num <- nums[k]
          if (o != "semi-open" & num > 3) next
          numLabel <- numsLabel[k]
          if (num %in% c(1,4,7)) {
            labels <- c("Pre-IRS", "Low-coverage IRS")
            labelsSimple <- c("Pre-IRS", "IRS")
            if (s == "seasonal") {
              times <- list(c((preIRS - 1)*T_YEAR  + 180, (preIRS - 1)*T_YEAR + 300),
                            c((preIRS + 1)*T_YEAR  + 180, (preIRS + 1)*T_YEAR + 300))
            } else {
              times <- list(c((preIRS - 1)*T_YEAR  + 180, preIRS*T_YEAR),
                            c((preIRS + 1)*T_YEAR  + 180, (preIRS + 2)*T_YEAR))
            }
          } else {
            if (num %in% c(2,5,8)) {
              labels <- c("Mid-coverage IRS")
              labelsSimple <- c("IRS")
            } else {
              labels <- c("High-coverage IRS")
              labelsSimple <- c("IRS")
            }
            if (s == "seasonal") {
              times <- list(c((preIRS + 1)*T_YEAR  + 180, (preIRS + 1)*T_YEAR + 300))
            } else {
              times <- list(c((preIRS + 1)*T_YEAR  + 180, (preIRS + 2)*T_YEAR))
            }
          }
          for (m in 1:length(labels)) {
            l <- labels[m]
            ls <- labelsSimple[m]
            t <- times[[m]]
            
            for (n in 1:length(MOI_states)) {
              MOI_state <- MOI_states[n]
              for (p in 1:length(methods)) {
                method <- methods[p]
                readDirFinal <- paste0(readDir, run, "/", type, "/FOI/", method, "/", s, "/", o, "/", MOI_state, "/")
                fileName <- paste0(prefix, "_", num, "_rep", rep, "_", ls, "_", MOI_state, ".RData")
                if (!(file.exists(paste0(readDirFinal, fileName)))) {
                  fileName <- paste0(prefix, "_", num, "rep_", rep, "_", ls, "_", MOI_state, ".RData")
                }
                load(paste0(readDirFinal, fileName))
                
                if (method == "twoMoments") {
                  temp0 <- FOI_est %>%
                    arrange(likelihood) %>%
                    slice(seq_len(1))
                  temp1 <- data.frame("meanInterarrival" = temp0$meanInterarrival,
                                      "T_YEAR" = T_YEAR,
                                      "FOI_est" = T_YEAR/temp0$meanInterarrival,
                                      "varInterarrival_est" = temp0$VarInterArrival,
                                      "seasonality" = firstup(s), 
                                      "openness" = firstup(o), "Arrivals" = runLabel, 
                                      "RiskGroupSize" = numsLabel, "TransmissionHeterogeneity" = heterogeneity,
                                      "IRSPhase" = l,
                                      "p_missing" = NA)
                } 
                vardfAll_sim <- rbind(vardfAll_sim, temp1)
              }
            }
          }
        }
      }
    }
  }
}


# Ghana
run <- ""
readDir <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/files", run, "/Ghana/FOI/")
prefix <- "survey"
T_YEAR <- 365
surveyIndices <- list(1:2, 5:6)
surveyLabels <- list("Pre-IRS \n(2012 and 2013, S1 and S2)",
                     "Right Post-IRS \n(2015 and 2016, S3 and S4)")
IRSPhases <- c("Pre-IRS", 
               "Right Post-IRS")
seasons <- c("Annual", "Annual")
methods <- c("twoMoments")
types <- c("annual")
p_missing <- c(0, 0.05, 0.10)
vardfAll_Ghana <- NULL
for (i in c(1,2)) {
  surveyIndex <- surveyIndices[[i]]
  surveyLabel <- surveyLabels[[i]]
  seasonLabel <- seasons[i]
  IRSPhase <- IRSPhases[i]
  for (j in 1:length(methods)) {
    method <- methods[j]
    for (k in 1:length(types)) {
      type <- types[k]
      for (m in 1:length(p_missing)) {
        p_missing_single <- p_missing[m]
        readDirFinal <- paste0(readDir, method, "/", type, "/p_missing_", p_missing_single, "/")
        fileName <- paste0(prefix, "_", surveyIndex[1], "_and_", surveyIndex[2], ".RData")
        load(paste0(readDirFinal, fileName))
        if (method == "twoMoments") {
          temp0 <- FOI_est %>%
            arrange(likelihood) %>%
            slice(seq_len(1))
          temp1 <- data.frame("meanInterarrival" = temp0$meanInterarrival,
                              "T_YEAR" = T_YEAR,
                              "FOI_est" = T_YEAR/temp0$meanInterarrival,
                              "varInterarrival_est" = temp0$VarInterArrival,
                              "seasonality" = "Ghana", 
                              "openness" = "Ghana", "Arrivals" = "Ghana", 
                              "RiskGroupSize" = "Ghana", "TransmissionHeterogeneity" ="Ghana",
                              "IRSPhase" = IRSPhase, "p_missing" = p_missing_single)
        } 
        vardfAll_Ghana <- rbind(vardfAll_Ghana, temp1)
      }
    }
  }
}

vardfAll <- rbind(vardfAll_sim %>% mutate(state = "sim"), vardfAll_Ghana %>% mutate(state = "Ghana"))
vardfAll$seasonality <- factor(vardfAll$seasonality, levels = c("Seasonal", "Non-seasonal", "Ghana"))
vardfAll$openness <- factor(vardfAll$openness, levels = c("Closed", "Semi-open", "Regionally-open", "Ghana"))
vardfAll$Arrivals <- factor(vardfAll$Arrivals, levels = c("Exponential", "Gamma High Variance", "Uniform", "Ghana"))
vardfAll$RiskGroupSize <- factor(vardfAll$RiskGroupSize, levels = c("Equal Size", "Unequal Size", "Ghana"))
vardfAll$TransmissionHeterogeneity <- factor(vardfAll$TransmissionHeterogeneity, levels = c("Homogeneous Risk", "Heterogeneous Risk", "Ghana"))
vardfAll$IRSPhase <- factor(vardfAll$IRSPhase, levels = c("Pre-IRS", "Low-coverage IRS",
                                                          "Mid-coverage IRS", "High-coverage IRS",
                                                          "Right Post-IRS"))
vardfAll <- vardfAll %>% mutate(p_missing_state = case_when(
  (p_missing == "NA") ~ "NA",
  (p_missing == 0) ~ "High Detectability",
  (p_missing == 0.05) ~ "Mid Detectability",
  (p_missing == 0.10) ~ "Low Detectability"
))
vardfAll$p_missing_state <- factor(vardfAll$p_missing_state, levels = c("High Detectability", "Mid Detectability", "Low Detectability", "NA"))
vardfAll2 <- vardfAll %>% mutate(lowerBound = T_YEAR / (meanInterarrival + 2*sqrt(varInterarrival_est)), 
                                 upperBound = T_YEAR / (meanInterarrival - 2*sqrt(varInterarrival_est)))
