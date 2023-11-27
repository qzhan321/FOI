rm(list = ls())
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(cowplot)
  library(stringr)
  library(RSQLite)
  library(readxl)
  library(xlsx)
})

T_YEAR <- 360
ageThreshold <- 5
nums <- 1:9
numsLabels <- c("Equal Size", "Equal Size", "Equal Size",
                "Equal Size", "Equal Size", "Equal Size",
                "Unequal Size", "Unequal Size", "Unequal Size")
heterogeneities <- c("Homogeneous Risk", "Homogeneous Risk", "Homogeneous Risk",
                     "Heterogeneous Risk", "Heterogeneous Risk", "Heterogeneous Risk",
                     "Heterogeneous Risk", "Heterogeneous Risk", "Heterogeneous Risk")

runs <- c(5, 7, 8)
runsLabels <- c("Exponential", "Gamma", "Uniform")
readDir <- "/project2/pascualmm/QZ/PhD/projects/FOI/files"
seasonality <- c("seasonal", "non-seasonal")
openness <- c("closed", "semi-open", "regionally-open")
MOI_states_pairs <- list(c("withME", "true"), c("withME-varcoding", "true"), c("withME-varcoding", "withME"))
MOI_states_pairs_labels <- list(c("Bayesian Varcoding", "True"), c("Varcoding", "True"), c("Varcoding", "Bayesian Varcoding"))
performanceAll <- NULL
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
      runsLabel <- runsLabels[i]
      if (s == "non-seasonal" & runsLabel != "Exponential") next
      for (j in 1:length(nums)) {
        num <- nums[j]
        numsLabel <- numsLabels[j]
        heterogeneity <- heterogeneities[j]
        if (o != "semi-open" & heterogeneity == "Heterogeneous Risk") next
        if (num %in% c(1,4,7)) {
          if (s == "seasonal") {
            times <- list(c((preIRS - 1)*T_YEAR + 180, (preIRS - 1)*T_YEAR + 300),
                          c((preIRS + 1)*T_YEAR + 180, (preIRS + 1)*T_YEAR + 300))
          } else {
            times <- list(c((preIRS - 1)*T_YEAR + 180, preIRS*T_YEAR),
                          c((preIRS + 1)*T_YEAR + 180, (preIRS + 2)*T_YEAR))
          }
          IRSPhases <- c("Pre-IRS", "Low-coverage IRS")
        } else {
          if (s == "seasonal") {
            times <- list(c((preIRS + 1)*T_YEAR + 180, (preIRS + 1)*T_YEAR + 300))
          } else {
            times <- list(c((preIRS + 1)*T_YEAR + 180, (preIRS + 2)*T_YEAR))
          }
          if (num %in% c(2,5,8)) {
            IRSPhases <- c("Mid-coverage IRS")
          } else {
            IRSPhases <- c("High-coverage IRS")
          }
        }
        for (k in 1:length(IRSPhases)) {
          IRSPhase <- IRSPhases[k]
          t <- times[[k]]
          
          for (m in 1:length(MOI_states_pairs)) {
            MOI_states <- MOI_states_pairs[[m]]
            MOI_states_labels <- MOI_states_pairs_labels[[m]]
            
            MOI_state <- MOI_states[1]
            file <- paste0(readDir, run, "/actualRuns/MOI/", s, "/", o, "/",
                           MOI_state, "/MOI_", num, "_rep0.RData")
            load(file)
            if (MOI_state %in% c("true", "withME-varcoding")) {
              MOI1 <- dfAll %>% filter(time %in% t, age <= ageThreshold) %>% select(time, host_id, pop_id, MOI) 
            } else {
              temp0 <- dfAll %>% filter(time %in% t, age <= ageThreshold)
              temp1 <- temp0 %>% group_by(time, MOI) %>% summarise(n = sum(p))
              MOI1 <- temp1 %>% group_by(time) %>% summarise(MOI = rep(MOI, round(n)))
            }
            
            MOI_state <- MOI_states[2]
            file <- paste0(readDir, run, "/actualRuns/MOI/", s, "/", o, "/",
                           MOI_state, "/MOI_", num, "_rep0.RData")
            load(file)
            if (MOI_state %in% c("true", "withME-varcoding")) {
              MOI2 <- dfAll %>% filter(time %in% t, age <= ageThreshold) %>% select(time, host_id, pop_id, MOI) 
            } else {
              temp0 <- dfAll %>% filter(time %in% t, age <= ageThreshold)
              temp1 <- temp0 %>% group_by(time, MOI) %>% summarise(n = sum(p))
              MOI2 <- temp1 %>% group_by(time) %>% summarise(MOI = rep(MOI, round(n)))
            }
            meandiff <- mean(MOI2$MOI) - mean(MOI1$MOI)
            
            F1 <- ecdf(MOI1$MOI)
            F2 <- ecdf(MOI2$MOI)
            x <- c(MOI1$MOI, MOI2$MOI)
            kst <- max(abs(F1(x) - F2(x)))
            performance <- data.frame("KS_statistics" = kst, "comparison" = paste0(MOI_states_labels[1], " vs ", MOI_states_labels[2]), 
                                      "seasonality" = s, "seasonLabel" = ifelse(s=="seasonal","Seasons Combined","NA"), 
                                      "openness" = o, "Arrivals" = runsLabel, "RiskGroupSize" = numsLabel, 
                                      "TransmissionHeterogeneity" = heterogeneity,
                                      "IRSPhase" = IRSPhase, "meanMOIDiff" = meandiff)
            performanceAll <- rbind(performanceAll, performance)
            
            if (s == "seasonal") {
              MOI1_low <- MOI1 %>% filter(time%%T_YEAR == 180)
              MOI2_low <- MOI2 %>% filter(time%%T_YEAR == 180)
              meandiff_low <- mean(MOI2_low$MOI) - mean(MOI1_low$MOI)    
              F1 <- ecdf(MOI1_low$MOI)
              F2 <- ecdf(MOI2_low$MOI)
              x <- c(MOI1_low$MOI, MOI2_low$MOI)
              kst <- max(abs(F1(x) - F2(x)))
              performance <- data.frame("KS_statistics" = kst, "comparison" = paste0(MOI_states_labels[1], " vs ", MOI_states_labels[2]), 
                                        "seasonality" = s, "seasonLabel" = "Low/Dry Season", 
                                        "openness" = o, "Arrivals" = runsLabel, "RiskGroupSize" = numsLabel, 
                                        "TransmissionHeterogeneity" = heterogeneity,
                                        "IRSPhase" = IRSPhase, "meanMOIDiff" = meandiff_low)
              performanceAll <- rbind(performanceAll, performance)
              
              MOI1_high <- MOI1 %>% filter(time%%T_YEAR == 300)
              MOI2_high <- MOI2 %>% filter(time%%T_YEAR == 300)
              meandiff_high <- mean(MOI2_high$MOI) - mean(MOI1_high$MOI)
              F1 <- ecdf(MOI1_high$MOI)
              F2 <- ecdf(MOI2_high$MOI)
              x <- c(MOI1_high$MOI, MOI2_high$MOI)
              kst <- max(abs(F1(x) - F2(x)))
              performance <- data.frame("KS_statistics" = kst, "comparison" = paste0(MOI_states_labels[1], " vs ", MOI_states_labels[2]), 
                                        "seasonality" = s, "seasonLabel" = "High/Wet Season", 
                                        "openness" = o, "Arrivals" = runsLabel, "RiskGroupSize" = numsLabel, 
                                        "TransmissionHeterogeneity" = heterogeneity,
                                        "IRSPhase" = IRSPhase, "meanMOIDiff" = meandiff_high)
              performanceAll <- rbind(performanceAll, performance)
            }
          }
        }
      }
    }
  }
}

saveDir <- "/project2/pascualmm/QZ/PhD/projects/FOI/writings/round1/files/MOImethodsPerformance/"
dir.create(saveDir)
write.xlsx(performanceAll, file = paste0(saveDir, "MOImethodsPerformance.xlsx"),
           sheetName = "Sheet1", col.names = T, row.names = F, append = F)

