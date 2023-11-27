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
MOI_states <- c("true", "withME")
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
            } else {
              labels <- c("High-coverage IRS")
            }
            if (s == "seasonal") {
              times <- list(c((preIRS + 1)*T_YEAR  + 180, (preIRS + 1)*T_YEAR + 300))
            } else {
              times <- list(c((preIRS + 1)*T_YEAR  + 180, (preIRS + 2)*T_YEAR))
            }
          }
          for (m in 1:length(labels)) {
            l <- labels[m]
            t <- times[[m]]
            for (n in 1:length(MOI_states)) {
              MOI_state <- MOI_states[n]
              file <- paste0(readDir, run, "/", type, "/MOI/", s, "/",
                             o, "/", MOI_state, "/MOI_", num, "_rep", rep, ".RData")
              load(file)
              
              dfAllSub <- dfAll %>% filter(time %in% t, age <= ageCutoff) %>% mutate(IRSPhase = l) 
              if (MOI_state == "withME") {
                temp1 <- dfAllSub %>% group_by(time, MOI) %>% summarise(n = sum(p))
                df1_temp <- temp1 %>% group_by(time) %>% summarize(MOI = rep(MOI, round(n))) # %>% filter(MOI > 0)
              } else {
                df1_temp <- dfAllSub
              }
              N <- nrow(df1_temp)
              score <- (N-1)*var(df1_temp$MOI)/mean(df1_temp$MOI)
              score
              pvalue <- pchisq(score, N-1, lower.tail = FALSE)
              pvalue_df <- data.frame("p-value" = pvalue, "type" = "simulation", "seasonality" = s, 
                                      "openness" = o, "Arrivals" = runLabel, 
                                      "RiskGroupSize" = numsLabel, "TransmissionHeterogeneity" = heterogeneity,
                                      "IRSPhase" = l, "seasonLabel" = ifelse(s=="seasonal", "Seasons Combined", "NA"),
                                      "MOIState" = MOI_state)
              pvalue_dfAll <- rbind(pvalue_dfAll, pvalue_df)
              if (s == "seasonal") {
                df2_temp <- df1_temp 
                low_s <- df2_temp %>% filter(time%%T_YEAR == 180)
                  
                N <- nrow(low_s)
                score <- (N-1)*var(low_s$MOI)/mean(low_s$MOI)
                score
                pvalue <- pchisq(score, N-1, lower.tail = FALSE)
                pvalue_df <- data.frame("p-value" = pvalue, "type" = "simulation", "seasonality" = s, 
                                        "openness" = o, "Arrivals" = runLabel, 
                                        "RiskGroupSize" = numsLabel, "TransmissionHeterogeneity" = heterogeneity,
                                        "IRSPhase" = l, "seasonLabel" = "Low/Dry Season",
                                        "MOIState" = MOI_state)
                pvalue_dfAll <- rbind(pvalue_dfAll, pvalue_df)
                  
                high_s <- df2_temp %>% filter(time%%T_YEAR == 300)
                N <- nrow(high_s)
                score <- (N-1)*var(high_s$MOI)/mean(high_s$MOI)
                score
                pvalue <- pchisq(score, N-1, lower.tail = FALSE)
                pvalue_df <- data.frame("p-value" = pvalue, "type" = "simulation", "seasonality" = s, 
                                        "openness" = o, "Arrivals" = runLabel, 
                                        "RiskGroupSize" = numsLabel, "TransmissionHeterogeneity" = heterogeneity,
                                        "IRSPhase" = l, "seasonLabel" = "High/Wet Season",
                                        "MOIState" = MOI_state)
                pvalue_dfAll <- rbind(pvalue_dfAll, pvalue_df)
              }
            }
          }
        }
      }
    }
  }
}
pvalue_dfAll <- pvalue_dfAll %>% mutate(MOIState = ifelse(MOIState == "true", "True", "With Measurement Error"))
write.xlsx(pvalue_dfAll, file = paste0(saveDir1, "deviationFromPoissonTest.xlsx"),
           sheetName = "Sheet1", col.names = T, row.names = F, append = F)


# Ghana
rm(list = ls())
readDir <- "/project2/pascualmm/QZ/PhD/projects/FOI/files/Ghana/MOI/mixtureDist/"
surveys <- list(1,2,1:2,5,6,5:6,7)
surveyLabels <- list("Pre-IRS Wet \n(2012, S1)",
                     "Pre-IRS Dry \n(2013, S2)",
                     "Pre-IRS \n(2012 and 2013, S1 and S2)",
                     "Right Post-IRS Wet \n(2015, S3)", 
                     "Right Post-IRS Dry \n(2016, S4)",
                     "Right Post-IRS \n(2015 and 2016, S3 and S4)",
                     "Post-IRS/SMC Wet \n(2017, S5)")
IRSPhases <- c("Pre-IRS", "Pre-IRS", "Pre-IRS", "Right Post-IRS", "Right Post-IRS",
               "Right Post-IRS", "Post-IRS/SMC")
seasons <- c("High/Wet Season", "Low/Dry Season", "Seasons Combined", 
             "High/Wet Season", "Low/Dry Season", "Seasons Combined",
             "High/Wet Season")
prefix <- "survey"
type <- "Ghana"
saveDir0 <- "/project2/pascualmm/QZ/PhD/projects/FOI/writings/"
dir.create(saveDir0)
round <- "round1"
saveDir1 <- paste0(saveDir0, round, "/files/devFromPoisson/")
dir.create(saveDir1)

pvalue_dfAll <- NULL
for (i in 1:length(surveys)) {
  survey <- surveys[[i]]
  if (identical(survey, 7)) {
    ageL <- "Children (6-10 years)"
  } else {
    ageL <- "Children (1-5 years)"
  }
  surveyLabel <- surveyLabels[[i]]
  IRSPhase <- IRSPhases[i]
  season <- seasons[i]
  
  dfAll2Sub <- NULL
  for (j in 1:length(survey)) {
    survey_s <- survey[j]
    file <- paste0(readDir, prefix, "_", survey_s, ".RData")
    load(file)
    dfAll2Sub <- rbind(dfAll2Sub, dfAll2 %>% filter(age %in% ageL)) 
  }
  
  temp1 <- dfAll2Sub %>% group_by(MOI) %>% summarise(n = sum(p))
  temp2 <- rep(temp1$MOI, round(temp1$n)) 
  
  N <- length(temp2)
  score <- (N-1)*var(temp2)/mean(temp2)
  score
  pvalue <- pchisq(score, N-1, lower.tail = FALSE)
  pvalue_df <- data.frame("p-value" = pvalue, "type" = type, 
                          "survey" = ifelse(length(survey) == 1, paste0(prefix, "_", survey), paste0(prefix, "_", survey[1], "_and_", survey[2])), 
                          "IRSPhase" = IRSPhase, "seasonLabel" = season, "surveyLabel" = surveyLabel)
  pvalue_dfAll <- rbind(pvalue_dfAll, pvalue_df)
}
write.xlsx(pvalue_dfAll, file = paste0(saveDir1, "deviationFromPoissonTest.xlsx"),
           sheetName = "Sheet2", col.names = T, row.names = F, append = T)



