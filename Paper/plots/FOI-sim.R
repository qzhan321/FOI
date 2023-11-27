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
runs <- c(5, 7, 8)
runsLabels <- c("Exponential", "Gamma", "Uniform")
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
methods <- c("twoMoments", "littleslaw")
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
saveDir1 <- paste0(saveDir0, round, "/plots/FOI/")
dir.create(saveDir1)
cols <- c("#440154FF", "#21908CFF", "#FDE725FF")
cols <- c("#FF410DFF", "#6EE2FFFF", "#F7C530FF")
cols <- c("#0073C2FF", "#EFC000FF", "#868686FF")
for (a in 1:length(seasonality)) {
  s <- seasonality[a]
  saveDir2 <- paste0(saveDir1, s, "/")
  dir.create(saveDir2)
  for (b in 1:length(openness)) {
    o <- openness[b]
    saveDir3 <- paste0(saveDir2, o, "/")
    dir.create(saveDir3)
    if (o == "closed") {
      preIRS <- 200
    } else {
      preIRS <- 150
    }
    for (i in 1:length(runs)) {
      run <- runs[i]
      runLabel <- runsLabels[i]
      if (s == "non-seasonal" & runLabel != "Exponential") next
      saveDir4 <- paste0(saveDir3, runLabel, "/")
      dir.create(saveDir4)
      for (j in 1:length(numsList)) {
        nums <- numsList[[j]]
        numsLabel <- numsLabels[j]
        heterogeneity <- heterogeneities[j]
        if (o != "semi-open" & heterogeneity == "Heterogeneous Risk") next
        
        for (k in 1:length(nums)) {
          num <- nums[k]
          numLabel <- numsLabel[k]
          saveDir5 <- paste0(saveDir4, prefix, "_", num, "/")
          dir.create(saveDir5)
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
            saveDir6 <- paste0(saveDir5, l, "/")
            dir.create(saveDir6)
            
            FOIdf <- NULL
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
                  temp1 <- data.frame("FOI_mean_est" = T_YEAR/temp0$meanInterarrival,
                                      "varInterarrival_est" = temp0$VarInterArrival,
                                      "MOI_state" = MOI_state,
                                      "seasonality" = capitalize(s),
                                      "method" = method,
                                      "openness" = capitalize(o),
                                      "IRSPhase" = l)
                } else {
                  temp1 <- data.frame("FOI_mean_est" = FOI_est_single,
                                      "varInterarrival_est" = NA, 
                                      "MOI_state" = MOI_state,
                                      "seasonality" = capitalize(s),
                                      "method" = method,
                                      "openness" = capitalize(o),
                                      "IRSPhase" = l)
                }
                FOIdf <- rbind(FOIdf, temp1)
              }
              if (o == "semi-open") {
                if (numsLabel == "Equal Size") {
                  FOI_mean <- (FOI_actual1 + FOI_actual2)/(N*2)
                } else if (numsLabel == "Unequal Size") {
                  FOI_mean <- (FOI_actual1 + FOI_actual2)/(N*1.5)
                }
              } else {
                FOI_mean <- FOI_actual/N 
              } 
              temp1 <- data.frame("FOI_mean_est" = FOI_mean,
                                  "varInterarrival_est" = NA,
                                  "MOI_state" = MOI_state,
                                  "seasonality" = capitalize(s),
                                  "method" = "True",
                                  "openness" = capitalize(o),
                                  "IRSPhase" = l)
              FOIdf <- rbind(FOIdf, temp1)
            }
            FOIdf <- FOIdf %>% mutate(method = case_when(
              (method == "twoMoments") ~ "Two-Moment Approximation",
              (method == "littleslaw") ~ "Little's Law",
              (method == "True") ~ "True"
            ))
            FOIdf$method <- factor(FOIdf$method, levels = c("Two-Moment Approximation", "Little's Law", "True"))
            FOIdf <- FOIdf %>% mutate(MOI_state = ifelse(MOI_state == "true", "True MOI", "With Measurement Error"))
            p1<-ggplot(FOIdf, aes(x=IRSPhase, y = FOI_mean_est, col = method, fill = method))+
              geom_bar(stat = "identity", position = "dodge") +
              facet_grid(.~MOI_state) +
              xlab("") + ylab("True and Estimated Mean FOI \nper Host per Year") +
              theme_classic() + theme(
                plot.title = element_text(color="black", size=sizeV, hjust = 0.5),
                axis.title.x = element_text(color="black", size=sizeV),
                axis.title.y = element_text(color="black", size=sizeV),
                axis.text.x = element_text(color="black", 
                                           size=sizeV, angle=0),
                axis.text.y = element_text(color="black", 
                                           size=sizeV, angle=0),
                legend.text = element_text(color="black", 
                                           size=sizeV/sizeVFactor^2, angle=0),
                legend.title = element_text(color="black", 
                                            size=sizeV/sizeVFactor^2, angle=0),
                strip.text = element_text(color="black", 
                                          size=sizeV/sizeVFactor, angle=0)) + 
              theme(legend.position = c(0.5, 0.875)) +
              coord_cartesian(ylim = c(0, max(FOIdf$FOI_mean_est)*1.38)) + 
              scale_fill_manual(name = "", values = cols,
                                labels = c("Two-Moment Approximation", "Little's Law", "True")) +
              scale_color_manual(name = "", values = cols,
                                labels = c("Two-Moment Approximation", "Little's Law", "True")) + 
              guides(color = "none", fill = guide_legend(nrow = 1))
            ggsave(paste0(saveDir6, "FOI_withME_and_true", ".pdf"), p1, width = 7, height = 4)
          }
        }
      }
    }
  }
}