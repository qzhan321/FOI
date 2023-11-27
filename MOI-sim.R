rm(list = ls())
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(stringr)
  library(ggpubr)
  library(cowplot)
  library(RSQLite)
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
xlabS <- expression(paste("True MOI and Estimated MOI"[italic("var")]))
ylabS <- expression(paste("Proportion of ",italic("P. falciparum"), " isolates"))
ylabS <- bquote(atop("Proportion of ", paste(italic("P. falciparum"), " isolates")))
sizeV <- 18
sizeVFactor <- 1.175
saveDir0 <- "/project2/pascualmm/QZ/PhD/projects/FOI/writings/"
dir.create(saveDir0)
round <- "round1"
saveDir1 <- paste0(saveDir0, round, "/plots/MOI/")
dir.create(saveDir1)

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
            saveDir6 <- paste0(saveDir5, l, "/")
            dir.create(saveDir6)
            p3df <- NULL
            for (n in 1:length(MOI_states)) {
              MOI_state <- MOI_states[n]
              file <- paste0(readDir, run, "/", type, "/MOI/", s, "/",
                             o, "/", MOI_state, "/MOI_", num, "_rep", rep, ".RData")
              load(file)
              dfAllSub <- dfAll %>% filter(time %in% t) %>% mutate(IRSPhase = l) 
              if (s == "seasonal") {
                dfAllSub <- dfAllSub %>% mutate(timeLabel = ifelse(time%%T_YEAR == 300, "High/Wet Season", "Low/Dry Season"))
              } else {
                dfAllSub <- dfAllSub %>% mutate(timeLabel = "NA")
              }
              dfAllSub2 <- dfAllSub %>% filter(age <= ageCutoff)
              if (MOI_state == "withME") {
                temp1 <- dfAllSub2 %>% group_by(time, IRSPhase, timeLabel) %>% summarise(n_total = sum(p))
                temp2 <- dfAllSub2 %>% group_by(time, IRSPhase, timeLabel, MOI) %>% summarise(n = sum(p))
                p1df <- temp2 %>% left_join(temp1, by = c("time", "IRSPhase", "timeLabel")) %>% mutate(prop = n/n_total)
                
                if (o == "semi-open" & heterogeneity == "Heterogeneous Risk") {
                  temp1 <- dfAllSub2 %>% group_by(pop_id, time, IRSPhase, timeLabel) %>% summarise(n_total = sum(p))
                  temp2 <- dfAllSub2 %>% group_by(pop_id, time, IRSPhase, timeLabel, MOI) %>% summarise(n = sum(p))
                  p2df <- temp2 %>% left_join(temp1, by = c("pop_id", "time", "IRSPhase", "timeLabel")) %>% mutate(prop = n/n_total)
                } 
              } else {
                temp1 <- dfAllSub2 %>% group_by(time, IRSPhase, timeLabel) %>% summarise(n_total = n())
                temp2 <- dfAllSub2 %>% group_by(time, IRSPhase, timeLabel, MOI) %>% summarise(n = n())
                p1df <- temp2 %>% left_join(temp1, by = c("time", "IRSPhase", "timeLabel")) %>% mutate(prop = n/n_total)
                
                if (o == "semi-open" & heterogeneity == "Heterogeneous Risk") {
                  temp1 <- dfAllSub2 %>% group_by(pop_id, time, IRSPhase, timeLabel) %>% summarise(n_total = n())
                  temp2 <- dfAllSub2 %>% group_by(pop_id, time, IRSPhase, timeLabel, MOI) %>% summarise(n = n())
                  p2df <- temp2 %>% left_join(temp1, by = c("pop_id", "time", "IRSPhase", "timeLabel")) %>% mutate(prop = n/n_total)
                }
              }
              if (s == "seasonal") {
                p1df$timeLabel <- factor(p1df$timeLabel, levels = c("High/Wet Season", "Low/Dry Season"))
              } else {
                p1df$timeLabel <- factor(p1df$timeLabel, levels = c("NA"))
              }
              p1df$IRSPhase <- factor(p1df$IRSPhase, levels = unique(p1df$IRSPhase))
              p1df <- p1df %>% mutate(state = "Two Groups Combined")
              p1<-ggplot(p1df, aes(x=MOI, y = prop, fill = state))+
                geom_bar(stat = "identity", position = "dodge")+
                facet_grid(IRSPhase~timeLabel) +
                xlab(xlabS) + ylab(ylabS) +
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
                                            size=sizeV, angle=0)) + coord_cartesian(xlim = c(0,20)) + 
                scale_fill_manual(values = "blue", label = "Two Groups \nCombined", name = "") +
                theme(legend.position = c(0.5, 0.5))
              # print(p1)
              p1_legend <- get_legend(p1)
              ggsave(paste0(saveDir6, "MOI_", MOI_state, "_comb_risk_groups.pdf"), p1 + rremove("legend"), width = 7, height = 3.5)
              ggsave(paste0(saveDir6, "MOI_", MOI_state, "_comb_risk_groups-lg.pdf"), p1_legend, width = 3.5, height = 3.5)
              
              if (o == "semi-open" & heterogeneity == "Heterogeneous Risk") {
                if (s == "seasonal") {
                  p2df$timeLabel <- factor(p2df$timeLabel, levels = c("High/Wet Season", "Low/Dry Season"))
                } else {
                  p2df$timeLabel <- factor(p2df$timeLabel, levels = c("NA"))
                }
                p2df$IRSPhase <- factor(p2df$IRSPhase, levels = unique(p2df$IRSPhase))
                p2df <- p2df %>% mutate(risk = ifelse(pop_id == 0, "High Risk Group", "Low Risk Group"))
                p2df$risk <- factor(p2df$risk, levels = c("Low Risk Group", "High Risk Group"))
                p2<-ggplot(p2df, aes(x=MOI, y = prop, alpha = risk))+
                  geom_bar(stat = "identity", position = "dodge", fill = "dark blue")+
                  facet_grid(IRSPhase~timeLabel) +
                  xlab(xlabS) + ylab(ylabS) +
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
                                              size=sizeV, angle=0)) + coord_cartesian(xlim = c(0,20)) + 
                  scale_alpha_manual(values = c(0.4, 1), labels = c("Low Risk \nGroup", "High Risk \nGroup"), name = "") +
                  theme(legend.position = c(0.5, 0.5))
                # print(p2)
                p2_legend <- get_legend(p2)
                ggsave(paste0(saveDir6, "MOI_", MOI_state, "_ind_risk_groups.pdf"), p2 + rremove("legend"), width = 7, height = 3.5)
                ggsave(paste0(saveDir6, "MOI_", MOI_state, "_ind_risk_groups-lg.pdf"), p2_legend, width = 3.5, height = 3.5)
              }
              p3df <- rbind(p3df, p1df %>% mutate(MOIState = MOI_state)) 
            }
            
            p3df <- p3df %>% mutate(MOIState = ifelse(MOIState == "true", "True", "With Measurement Error"))
            p3df$MOIState <- factor(p3df$MOIState, levels = c("With Measurement Error", "True"))
            p3<-ggplot(p3df, aes(x=MOI, y = prop, fill = MOIState))+
              geom_bar(stat = "identity", position = "dodge")+
              facet_grid(IRSPhase~timeLabel) +
              xlab(xlabS) + ylab(ylabS) +
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
                                          size=sizeV, angle=0)) + coord_cartesian(xlim = c(0,20)) + 
              scale_fill_manual(values = c("blue", "orange"), name = "", label = c("Measurement Error", "True")) +
              theme(legend.position = c(0.5, 0.5))
            # print(p3)
            p3_legend <- get_legend(p3)
            ggsave(paste0(saveDir6, "MOI_withME_and_true", ".pdf"), p3 + rremove("legend"), width = 7, height = 3.5)
            ggsave(paste0(saveDir6, "MOI_withME_and_true-lg", ".pdf"), p3_legend, width = 3.5, height = 3.5)
          }
        }
      }
    }
  }
}

