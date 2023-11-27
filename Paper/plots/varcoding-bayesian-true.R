rm(list = ls())
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(cowplot)
  library(stringr)
  library(RSQLite)
})

preIRS <- 150
T_YEAR <- 360
ageThreshold <- 5
nums <- c(7,8)
runs <- c(5,7)
runsLabels <- c("Exponential", "Gamma")

readDir <- "/project2/pascualmm/QZ/PhD/projects/FOI/files"
seasonality <- "seasonal"
openness <- "semi-open"

MOI_states <- c("true", "withME", "withME-varcoding")
MOIAll <- NULL
for (i in 1:length(runs)) {
  run <- runs[i]
  runsLabel <- runsLabels[i]
  for (j in 1:length(MOI_states)) {
    MOI_state <- MOI_states[j]
    for (k in 1:length(nums)) {
      num <- nums[k]
      file <- paste0(readDir, run, "/actualRuns/MOI/", seasonality, "/", openness, "/",
                     MOI_state, "/MOI_", num, "_rep0.RData")
      load(file)
      if (num == 7) {
        t <- (preIRS - 1)*T_YEAR + 300
        IRSPhase <- "Pre-IRS"
      } else {
        t <- (preIRS + 1)*T_YEAR + 300
        IRSPhase <- "Mid-coverage IRS"
      }
      temp0 <- dfAll %>% filter(time == t, age <= ageThreshold) 
      if (MOI_state %in% c("true", "withME-varcoding")) {
        MOI <- temp0 
      } else {
        temp1 <- temp0 %>% group_by(time, MOI) %>% summarise(n = sum(p))
        MOI <- temp1 %>% group_by(time) %>% summarise(MOI = rep(MOI, round(n))) 
      }
      print(nrow(MOI))
      MOIAll <- rbind(MOIAll, MOI %>% mutate(
        MOI_state = MOI_state, num = num, runLabel = runsLabel, IRSPhase = IRSPhase 
      ))
    }
  }
}

saveDir <- "/project2/pascualmm/QZ/PhD/projects/FOI/writings/round1/plots/MOImethodsPerformance/"
dir.create(saveDir)
sizeV <- 18
sizeVFactor <- 1.15
xlabS <- expression(paste("True MOI and Estimated MOI"[italic("var")]))
ylabS <- bquote(atop("Proportion of " , paste(italic("P. falciparum"), " isolates")))
ylabS <- expression(paste("Proportion of " , italic("P. falciparum"), " isolates"))
MOIAll2 <- MOIAll %>% mutate(MOI_state = case_when(
  (MOI_state == "true") ~ "True", 
  (MOI_state == "withME") ~ "Bayesian Varcoding",
  (MOI_state == "withME-varcoding") ~ "Varcoding"
))
MOIAll2$MOI_state <- factor(MOIAll2$MOI_state, levels = c("Varcoding", "Bayesian Varcoding", "True"))
MOIAll2 <- MOIAll2 %>% mutate(runLabel = ifelse(runLabel == "Exponential", 
                                                "Exponentially Distributed \nTransmission Times",
                                                "Gamma Distributed \nHigh Variance \nTransmission Times"))
MOIAll2$IRSPhase <- factor(MOIAll2$IRSPhase, levels = c("Pre-IRS", "Mid-coverage IRS"))
p1 <- ggplot(MOIAll2, aes(x=MOI, col = MOI_state, fill = MOI_state))+ 
  geom_bar(aes (y=..prop..), stat = "count", position = "dodge")+
  facet_grid(IRSPhase~runLabel) + 
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
                              size=sizeV/sizeVFactor^2, angle=0)) + 
  theme(legend.position = c(0.3, 0.875)) +
  scale_color_manual(values = c("dark green", "cyan", "purple"),
                     labels = c("Varcoding", "Bayesian Varcoding", "True"),
                     name = "") + 
  scale_fill_manual(values = c("dark green", "cyan", "purple"),
                    labels = c("Varcoding", "Bayesian Varcoding", "True"),
                    name = "")
print(p1)
ggsave(paste0(saveDir, "performance.pdf"), p1, width = 6.25, height = 6.25)

