rm(list = ls())
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(ggpubr)
  library(cowplot)
  library(ggsci)
})
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
methods <- c("twoMoments", "littleslaw")
types <- c("annual")
p_missing <- c(0, 0.05, 0.10)
FOICombined <- NULL
for (i in 1:length(surveyIndices)) {
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
          temp <- FOI_est %>% arrange(likelihood) %>% slice(seq_len(1)) %>% 
            mutate(FOI_est = T_YEAR/meanInterarrival) %>% select(FOI_est, p_missing)
        } else {
          temp <- data.frame("FOI_est" = FOI_est_single, "p_missing" = p_missing_single)
        }
        temp <- as_tibble(temp)
        FOICombined <- rbind(FOICombined, temp %>% mutate("survey_label" = surveyLabel, 
                                                          "method" = method, "type" = type,
                                                          "IRSPhase" = IRSPhase, "seasonLabel" = seasonLabel))
      }
    }
  }
}

FOICombined2 <- FOICombined %>% mutate(p_missingL = case_when(
  (p_missing == 0)~"High",
  (p_missing == 0.05)~"Mid",
  (p_missing == 0.10)~"Low"
))
FOICombined2$p_missingL <- factor(FOICombined2$p_missingL, levels = 
                                    c("High", "Mid", "Low"))
FOICombined2$survey_label <- factor(FOICombined2$survey_label, levels = 
                                      c("Pre-IRS \n(2012 and 2013, S1 and S2)",
                                        "Right Post-IRS \n(2015 and 2016, S3 and S4)"))
FOICombined2$IRSPhase <- factor(FOICombined2$IRSPhase, levels = c("Pre-IRS", "Right Post-IRS"))
FOICombined2$seasonLabel <- factor(FOICombined2$seasonLabel, levels = c("Annual"))
FOICombined2 <- FOICombined2 %>% mutate(method = ifelse(method == "twoMoments", "Two-Moment Approximation", "Little's Law"))
FOICombined2$method <- factor(FOICombined2$method, levels = c("Two-Moment Approximation", "Little's Law"))
saveDir0 <- "/project2/pascualmm/QZ/PhD/projects/FOI/writings/round1/plots/FOI/Ghana/" 
dir.create(saveDir0)
sizeV <- 18
sizeVFactor <- 1.175
p1<-ggplot(FOICombined2, aes(x=p_missingL, y = FOI_est, fill = p_missingL, alpha = method))+
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(seasonLabel~IRSPhase) +
  xlab("PCR Detectability") + ylab("Estimated Mean FOI \nper Host per Year") +
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
  guides(fill = "none") +
  scale_fill_tron() + coord_cartesian(ylim = c(0,7)) + 
  scale_alpha_manual(name = "",
                     values = c(1, 0.35),
                     labels = c("Two-Moment Approximation", "Little's Law")) +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(1, 0.35)),
                                              colour=NA), nrow = 1)) + 
  theme(legend.position = c(0.5, 0.89))
print(p1)
ggsave(paste0(saveDir0, "FOI_Ghana_paired_surveys", ".pdf"), p1, width = 6, height = 4.5)

