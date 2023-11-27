rm(list = ls())
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(ggpubr)
  library(cowplot)
  library(ggsci)
})
run <- ""
readDir <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/files", run, "/Ghana/FOIBootstrap/")
prefix <- "survey"
T_YEAR <- 365
surveyIndices <- list(c(1), c(2), c(3), c(4), c(5), c(6), c(7))
surveyLabels <- list("Pre-IRS Wet \n(2012, S1)",
                     "Pre-IRS Dry \n(2013, S2)",
                     "IRS Dry \n(2014, S3)", # did not include in the analysis
                     "IRS Wet \n(2014, S4)", # did not include in the analysis
                     "Right Post-IRS Wet \n(2015, S3)", 
                     "Right Post-IRS Dry \n(2016, S4)",
                     "Post-IRS/SMC Wet \n(2017, S5)")
IRSPhases <- c("Pre-IRS", "Pre-IRS", "IRS", "IRS", "Right Post-IRS", "Right Post-IRS",
               "Post-IRS/SMC")
# IRSPhasesLabels <- c("Pre-IRS \n(2012, S1)", "Pre-IRS \n(2013, S2)", "IRS \n(2014, S3)", "IRS \n(2014, S4)",
#                "Right Post-IRS \n(2015, S3)", "Right Post-IRS \n(2016, S4)",
#                "Post-IRS/SMC \n(2017, S5)")
seasons <- c("High/Wet Season", "Low/Dry Season", "Low/Dry Season", "High/Wet Season", "High/Wet Season", "Low/Dry Season", "High/Wet Season")
methods <- c("twoMoments", "littleslaw")
types <- c("season")
p_missing <- c(0, 0.05, 0.10)
bs_reps <- 1:200
FOICombined <- NULL
for (i in c(1,2,5,6,7)) {
  surveyIndex <- surveyIndices[[i]]
  surveyLabel <- surveyLabels[[i]]
  seasonLabel <- seasons[i]
  IRSPhase <- IRSPhases[i]
  for (j in 1:length(surveyIndex)) {
    sIndex <- surveyIndex[j]
    for (k in 1:length(methods)) {
      method <- methods[k]
      for (m in 1:length(types)) {
        type <- types[m]
        for (n in 1:length(p_missing)) {
          p_missing_single <- p_missing[n]
          readDirFinal <- paste0(readDir, method, "/", type, "/p_missing_", p_missing_single, "/", prefix, "_", sIndex, "/")
          
          if (method == "twoMoments") {
            fileNames <- paste0("bootstrap_rep_", bs_reps, "-", bs_reps, ".RData")
            FOICombinedSub <- NULL
            for (p in 1:length(fileNames)) {
              fileName <- fileNames[p]
              load(paste0(readDirFinal, fileName))
              temp <- FOI_est_bootstrap %>% arrange(likelihood) %>% slice(seq_len(1)) %>% 
                mutate(FOI_est = T_YEAR/meanInterarrival, p_missing = p_missing_single) %>% select(FOI_est, p_missing)
              FOICombinedSub <- rbind(FOICombinedSub, temp)
            }
          } else {
            fileName <- paste0("bootstrap_rep_", min(bs_reps), "-", max(bs_reps), ".RData")
            load(paste0(readDirFinal, fileName))
            FOICombinedSub <- data.frame("FOI_est" = FOI_est_bootstrap$FOI, "p_missing" = p_missing_single)
          }
          FOICombinedSub <- as_tibble(FOICombinedSub)
          FOICombined <- rbind(FOICombined, FOICombinedSub %>% mutate("survey_label" = surveyLabel, 
                                                            "method" = method, "type" = type,
                                                            "IRSPhase" = IRSPhase, "seasonLabel" = seasonLabel))
        }
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
                            c("Pre-IRS Wet \n(2012, S1)",
                              "Pre-IRS Dry \n(2013, S2)",
                              "Right Post-IRS Wet \n(2015, S3)", 
                              "Right Post-IRS Dry \n(2016, S4)",
                              "Post-IRS/SMC Wet \n(2017, S5)"))
FOICombined2$IRSPhase <- factor(FOICombined2$IRSPhase, levels = c("Pre-IRS",  
                                                                  "Right Post-IRS", 
                                                                  "Post-IRS/SMC"))
FOICombined2$seasonLabel <- factor(FOICombined2$seasonLabel, levels = c("High/Wet Season", "Low/Dry Season"))
FOICombined2 <- FOICombined2 %>% mutate(method = ifelse(method == "twoMoments", "Two-Moment Approximation", "Little's Law"))
FOICombined2$method <- factor(FOICombined2$method, levels = c("Two-Moment Approximation", "Little's Law"))
saveDir0 <- "/project2/pascualmm/QZ/PhD/projects/FOI/writings/round1/plots/FOIBootstrap/Ghana/" 
dir.create(saveDir0)
sizeV <- 18
sizeVFactor <- 1.175
statsSummaryf <- function(x) {
  r <- quantile(x, probs = c(0.00, 0.05, 0.5, 0.95, 1.00))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}
p1<-ggplot(FOICombined2, aes(x=p_missingL, y = FOI_est, fill = p_missingL, alpha = method))+
  stat_summary(fun.data = statsSummaryf, geom="boxplot", 
               position=position_dodge(1)) +
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
  scale_fill_tron() + coord_cartesian(ylim = c(0,8.5)) + 
  scale_alpha_manual(name = "",
                     values = c(1, 0.35),
                     labels = c("Two-Moment \nApproximation", "Little's Law")) +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(1, 0.35)),
                                              colour=NA), nrow = 2)) + 
  theme(legend.position = c(0.8, 0.3))
print(p1)
ggsave(paste0(saveDir0, "FOI_Ghana_single_surveys", ".pdf"), p1, width = 6, height = 5)

