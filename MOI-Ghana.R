#### MOI estimation empirical 
rm(list = ls())
suppressPackageStartupMessages({
  library(stringr)
  library(ggplot2)
  library(cowplot)
  library(ggpubr)
  library(dplyr)
})

readDir <- "~/others/PhD/projects/FOI/utils/"
epi <- read.csv(paste0(readDir, "Ghana_Survey_Merged_Epi_MOI_S1_S7_070721_UChicago_080822.csv"), header = T, row.names = 1)
epi$SeqID <- str_replace(epi$SeqID, "-", ".")
run <- ""
round <- 1
MOIreadDir0 <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/files", run, "/Ghana/FOI/twoMoments/season/")
surveysID <- c(1:2,5:7)
prefix <- "survey"
surveys <- paste0(prefix, "_", surveysID)

MOIepi_data_analysis <- NULL
p_missing <- c(0, 0.05, 0.10)
for (i in 1:length(surveys)) {
  survey <- surveys[i]
  if (survey == "survey_7") {
    ageL <- "Children (6-10 years)"
  } else {
    ageL <- "Children (1-5 years)"
  }
  for (j in 1:length(p_missing)) {
    p_missing_single <- p_missing[j]
    MOIreadDir1 <- paste0(MOIreadDir0, "p_missing_", p_missing_single, "/")
    load(paste0(MOIreadDir1, survey, ".RData"))
    # MOI_temp_sub <- MOI_temp[MOI_temp>0]
    MOI_temp_sub <- MOI_temp
    MOI_df <- data.frame("MOI" = MOI_temp_sub, "survey" = survey, "age" = ageL, "p_missing" = p_missing_single)
    MOIepi_data_analysis <- rbind(MOIepi_data_analysis, MOI_df)
  }
}

MOIepi_data_analysis_sub <- MOIepi_data_analysis %>% mutate(IRSPhase = case_when(
  (survey == "survey_1")~"Pre-IRS",
  (survey == "survey_2")~"Pre-IRS",
  (survey == "survey_5")~"Right Post-IRS",
  (survey == "survey_6")~"Right Post-IRS",
  (survey == "survey_7")~"Post-IRS/SMC)"
))
MOIepi_data_analysis_sub$IRSPhase <- factor(MOIepi_data_analysis_sub$IRSPhase,
                                         levels = c("Pre-IRS",
                                                    "Right Post-IRS",
                                                    "Post-IRS/SMC)"))
MOIepi_data_analysis_sub <- MOIepi_data_analysis_sub %>% mutate(seasonL = case_when(
  (survey == "survey_1")~"High/Wet Season",
  (survey == "survey_2")~"Low/Dry Season",
  (survey == "survey_5")~"High/Wet Season",
  (survey == "survey_6")~"Low/Dry Season",
  (survey == "survey_7")~"High/Wet Season"
))
MOIepi_data_analysis_sub$seasonL <- factor(MOIepi_data_analysis_sub$seasonL, levels = c("Low/Dry Season", "High/Wet Season"))
MOIepi_data_analysis_sub <- MOIepi_data_analysis_sub %>% mutate(p_missingL = case_when(
  (p_missing == 0)~"High Detectability",
  (p_missing == 0.05)~"Mid Detectability",
  (p_missing == 0.10)~"Low Detectability"
))
MOIepi_data_analysis_sub$p_missingL <- factor(MOIepi_data_analysis_sub$p_missingL, levels = c("High Detectability", "Mid Detectability", "Low Detectability"))
MOI_Survey_mean = MOIepi_data_analysis_sub %>% 
  group_by(survey, seasonL, p_missingL) %>% 
  mutate(mean_MOI = mean(MOI))

saveDir <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/writings/round", round, "/plots/MOI/Ghana/")
dir.create(saveDir)

p1_df <- MOIepi_data_analysis_sub  
IRSPhaseLabels <- c("Pre-IRS" = "Pre-IRS",
                    "Right Post-IRS" = "Right Post-IRS",
                    "Post-IRS/SMC)" = "Post-IRS/SMC")
seasonLabels <- c("High/Wet Season" = "High/Wet Season",
                  "Low/Dry Season" = "Low/Dry Season")
sizeV <- 20
sizeVFactor <- 1.15
p1 <- ggplot(p1_df, aes(x = (MOI), fill = seasonL)) +
  geom_bar(aes (y=..prop..), stat = "count", position = "dodge")+
  xlab(expression(paste("Estimated MOI"[italic("var")]))) + 
  ylab(expression(paste("Proportion of ",italic("P. falciparum"), " isolates"))) +
  facet_wrap(p_missingL~IRSPhase, labeller = labeller(Survey=IRSPhaseLabels), ncol = 3)+
  scale_fill_manual(values=c("green", "magenta"),
                    name = "",
                    labels = seasonLabels)+
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
                              size=sizeV/sizeVFactor, angle=0)) + coord_cartesian(xlim = c(0,20)) + 
  theme(legend.position = c(0.178, 0.9275))
print(p1)
ggsave(paste0(saveDir, "MOI.pdf"), p1, width = 8, height = 8)

