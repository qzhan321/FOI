rm(list = ls())
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(ggpubr)
})
statsSummaryf <- function(x) {
  r <- quantile(x, probs = c(0.00, 0.05, 0.5, 0.95, 1.00))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}
scaleFUN <- function(x) sprintf("%.1f", x)
cols <- c("#0073C2FF", "#EFC000FF", "#868686FF")
folder <- "eLifeSubAugust2024"
roundIndex <- 3
arrivalType <- "exponential"
readDir0 <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/", folder, "/round", roundIndex, "/files/")
MOIState <- "missingData"
nums <- 1:3
methods <- c("TwoMomentApproximation", "LittlesLaw")
prefix <- "sim"
rep <- 0
ageLabel <- "0-5yrs"
sizeV <- 11
sizeVFactor <- 1.35
saveDir0 <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/", folder, "/round", roundIndex, "/plots/")
if (!dir.exists(saveDir0)) {
  dir.create(saveDir0)
}
saveDir1 <- paste0(saveDir0, "visualizeFOIBootstrapDist/")
if (!dir.exists(saveDir1)) {
  dir.create(saveDir1)
}
saveDir2 <- paste0(saveDir1, "simulation/")
if (!dir.exists(saveDir2)) {
  dir.create(saveDir2)
}
saveDir3 <- paste0(saveDir2, "/", arrivalType, "/")
if (!dir.exists(saveDir3)) {
  dir.create(saveDir3)
}
seasonality <- "seasonal"
openness <- "closed"
saveDir4 <- paste0(saveDir3, "/", seasonality, "/")
if (!dir.exists(saveDir4)) {
  dir.create(saveDir4)
}
saveDir5 <- paste0(saveDir4, "/", openness, "/")
if (!dir.exists(saveDir5)) {
  dir.create(saveDir5)
}  
saveDir6 <- paste0(saveDir5, "/", MOIState, "/")
if (!dir.exists(saveDir6)) {
  dir.create(saveDir6)
}  
for (i in 1:length(nums)) {
  num <- nums[i]
  if (num == min(nums)) {
    labels <- c("Pre-IRS", "IRS")
  } else {
    labels <- c("IRS")
  }
  for (j in 1:length(labels)) {
    label <- labels[j]
    for (k in 1:length(methods)) {
      method <- methods[k]
      if (method == "TwoMomentApproximation") {
        repsNames1 <- seq(0, 195, 5) + 1
        repsNames2 <- seq(5, 200, 5)
        repsNames <- paste0(repsNames1, "-", repsNames2)
      } else {
        repsNames <- "1-200"
      }
      filesList <- lapply(repsNames, function(x){paste0(readDir0, arrivalType, "/FOIBootstrap/estimation/", method, "/", seasonality, "/", openness, "/", MOIState, "/", prefix, "_", num, "/r", rep, "_", label, "_bs_", x, ".RData")})
      files <- unlist(filesList)
      FOIbsCombined <- NULL
      for (m in 1:length(files)) {
        file <- files[m]
        load(file)
        if (method == "TwoMomentApproximation") {
          FOI <- FOI %>% group_by(rep) %>%
            arrange(negLogLikelihood) %>%
            slice(seq_len(1)) %>% select(FOI, rep)
        }
        FOIbs <- FOI %>% mutate(Seasonality = seasonality, Openness = openness, 
                                ArrivalofInfection = ifelse(arrivalType == "exponential", "Exponential", "Gamma"),
                                IRSPhase = case_when((label == "Pre-IRS") ~ "Pre-IRS",
                                                     (num == 1 & label == "IRS")~"Low-coverage IRS",
                                                     (num == 2 & label == "IRS")~"Mid-coverage IRS",
                                                     (num == 3 & label == "IRS")~"High-coverage IRS"),
                                Method = case_when((method == "TwoMomentApproximation")~"Two-Moment Approximation",
                                                   (method == "LittlesLaw")~"Little's Law"), 
                                State = case_when((MOIState == "true")~"Estimated \nfrom \nTrue MOI",
                                                  (MOIState == "missingData")~"Estimated \nMissing \nData",
                                                  (MOIState == "measurementError")~"Estimated \nUndersampling \nof Var",
                                                  (MOIState == "drugUsage")~"Estimated \nAntimalarial \nTreatment",
                                                  (MOIState == "combined")~"Estimated \nAll Errors"))
        FOIbsCombined <- bind_rows(FOIbsCombined, FOIbs)
      }
      p1 <- ggplot(FOIbsCombined, aes(x = FOI)) +
        geom_histogram(bins = 50) +
        xlab("Bootstrapped FOI") + ylab(method) +
        theme_classic() + theme(
          plot.title = element_text(color="black", size=sizeV, hjust = 0.5),
          axis.title.x = element_text(color="black", size=sizeV),
          axis.title.y = element_text(color="black", size=sizeV),
          axis.text.x = element_text(color="black", size=sizeV/sizeVFactor, angle=0, face = "bold"),
          axis.text.y = element_text(color="black", size=sizeV, angle=0),
          legend.text = element_text(color="black", size=sizeV/sizeVFactor, angle=0),
          legend.title = element_text(color="black", size=sizeV/sizeVFactor, angle=0),
          strip.text = element_text(color="black", size=sizeV, angle=0)) +
        coord_cartesian(xlim = c(min(FOIbsCombined$FOI)*0.95, max(FOIbsCombined$FOI)*1.05))
      p1
      ggsave(paste0(saveDir6, prefix, "_", num, "_", label, "_", method, ".pdf"), width = 6, height = 6)
    }
  }
}
