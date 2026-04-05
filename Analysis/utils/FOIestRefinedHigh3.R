rm(list = ls())
suppressPackageStartupMessages({
  library(dplyr)
  library(optparse)
  library(RSQLite)
})

option_list <- list(
  make_option(c("-i", "--inputFile"), type = "character", default = NULL,
              help = "the path and the name of the input file containing MOI information"),
  make_option(c("-c", "--bloodStageCarryingCapacity"), type = "integer", default = 30,
              help = "the carrying capacity for blood-stage infections"),
  make_option(c("-b", "--bootstrap"), type = "logical", default = TRUE,
              help = "whether running bootstrap analysis or not"),
  make_option(c("-s", "--replicateStartIndexBootstrap"), type = "integer", default = NULL,
              help = "the start index of replicates for bootstrap analysis"),
  make_option(c("-e", "--replicateEndIndexBootstrap"), type = "integer", default = NULL,
              help = "the end index of replicates for bootstrap analysis"),
  make_option(c("-m", "--method"), type = "character", default = "TwoMomentApproximation",
              help = "which method for FOI estimation [default= %default]; either TwoMomentApproximation or LittlesLaw"),
  make_option(c("-p", "--paramRange"), type = "character", default = "medium",
              help = "transmission intensity of the location [default= %default]; four options, verylow or low or medium or high"),
  make_option(c("-d", "--durationInformation"), type = "character", default = NULL,
              help = "the path to the RData object which contains the mean and variance of infection duration; when not supplied, use the information from the historical dataset of neurosyphilis patients treated with malaria infection"),
  make_option(c("-o", "--output"), type = "character", default = "./FOI.RData",
              help = "the path and the name of the output file [default= %default]"),
  make_option(c("-t", "--T_YEAR"), type = "integer", default = 365,
              help = "the number of days in a year; this can vary for simulation output, for example, 360 days")
)
opt <- parse_args(OptionParser(option_list = option_list))
print(opt)

c <- opt$bloodStageCarryingCapacity
r <- 0

T_YEAR <- opt$T_YEAR

if (is.null(opt$durationInformation)) {
  meanServiceT <- 208.4681
  VarServiceT <- 10817.6
} else {
  load(opt$durationInformation)
  meanServiceT <- meanDuration
  VarServiceT <- varDuration
}

MOIInfo <- read.csv(opt$inputFile, header = T)

source("/home/qizhan/others/PhD/projects/FOI/eLifeSubAugust2024/round1/analysis/scripts/utils/funcs.R")

if ("Prob" %in% colnames(MOIInfo)) {
  MOIs <- rep(MOIInfo$MOI, round(MOIInfo$N*MOIInfo$Prob))
} else if ("Count" %in% colnames(MOIInfo)) {
  MOIs <- rep(MOIInfo$MOI, MOIInfo$Count)
}

runLittlesLaw <- function(bootstrap = FALSE, reps = NA, MOIs, meanDur, T_YEAR) {
  if (bootstrap) {
    FOI_est <- NULL
    for (rep in reps) {
      set.seed(rep)
      MOIsBS <- sample(MOIs, size = length(MOIs), replace = T)
      FOI_est_single <- mean(MOIsBS)/meanDur*T_YEAR
      FOI_est <- bind_rows(FOI_est, data.frame("FOI" = FOI_est_single, "rep" = rep))
    }
  } else {
    FOI_est_single <- mean(MOIs)/meanDur*T_YEAR
    FOI_est <- data.frame("FOI" = FOI_est_single)
  }
  return(FOI_est)
}

runTwoMomentsApproximation <- function(bootstrap = FALSE, reps = NA, paramRange, MOIs, meanDur, VarDur, c, r, topn = NULL) {
  if (paramRange == "high") {
    VarInterArrivals = seq(0,160000,12.5)
    meanInterarrivals = seq(1,150,1/4)
  } else if (paramRange == "medium") {
    VarInterArrivals = seq(0,360000,100)
    meanInterarrivals = seq(1,300,1)
  } else if (paramRange == "low") {
    VarInterArrivals = seq(0,2250000,500)
    meanInterarrivals = seq(1,700,1)
  } else if (paramRange == "verylow") {
    VarInterArrivals = seq(0,6250000,2000)
    meanInterarrivals = seq(1,2100,2)
  }
  if (bootstrap) {
    FOI_est_list <- lapply(reps, function(x){vectorizeTwoMomentsApproximation(x, MOIs, meanInterarrivals, VarInterArrivals, meanDur, VarDur, c, r, topn)})
    FOI_est <- do.call("rbind", FOI_est_list)
  } else {
    FOI_est <- vectorizeTwoMomentsApproximationNoBS(MOIs, meanInterarrivals, VarInterArrivals, meanDur, VarDur, c, r, topn)
  }
  return(FOI_est)
}

vectorizeTwoMomentsApproximation <- function(rep, MOIs, meanInterarrivals, VarInterArrivals, meanDur, VarDur, c, r, topn) {
  FOI_est_rep_list <- lapply(meanInterarrivals, function(x){vectorizeTwoMomentsApproximationLevel1(rep, MOIs, x, VarInterArrivals, meanDur, VarDur, c, r)})
  FOI_est_rep <- do.call("rbind", FOI_est_rep_list)
  if (is.null(topn)) {
    FOI_est_rep <- FOI_est_rep %>% group_by(rep) %>% arrange(negLogLikelihood) 
  } else {
    FOI_est_rep <- FOI_est_rep %>% group_by(rep) %>% arrange(negLogLikelihood) %>% slice(seq_len(topn)) 
  }
  return(FOI_est_rep)
}

vectorizeTwoMomentsApproximationLevel1 <- function(rep, MOIs, meanInterarrival, VarInterArrivals, meanDur, VarDur, c, r) {
  FOI_est_rep_single_list <- lapply(VarInterArrivals, function(x){vectorizeTwoMomentsApproximationLevel2(rep, MOIs, meanInterarrival, x, meanDur, VarDur, c, r)})
  FOI_est_rep_single <- do.call("rbind", FOI_est_rep_single_list)
  return(FOI_est_rep_single)
}

vectorizeTwoMomentsApproximationLevel2 <- function(rep, MOIs, meanInterarrival, VarInterArrival, meanDur, VarDur, c, r) {
  set.seed(rep)
  MOIsBS <- sample(MOIs, size = length(MOIs), replace = T)
  P_i_all <- estQLDist(Va = VarInterArrival, ma = meanInterarrival,
                       Vs = VarDur, ms = meanDur, c = c, r = r)
  l <- calcNegLogLikelihood(p = P_i_all, MOI = MOIsBS)
  FOI_est_rep_single_temp <- data.frame("negLogLikelihood" = l$negLogLikelihood, "minp" = l$minp, "meanInterarrival" = meanInterarrival,
                                        "VarInterArrival" = VarInterArrival, "rep" = rep) 
  return(FOI_est_rep_single_temp)
}  


vectorizeTwoMomentsApproximationNoBS <- function(MOIs, meanInterarrivals, VarInterArrivals, meanDur, VarDur, c, r, topn) {
  FOI_est_list <- lapply(meanInterarrivals, function(x){vectorizeTwoMomentsApproximationNoBSLevel1(MOIs, x, VarInterArrivals, meanDur, VarDur, c, r)})
  FOI_est <- do.call("rbind", FOI_est_list)
  if (is.null(topn)) {
    FOI_est <- FOI_est %>% arrange(negLogLikelihood) 
  } else {
    FOI_est <- FOI_est %>% arrange(negLogLikelihood) %>% slice(seq_len(topn))
  }
  return(FOI_est)
}

vectorizeTwoMomentsApproximationNoBSLevel1 <- function(MOIs, meanInterarrival, VarInterArrivals, meanDur, VarDur, c, r) {
  FOI_single_list <- lapply(VarInterArrivals, function(x){vectorizeTwoMomentsApproximationNoBSLevel2(MOIs, meanInterarrival, x, meanDur, VarDur, c, r)})
  FOI_single <- do.call("rbind", FOI_single_list)
  return(FOI_single)
}

vectorizeTwoMomentsApproximationNoBSLevel2 <- function(MOIs, meanInterarrival, VarInterArrival, meanDur, VarDur, c, r) {
  P_i_all <- estQLDist(Va = VarInterArrival, ma = meanInterarrival,
                       Vs = VarDur, ms = meanDur, c = c, r = r)
  l <- calcNegLogLikelihood(p = P_i_all, MOI = MOIs)
  FOI_est_single_temp <- data.frame("negLogLikelihood" = l$negLogLikelihood, "minp" = l$minp, "meanInterarrival" = meanInterarrival,
                                    "VarInterArrival" = VarInterArrival) 
  return(FOI_est_single_temp)
}  

reps <- opt$replicateStartIndexBootstrap:opt$replicateEndIndexBootstrap
if (opt$method == "LittlesLaw") {
  FOI <- runLittlesLaw(bootstrap = opt$bootstrap, reps = reps, MOIs = MOIs, meanDur = meanServiceT, T_YEAR = T_YEAR)
} else if (opt$method == "TwoMomentApproximation") {
  FOI <- runTwoMomentsApproximation(bootstrap = opt$bootstrap, reps = reps, MOIs = MOIs, paramRange = opt$paramRange, meanDur = meanServiceT, VarDur = VarServiceT, c = c, r = r, topn = NULL)
  FOI <- FOI %>% mutate(FOI = T_YEAR/meanInterarrival) %>% select(-minp)
}

save(FOI, file = opt$output)
