rm(list = ls())
suppressPackageStartupMessages({
  library(RSQLite)
  library(reshape2)
  library(vegan)
  library(tidyverse)
  library(cowplot)
  library(gridExtra)
  library(optparse)
})
args=(commandArgs(TRUE))
if(length(args)==0){
  print("No arguments supplied.")
}else{
  for(i in 1:length(args)){
    print(args[[i]])
    eval(parse(text=args[[i]]))
  }
}

c <- 20
r <- 20

source("/home/qizhan/others/PhD/projects/FOI/utils/solver.R")
preIRS <- 200
T_YEAR <- 360
IRSDur <- 2
postIRS <- 10-IRSDur
timesList <- list(c((preIRS - 1)*T_YEAR+180, (preIRS - 1)*T_YEAR+300),
                  c((preIRS + IRSDur - 1)*T_YEAR+180, (preIRS + IRSDur - 1)*T_YEAR+300))
labels <- c("Pre-IRS", "IRS") 

seasonality <- "seasonal"
openness <- "closed"
run <- 5
FOIstate <- "true"

nums <- 1:3
prefix <- "sim"

method <- "twoMoments"
saveDir1 <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/files", run, "/actualRuns/FOIBootstrap", "/", method, "/")
dir.create(saveDir1)
saveDir2 <- paste0(saveDir1, seasonality, "/")
dir.create(saveDir2)
saveDir3 <- paste0(saveDir2, openness, "/")
dir.create(saveDir3)
saveDir4 <- paste0(saveDir3, FOIstate, "/")
dir.create(saveDir4)

ageCutoff <- 5

VarInterArrivals_hightrans = seq(0,40000,100)
meanInterarrivals_hightrans = seq(1,90,1)
VarInterArrivals_midtrans = seq(0,90000,100)
meanInterarrivals_midtrans = seq(1,200,1)
VarInterArrivals_lowtrans = seq(0,1000000,1000)
meanInterarrivals_lowtrans = seq(200,900,2)

if (seasonality == "seasonal") {
  forward <- 60
  backward <- 180
} else if (seasonality == "non-seasonal") {
  forward <- 0
  backward <- 180
}
topn <- 5000 # top 5000 best likelihoods
bs_reps <- args[[1]]
nums_w_reps <- NULL
readDirFOI <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/files", run, 
                     "/actualRuns/FOI/", method, "/", seasonality, "/", openness, "/", FOIstate, "/")

for (m in 1:length(nums)) {
  no <- nums[m]
  print(paste0("Inference for sim_", no))
  saveDir5 <- paste0(saveDir4, prefix, "_", no, "/")
  dir.create(saveDir5)
  
  if (no %in% nums_w_reps) {
    reps <- 0:2
  } else {
    reps <- 0
  }
  
  for (rep in reps) {
    saveDir6 <- paste0(saveDir5, "rep_", rep, "/")
    dir.create(saveDir6)
    
    if (no %in% c(1,4,7)) {
      ds <- 1:length(timesList)
    } else {
      ds <- 2
    }
    
    for (d in ds) {
      label <- labels[d]
      saveDir7 <- paste0(saveDir6, label, "/")
      dir.create(saveDir7)
      times <- timesList[[d]]
      
      file <- paste0(readDirFOI, prefix, "_", no, "rep_", rep, "_", label, "_", FOIstate, ".RData")
      load(file)
      
      MOI_temp_original <- MOI_temp
      
      VarServiceT = var(dursEmpi)
      meanServiceT = mean(dursEmpi) 
      
      if (no %in% c(2,5,8) & label %in% labels[2]) {
        VarInterArrivals = VarInterArrivals_midtrans
        meanInterarrivals = meanInterarrivals_midtrans
      } else if (no %in% c(3,6,9) & label %in% labels[2]) {
        VarInterArrivals = VarInterArrivals_lowtrans
        meanInterarrivals = meanInterarrivals_lowtrans
      } else if (label %in% labels[1] | no %in% c(1,4,7)) {
        VarInterArrivals = VarInterArrivals_hightrans
        meanInterarrivals = meanInterarrivals_hightrans
      }
      
      FOI_est_bootstrap <- NULL
      for (v in bs_reps) {
        set.seed(v)
        MOI_temp_original_sub <- sample(MOI_temp_original, size = length(MOI_temp_original), replace = T)
        
        FOI_est <- NULL
        for (i in 1:length(meanInterarrivals)) {
          meanInterarrival <- meanInterarrivals[i]
          for (j in 1:length(VarInterArrivals)) {
            VarInterArrival <- VarInterArrivals[j]
            P_i_all <- estQLDist(Va = VarInterArrival, ma = meanInterarrival,
                                 Vs = VarServiceT, ms = meanServiceT, c = c, r = r)
            l <- lh_nlogT2(p = P_i_all, MOI = MOI_temp_original_sub)
            
            FOI_est_temp <- data.frame("likelihood" = l$lh, "minp" = l$minp, "meanInterarrival" = meanInterarrival,
                             "VarInterArrival" = VarInterArrival)
            FOI_est <- rbind(FOI_est, FOI_est_temp)
          } 
        }
        FOI_est_sub <- FOI_est %>%
          arrange(likelihood) %>%
          slice(seq_len(topn))
        FOI_est_bootstrap <- rbind(FOI_est_bootstrap, FOI_est_sub %>% mutate("rep" = v))
      }
      
      save(FOI_est_bootstrap, file = paste0(saveDir7, "bootstrap_rep_", min(bs_reps), "-", max(bs_reps), ".RData"))
    }
  }
}


# dfAll <- as_tibble(dfAll)
# dfAllSub <- dfAll %>% group_by(p_missing) %>% slice(which.min(likelihood))
# dfAllSub
# 
# dfAllSub2 <- dfAll %>%
#   group_by(p_missing) %>%
#   arrange(likelihood) %>%
#   slice(seq_len(5))
