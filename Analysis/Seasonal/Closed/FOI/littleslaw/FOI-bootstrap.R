rm(list = ls())
suppressPackageStartupMessages({
  library(dplyr)
})
c <- 20
r <- 20
run <- 5
readDir <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/files", run, "/actualRuns/FOI/", "twoMoments", "/")
seasonality <- "seasonal"
openness <- "closed"
nums <- 1:3
T_YEAR <- 360
prefix <- "sim"

MOIstates <- c("true", "withME")
saveDir1 <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/files", run, "/actualRuns/FOIBootstrap/littleslaw/")
dir.create(saveDir1)
saveDir2 <- paste0(saveDir1, seasonality, "/")
dir.create(saveDir2)
saveDir3 <- paste0(saveDir2, openness, "/")
dir.create(saveDir3)

ageCutoff <- 5
N <- 10000
preIRS <- 200
IRSDur <- 2
timesList <- list(c((preIRS - 1)*T_YEAR+180, (preIRS - 1)*T_YEAR+300),
                  c((preIRS + IRSDur - 1)*T_YEAR+180, (preIRS + IRSDur - 1)*T_YEAR+300))
tLabels <- c("Pre-IRS", "IRS")
nums_w_reps <- NULL
bs_reps <- 1:200
for (i in 1:length(MOIstates)) {
  MOIstate <- MOIstates[i]
  saveDir4 <- paste0(saveDir3, MOIstate, "/")
  dir.create(saveDir4)
  for (j in 1:length(nums)) {
    num <- nums[j]
    saveDir5 <- paste0(saveDir4, prefix, "_", num, "/")
    dir.create(saveDir5)
    if (num %in% nums_w_reps) {
      reps <- 0:2
    } else {
      reps <- 0
    }
    for (rep in reps) {
      saveDir6 <- paste0(saveDir5, "rep_", rep, "/")
      dir.create(saveDir6)
      if (num %in% c(1,4,7)) {
        timesSub <- timesList
        tLabelsSub <- tLabels
      } else {
        timesSub <- timesList[2]
        tLabelsSub <- tLabels[2]
      }
      for (k in 1:length(timesSub)) {
        t <- timesSub[k]
        l <- tLabelsSub[k]
        saveDir7 <- paste0(saveDir6, l, "/")
        dir.create(saveDir7)
        file <- paste0(readDir, seasonality, "/", openness, "/", MOIstate, "/", prefix, "_", num, "rep_", rep, "_", l, "_", MOIstate, ".RData")
        if (file.exists(file)) {
        } else {
          file <- paste0(readDir, seasonality, "/", openness, "/", MOIstate, "/", prefix, "_", num, "_rep_", rep, "_", l, "_", MOIstate, ".RData")
        }
        load(file)
        meanDur <- mean(dursEmpi)
        FOI_est_bootstrap <- NULL
        for (v in bs_reps) {
          set.seed(v)
          MOI_temp_sub <- sample(MOI_temp, size = length(MOI_temp), replace = T)
          FOI_est_single <- mean(MOI_temp_sub)/meanDur*T_YEAR
          FOI_est_bootstrap <- rbind(FOI_est_bootstrap, data.frame("FOI" = FOI_est_single, "rep" = v))
        }
        if (openness == "semi-open") {
          save(FOI_est_bootstrap, FOI_actual1, FOI_actual2, file = paste0(saveDir7, "bootstrap_rep_", min(bs_reps), "-", max(bs_reps), ".RData"))
        } else {
          save(FOI_est_bootstrap, FOI_actual, file = paste0(saveDir7, "bootstrap_rep_", min(bs_reps), "-", max(bs_reps), ".RData"))
        }
      }
    }
  }
}
