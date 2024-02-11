rm(list = ls())
c <- 20
r <- 0
run <- 5
readDir <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/files", run, "/actualRuns/FOI/", "twoMoments", "/")
seasonality <- "seasonal"
openness <- "closed"
nums <- 1:3
T_YEAR <- 360
prefix <- "sim"

MOIstates <- c("true", "withME")
saveDir1 <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/files", run, "/actualRuns/FOI/littleslaw/")
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
for (i in 1:length(MOIstates)) {
  MOIstate <- MOIstates[i]
  saveDir4 <- paste0(saveDir3, MOIstate, "/")
  dir.create(saveDir4)
  for (j in 1:length(nums)) {
    num <- nums[j]
    if (num %in% nums_w_reps) {
      reps <- 0:2
    } else {
      reps <- 0
    }
    for (rep in reps) {
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
        file <- paste0(readDir, seasonality, "/", openness, "/", MOIstate, "/", prefix, "_", num, "rep_", rep, "_", l, "_", MOIstate, ".RData")
        if (file.exists(file)) {
        } else {
          file <- paste0(readDir, seasonality, "/", openness, "/", MOIstate, "/", prefix, "_", num, "_rep_", rep, "_", l, "_", MOIstate, ".RData")
        }
        load(file)
        meanDur <- mean(dursEmpi)
        FOI_est_single <- mean(MOI_temp)/meanDur*T_YEAR
        if (openness == "semi-open") {
          save(FOI_est_single, FOI_actual1, FOI_actual2, dursEmpi, MOIs, MOI_temp, file = paste0(saveDir4, prefix, "_", num, "rep_", rep, "_", l, "_", MOIstate, ".RData"))
        } else {
          save(FOI_est_single, FOI_actual, dursEmpi, MOIs, MOI_temp, file = paste0(saveDir4, prefix, "_", num, "rep_", rep, "_", l, "_", MOIstate, ".RData"))
        }
      }
    }
  }
}
