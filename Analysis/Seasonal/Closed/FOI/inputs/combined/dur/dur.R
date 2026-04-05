rm(list = ls())
suppressPackageStartupMessages({
  library(dplyr)
  library(RSQLite)
})
fetchdb<-function(dbname,query,numQuery = 20000000) {
  r<-dbSendQuery(conn=dbname, query)
  er<-dbFetch(r,numQuery)
  while(!dbHasCompleted(r)){
    er <- rbind(er, dbFetch(r, numQuery))
    print(nrow(er))
  }
  dbClearResult(r)
  return(er)
}
readRoundIndex <- 1
arrivalType <- "exponential"
folder <- "eLifeSubAugust2024"
wd <- paste0("/scratch/midway2/qizhan/PhD/projects/FOI/", folder, "/round", readRoundIndex, "/simulation/", arrivalType, "/")
seasonality <- "seasonal"
openness <- "closed"
state <- "combined"
saveRoundIndex <- 3
saveDir0 <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/", folder, "/round", saveRoundIndex, "/files/", arrivalType, "/FOI/inputs/dur/")
if (!dir.exists(saveDir0)) {
  dir.create(saveDir0)
}
saveDir1 <- paste0(saveDir0, seasonality, "/")
if (!dir.exists(saveDir1)) {
  dir.create(saveDir1)
}
saveDir2 <- paste0(saveDir1, openness, "/")
if (!dir.exists(saveDir2)) {
  dir.create(saveDir2)
}
saveDir3 <- paste0(saveDir2, state, "/")
if (!dir.exists(saveDir3)) {
  dir.create(saveDir3)
}

prefix <- "sim"
nums <- 1:3
preIRS <- 200
T_YEAR <- 360
ageCutoff <- 5
ageGroupLabel <- "0-5yrs"
nums_w_reps <- NULL
LIVER_STAGE <- 14
if (seasonality == "seasonal") {
  forward <- 0
  backward <- 120
} else if (seasonality == "non-seasonal") {
  forward <- 0
  backward <- 180
}
for (i in 1:length(nums)) {
  num <- nums[i]
  saveDir4 <- paste0(saveDir3, prefix, "_", num, "/")
  if (!dir.exists(saveDir4)) {
    dir.create(saveDir4)
  }
  if (num %in% nums_w_reps) {
    reps <- 0:2
  } else {
    reps <- 0
  }
  if (num == min(nums)) {
    layersList <- list(c((preIRS - 2)*T_YEAR + 300, (preIRS - 1)*T_YEAR + 180), 
                       c((preIRS + 1)*T_YEAR + 300, (preIRS + 2)*T_YEAR + 180))
    layersLabels <- c("Pre-IRS", "IRS")
  } else {
    layersList <- list(c((preIRS + 1)*T_YEAR + 300, (preIRS + 2)*T_YEAR + 180))
    layersLabels <- c("IRS")
  }
  for (r in reps) {
    sampleSqlFile <- paste(wd, seasonality, "/", openness, "/",prefix, "_", num, "/sqlitesDir/", prefix, "_", num, "_r", r, "_sd.sqlite",sep="")
    print(sampleSqlFile)
    db <- dbConnect(SQLite(),dbname = sampleSqlFile)
    
    sc <- "select * from hosts"
    hosts <- fetchdb(db, sc)
    colnames(hosts)[1:2] <- c("host_id", "pop_id")
    
    for (j in 1:length(layersList)) {
      layers <- layersList[[j]]
      layersLabel <- layersLabels[j]
     
      sc <- "select * from sampled_duration"
      sampled_dur <- fetchdb(db, sc)
      print(dim(sampled_dur))
      sampled_dur <- left_join(sampled_dur, hosts, by = c("host_id", "pop_id")) 
      sampled_dur <- sampled_dur %>% mutate(age = (time - birth_time)/T_YEAR)
      sampled_dur <- sampled_dur %>% filter(age <= ageCutoff, time > min(layers) - backward, time <= max(layers) + forward) 
      durations <- sampled_dur$duration - LIVER_STAGE
      meanDuration <- mean(durations)
      varDuration <- var(durations)
      save(meanDuration, varDuration, file = paste0(saveDir4, "r", r, "_", layersLabel, "_", ageGroupLabel, ".RData"))
    }
    dbDisconnect(db)
  }
}