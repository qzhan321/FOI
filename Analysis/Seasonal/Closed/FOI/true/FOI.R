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
state <- "true"
saveRoundIndex <- 3
saveDir0 <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/", folder, "/round", saveRoundIndex, "/files/", arrivalType, "/FOI/", state, "/")
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

prefix <- "sim"
nums <- 1:3
preIRS <- 200
T_YEAR <- 360
ageCutoff <- 5
ageGroupLabel <- "0-5yrs"
if (seasonality == "seasonal") {
  forward <- 0
  backward <- 120
} else if (seasonality == "non-seasonal") {
  forward <- 0
  backward <- 180
}
nums_w_reps <- NULL
for (i in 1:length(nums)) {
  num <- nums[i]
  if (openness == "semi-open") {
    N <- 15000
  } else {
    N <- 10000
  }
  if (num == min(nums)) {
    layersList <- list(c((preIRS - 2)*T_YEAR + 300, (preIRS - 1)*T_YEAR + 180), 
                       c((preIRS + 1)*T_YEAR + 300, (preIRS + 2)*T_YEAR + 180))
    layersLabels <- c("Pre-IRS", "IRS")
  } else {
    layersList <- list(c((preIRS + 1)*T_YEAR + 300, (preIRS + 2)*T_YEAR + 180))
    layersLabels <- c("IRS")
  }
  saveDir3 <- paste0(saveDir2, prefix, "_", num, "/")
  if (!dir.exists(saveDir3)) {
    dir.create(saveDir3)
  }
  if (num %in% nums_w_reps) {
    reps <- 0:2
  } else {
    reps <- 0
  }
  for (r in reps) {
    sampleSqlFile <- paste(wd, seasonality, "/", openness, "/",prefix, "_", num, "/sqlitesDir/", prefix, "_", num, "_r", r, "_sd.sqlite",sep="")
    print(sampleSqlFile)
    db <- dbConnect(SQLite(),dbname = sampleSqlFile)
    
    sc <- "select * from summary"
    summary <- fetchdb(db, sc)
    
    for (j in 1:length(layersList)) {
      layers <- layersList[[j]]
      layersLabel <- layersLabels[j]
      summarySub <- summary %>% filter(time > min(layers) - backward, time <= max(layers) + forward)
      trueFOI <- sum(summarySub$FOI)/N 
      save(trueFOI, file = paste0(saveDir3, "r", r, "_", layersLabel, "_", ageGroupLabel, ".RData"))
    }
    dbDisconnect(db)
  }
}