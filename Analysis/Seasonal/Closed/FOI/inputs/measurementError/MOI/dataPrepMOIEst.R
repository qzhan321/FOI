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
saveRoundIndex <- 3
saveDir0 <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/", folder, "/round", saveRoundIndex, "/files/", arrivalType, "/FOI/inputs/MOIInd/MOIEstInputs/")
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
state <- "measurementError"
saveDir3 <- paste0(saveDir2, state, "/")
if (!dir.exists(saveDir3)) {
  dir.create(saveDir3)
}

readDir <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/", folder, "/round", saveRoundIndex, "/files/", arrivalType, "/infTablePresence/")
prefix <- "sim"
nums <- 1:3

preIRS <- 200
T_YEAR <- 360
ageCutoff <- 5
ageGroupLabel <- "0-5yrs"
nums_w_reps <- NULL
nRealizations <- 200
for (i in 1:length(nums)) {
  num <- nums[i]
  if (num %in% nums_w_reps) {
    reps <- 0:2
  } else {
    reps <- 0
  }
  if (num == min(nums)) {
    layers <- c((preIRS - 2)*T_YEAR + 300, (preIRS - 1)*T_YEAR + 180,
                (preIRS + 1)*T_YEAR + 300, (preIRS + 2)*T_YEAR + 180)
  } else {
    layers <- c((preIRS + 1)*T_YEAR + 300, (preIRS + 2)*T_YEAR + 180)
  }
  saveDir4 <- paste0(saveDir3, prefix, "_", num, "/")
  if (!dir.exists(saveDir4)) {
    dir.create(saveDir4)
  }
  for (r in reps) {
    sampleSqlFile <- paste(wd, seasonality, "/", openness, "/", prefix, "_", num, "/sqlitesDir/", prefix, "_", num, "_r", r, "_sd.sqlite",sep="")
    print(sampleSqlFile)
    db <- dbConnect(SQLite(),dbname = sampleSqlFile)
    
    sc <- "select * from hosts"
    hosts <- fetchdb(db, sc)
    colnames(hosts)[1:2] <- c("host_id", "pop_id")
    dbDisconnect(db)
    
    for (nRealization in 1:nRealizations) {
      load(paste0(readDir, seasonality, "/", openness, "/", prefix, "_", num, "_r", r, "_realization", nRealization, ".RData"))
      
      infStrainDf <- infStrain_pre %>% left_join(hosts, by = c("host_id", "pop_id"))
      infStrainDf <- infStrainDf %>% mutate(age = (time - birth_time)/T_YEAR)
      
      for (j in 1:length(layers)) {
        layer <- layers[j]
        infStrainDfSub <- infStrainDf %>% filter(presence == 1, time == layer, age <= ageCutoff)
        MOIInput <- infStrainDfSub %>% group_by(host_id) %>% summarise(DBLa_upsBC_rep_size = n_distinct(gene_id))
        colnames(MOIInput)[1] <- "HostID" 
        write.csv(MOIInput, file = paste0(saveDir4, "r", r, "_time_", layer, "_", ageGroupLabel, "_realization", nRealization, ".csv"), row.names = F)
      }
    }
  }
}