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
saveDir0 <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/", folder, "/round", saveRoundIndex, "/files/", arrivalType, "/FOI/inputs/MOIInd/MOIEst/")
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
state <- "drugUsage"
saveDir3 <- paste0(saveDir2, state, "/")
if (!dir.exists(saveDir3)) {
  dir.create(saveDir3)
}

posHostsDetectedSaveDir0 <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/", folder, "/round", saveRoundIndex, "/files/", arrivalType, "/FOI/inputs/MOIInd/posHostsDetected/")
if (!dir.exists(posHostsDetectedSaveDir0)) {
  dir.create(posHostsDetectedSaveDir0)
}
posHostsDetectedSaveDir1 <- paste0(posHostsDetectedSaveDir0, seasonality, "/")
if (!dir.exists(posHostsDetectedSaveDir1)) {
  dir.create(posHostsDetectedSaveDir1)
}
posHostsDetectedSaveDir2 <- paste0(posHostsDetectedSaveDir1, openness, "/")
if (!dir.exists(posHostsDetectedSaveDir2)) {
  dir.create(posHostsDetectedSaveDir2)
}
posHostsDetectedSaveDir3 <- paste0(posHostsDetectedSaveDir2, state, "/")
if (!dir.exists(posHostsDetectedSaveDir3)) {
  dir.create(posHostsDetectedSaveDir3)
}

posHostsMissedSaveDir0 <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/", folder, "/round", saveRoundIndex, "/files/", arrivalType, "/FOI/inputs/MOIInd/posHostsMissed/")
if (!dir.exists(posHostsMissedSaveDir0)) {
  dir.create(posHostsMissedSaveDir0)
}
posHostsMissedSaveDir1 <- paste0(posHostsMissedSaveDir0, seasonality, "/")
if (!dir.exists(posHostsMissedSaveDir1)) {
  dir.create(posHostsMissedSaveDir1)
}
posHostsMissedSaveDir2 <- paste0(posHostsMissedSaveDir1, openness, "/")
if (!dir.exists(posHostsMissedSaveDir2)) {
  dir.create(posHostsMissedSaveDir2)
}
posHostsMissedSaveDir3 <- paste0(posHostsMissedSaveDir2, state, "/")
if (!dir.exists(posHostsMissedSaveDir3)) {
  dir.create(posHostsMissedSaveDir3)
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
  posHostsDetectedSaveDir4 <- paste0(posHostsDetectedSaveDir3, prefix, "_", num, "/")
  if (!dir.exists(posHostsDetectedSaveDir4)) {
    dir.create(posHostsDetectedSaveDir4)
  }
  posHostsMissedSaveDir4 <- paste0(posHostsMissedSaveDir3, prefix, "_", num, "/")
  if (!dir.exists(posHostsMissedSaveDir4)) {
    dir.create(posHostsMissedSaveDir4)
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
    
    load(paste0(readDir, seasonality, "/", openness, "/", prefix, "_", num, "_r", r, "_realization1.RData"))
    
    infStrainDf <- infStrain_pre %>% left_join(hosts, by = c("host_id", "pop_id"))
    infStrainDf <- infStrainDf %>% mutate(age = (time - birth_time)/T_YEAR)
    
    for (j in 1:length(layers)) {
      layer <- layers[j]
      infStrainDfSub <- infStrainDf %>% filter(time == layer, age <= ageCutoff)
      if (seasonality == "seasonal") {
        if (layer < preIRS*T_YEAR & layer %% T_YEAR == 300) {
          p_detection <- 1 - 0.575
        } else if (layer %% T_YEAR == 180) {
          p_detection <- 1 - 0.115
        } else if (layer > preIRS*T_YEAR & layer %% T_YEAR == 300) {
          p_detection <- 1 - 0.27
        }
      } else {
        if (layer <= preIRS*T_YEAR) {
          p_detection <- 1 - 0.575
        } else if (layer > preIRS*T_YEAR) {
          p_detection <- 1 - 0.27
        }
      }
      
      for (nRealization in 1:nRealizations) {
        seed.id <- (i*10000 + j)*nRealization 
        set.seed(seed.id) 
        posHostsDetected <- sample(unique(infStrainDfSub$host_id), round(p_detection*length(unique(infStrainDfSub$host_id))))
        save(posHostsDetected, file = paste0(posHostsDetectedSaveDir4, "r", r, "_time_", layer, "_", ageGroupLabel, "_realization", nRealization, ".RData"))
        posHostsMissed <- setdiff(unique(infStrainDfSub$host_id), posHostsDetected)
        save(posHostsMissed, file = paste0(posHostsMissedSaveDir4, "r", r, "_time_", layer, "_", ageGroupLabel, "_realization", nRealization, ".RData"))
        
        MOIPosDetected <- infStrainDfSub %>% filter(host_id %in% posHostsDetected) %>% group_by(host_id) %>% summarise(MOI = length(unique(uniqStrain)))
        colnames(MOIPosDetected)[1] <- "HostID" 
        save(MOIPosDetected, file = paste0(saveDir4, "r", r, "_time_", layer, "_", ageGroupLabel, "_realization", nRealization, ".RData"))
      }
    }
  }
}