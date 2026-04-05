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
readDir0 <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/", folder, "/round", saveRoundIndex, "/files/", arrivalType, "/FOI/inputs/MOIInd/MOIEst/")
readDir1 <- paste0(readDir0, seasonality, "/")
readDir2 <- paste0(readDir1, openness, "/")
state <- "drugUsage"
readDir3 <- paste0(readDir2, state, "/")

saveDir0 <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/", folder, "/round", saveRoundIndex, "/files/", arrivalType, "/FOI/inputs/MOIPop/")
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

posHostsMissedReadDir0 <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/", folder, "/round", saveRoundIndex, "/files/", arrivalType, "/FOI/inputs/MOIInd/posHostsMissed/")
posHostsMissedReadDir1 <- paste0(posHostsMissedReadDir0, seasonality, "/")
posHostsMissedReadDir2 <- paste0(posHostsMissedReadDir1, openness, "/")
posHostsMissedReadDir3 <- paste0(posHostsMissedReadDir2, state, "/")

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
  readDir4 <- paste0(readDir3, prefix, "_", num, "/")
  posHostsMissedReadDir4 <- paste0(posHostsMissedReadDir3, prefix, "_", num, "/")
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
      MOIAll <- NULL
      for (k in 1:length(layers)) {
        layer <- layers[k]
        for (nRealization in 1:nRealizations) {
          file <- paste0(readDir4, "r", r, "_time_", layer, "_", ageGroupLabel, "_realization", nRealization, ".RData")
          load(file)
          MOIPosDetected <- MOIPosDetected
          posHostsMissedFile <- paste0(posHostsMissedReadDir4, "r", r, "_time_", layer, "_", ageGroupLabel, "_realization", nRealization, ".RData")
          load(posHostsMissedFile)
          set.seed(k*nRealization)
          posHostsMissedImputed <- sample(unique(MOIPosDetected$HostID), length(posHostsMissed), replace = T)
          MOIPosMissedImputedList <- lapply(1:length(posHostsMissedImputed), function(x){MOIPosDetected %>% filter(HostID == posHostsMissedImputed[x]) %>% mutate(HostID = posHostsMissed[x])})
          MOIPosMissedImputed <- do.call("bind_rows", MOIPosMissedImputedList) 
          
          sc <- paste0("select * from all_sampled_hosts where time IN (", paste(noquote(layer), collapse = ","), ")")
          sampled_hosts <- fetchdb(db, sc)
          sampled_hosts_sub <- left_join(sampled_hosts, hosts, by = c("host_id")) %>% 
            mutate(age = (time - birth_time)/T_YEAR) %>% filter(age <= ageCutoff) %>%
            select(host_id) %>% mutate(MOI = 0)
          stopifnot(length(intersect(unique(MOIPosDetected$HostID), unique(MOIPosMissedImputed$HostID)))==0)
          hostsNoInf <- setdiff(unique(sampled_hosts_sub$host_id), union(unique(MOIPosDetected$HostID), unique(MOIPosMissedImputed$HostID)))
          sampled_hosts_sub <- sampled_hosts_sub %>% filter(host_id %in% hostsNoInf)
          colnames(sampled_hosts_sub)[1] <- "HostID"
          
          MOI <- do.call("bind_rows", list(MOIPosDetected, MOIPosMissedImputed, sampled_hosts_sub)) %>% mutate(time = layer, nRealization = nRealization)
          MOIAll <- rbind(MOIAll, MOI)
        }
      }
      MOIAllFinal <- MOIAll %>% group_by(MOI) %>% summarise(n = n()) %>% ungroup() %>% 
        mutate(n_total = sum(n), Prob = n/n_total, N = n_total/nRealizations) %>% select(-c(n, n_total))
      write.csv(MOIAllFinal, file = paste0(saveDir4, "r", r, "_", layersLabel, "_", ageGroupLabel, ".csv"), row.names = F)
    }
    dbDisconnect(db)
  }
}
