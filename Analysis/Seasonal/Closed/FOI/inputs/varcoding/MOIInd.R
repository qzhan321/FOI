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
saveDir0 <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/", folder, "/round", saveRoundIndex, "/files/", arrivalType, "/FOI/inputs/MOIIndVarcoding/MOIEst/")
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

readDir0 <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/", folder, "/round", saveRoundIndex, "/files/", arrivalType, "/")
fName <- "infTablePresence"
readDir1 <- paste0(readDir0, fName, "/")
prefix <- "sim"
nums <- 1:3

preIRS <- 200
IRSDur <- 3
T_YEAR <- 360
repertoireSize <- 45
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
  for (r in reps) {
    sampleSqlFile<-paste(wd, seasonality, "/", openness, "/",prefix, "_", num, "/sqlitesDir/", prefix, "_", num, "_r", r, "_sd.sqlite",sep="")
    print(sampleSqlFile)
    db<-dbConnect(SQLite(),dbname = sampleSqlFile)
    
    sc <- "select * from hosts"
    hosts <- fetchdb(db, sc)
    colnames(hosts)[1:2] <- c("host_id", "pop_id")
    
    sc <- paste0("select * from all_sampled_hosts where time IN (", paste(noquote(layers), collapse = ","), ")")
    all_sampled_hosts <- fetchdb(db, sc)
    all_sampled_hosts <- left_join(all_sampled_hosts, hosts, by = c("host_id")) %>% 
      mutate(age = (time - birth_time)/T_YEAR)
    
    for (nRealization in 1:nRealizations) {
      load(paste0(readDir1, seasonality, "/", openness, "/", prefix, "_", num, "_r", r, "_realization", nRealization, ".RData"))
      infStrain_pre_sub <- infStrain_pre %>% filter(time %in% layers, presence == 1)
      
      df <- left_join(infStrain_pre_sub, hosts, by = c("host_id", "pop_id")) %>% 
        mutate(age = (time-birth_time)/T_YEAR) %>% group_by(time, host_id, pop_id, age) %>% 
        summarise(DBLa_upsBC_rep_size = n_distinct(gene_id), MOI = ceiling(DBLa_upsBC_rep_size/repertoireSize)) %>% select(-DBLa_upsBC_rep_size)
      
      df_zeros <- NULL
      for (j in 1:length(layers)) {
        t <- layers[j]
        all_sampled_hosts_sub <- all_sampled_hosts %>% filter(time == t)
        df_sub <- df %>% filter(time == t)
        stopifnot(unique(df_sub$host_id) %in% unique(all_sampled_hosts_sub$host_id))
        hostIDs <- setdiff(unique(all_sampled_hosts_sub$host_id), unique(df_sub$host_id))
        all_sampled_hosts_sub_sub <- all_sampled_hosts_sub %>% filter(host_id %in% hostIDs)
        df_zeros_sub <- all_sampled_hosts_sub_sub %>% select(time, host_id, age, pop_id) %>% mutate(MOI = 0) 
        df_zeros_sub <- as_tibble(df_zeros_sub)
        df_zeros <- bind_rows(df_zeros, df_zeros_sub)
      }
      dfAll <- bind_rows(df, df_zeros)
      save(dfAll, file = paste0(saveDir2, "MOI_", num, "_rep", r, "_realization", nRealization, ".RData"))
    }
  }
}
