rm(list=ls())
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(cowplot)
  library(stringr)
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
run <- 5
wd <- paste0("/scratch/midway2/qizhan/PhD/projects/FOI/simulation", run, "/actualRuns/")
seasonality <- "seasonal"
openness <- "closed"
saveDir0 <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/files", run, "/actualRuns/MOI/")
dir.create(saveDir0)
saveDir1 <- paste0(saveDir0, seasonality, "/")
dir.create(saveDir1)
saveDir2 <- paste0(saveDir1, openness, "/")
dir.create(saveDir2)
fName <- "true"
saveDir3 <- paste0(saveDir2, fName, "/")
dir.create(saveDir3)

readDir <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/files", run, "/actualRuns/infTablePresence/")
prefix <- "sim"
nums <- 1:3

preIRS <- 200
IRSDur <- 2
T_YEAR <- 360
layers <- sort(c(c(preIRS - 1, preIRS + 1)*T_YEAR  + 300,
                 c(preIRS - 1, preIRS + 1)*T_YEAR  + 180))
nums_w_reps <- NULL
for (i in 1:length(nums)) {
  num <- nums[i]
  if (num %in% nums_w_reps) {
    reps <- 0:2
  } else {
    reps <- 0
  }
  for (r in reps) {
    sampleSqlFile<-paste(wd, seasonality, "/", openness, "/",prefix, "_", num, "/sqlitesDir/", prefix, "_", num, "_r", r, "_sd.sqlite",sep="")
    print(sampleSqlFile)
    db<-dbConnect(SQLite(),dbname = sampleSqlFile)
    
    sc <- "select * from hosts"
    hosts <- fetchdb(db, sc)
    colnames(hosts)[1:2] <- c("host_id", "pop_id")
    
    load(paste0(readDir, seasonality, "/", openness, "/", prefix, "_", num, "_r", r, ".RData"))
    infStrain_pre_sub <- infStrain_pre %>% filter(time %in% layers)
    
    df <- left_join(infStrain_pre_sub, hosts, by = c("host_id", "pop_id")) %>% 
      mutate(age = (time-birth_time)/T_YEAR) %>% group_by(time, host_id, pop_id, age) %>% 
      summarise(MOI = length(unique(uniqStrain)))
    
    sc <- paste0("select * from all_sampled_hosts where time IN (", paste(noquote(layers), collapse = ","), ")")
    all_sampled_hosts <- fetchdb(db, sc)
    all_sampled_hosts <- left_join(all_sampled_hosts, hosts, by = c("host_id")) %>% 
      mutate(age = (time - birth_time)/T_YEAR)
    
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
      df_zeros <- rbind(df_zeros, df_zeros_sub)
    }
    dfAll <- bind_rows(df, df_zeros)
    save(dfAll, file = paste0(saveDir3, "MOI_", num, "_rep", r, ".RData"))
  }
}
