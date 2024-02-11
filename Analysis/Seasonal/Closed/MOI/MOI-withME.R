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
fName <- "withME"
saveDir3 <- paste0(saveDir2, fName, "/")
dir.create(saveDir3)

sampledHostsSaveDir0 <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/files", run, "/actualRuns/MicroscopyPosHosts/")
dir.create(sampledHostsSaveDir0)
sampledHostsSaveDir1 <- paste0(sampledHostsSaveDir0, seasonality, "/")
dir.create(sampledHostsSaveDir1)
sampledHostsSaveDir2 <- paste0(sampledHostsSaveDir1, openness, "/")
dir.create(sampledHostsSaveDir2)
sampledHostsSaveDir3 <- paste0(sampledHostsSaveDir2, fName, "/")
dir.create(sampledHostsSaveDir3)

negHostsSaveDir0 <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/files", run, "/actualRuns/MicroscopyNegHosts/")
dir.create(negHostsSaveDir0)
negHostsSaveDir1 <- paste0(negHostsSaveDir0, seasonality, "/")
dir.create(negHostsSaveDir1)
negHostsSaveDir2 <- paste0(negHostsSaveDir1, openness, "/")
dir.create(negHostsSaveDir2)
negHostsSaveDir3 <- paste0(negHostsSaveDir2, fName, "/")
dir.create(negHostsSaveDir3)

readDir <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/files", run, "/actualRuns/infTablePresence/")
prefix <- "sim"
nums <- 1:3

load(paste0("/home/qizhan/others/PhD/projects/FOI/utils/s_givenMOI_list"))
p_s_givenMOI <- s_givenMOI_list
MOI_max <- 20
names(p_s_givenMOI) <- as.character(1:MOI_max)

MOI_priors <- rep(1/MOI_max, MOI_max)
names(MOI_priors) <- as.character(1:MOI_max)

f <- read.table(paste0("/home/qizhan/others/PhD/projects/FOI/utils/NbVarGenes_MOI1_upsBC_AllSurveys_Weight.txt"), header = T)

a <- min(f$DBLa_upsBC_rep_size)
b <- max(f$DBLa_upsBC_rep_size)

p <- f$n/sum(f$n)
names(p) <- as.character(f$DBLa_upsBC_rep_size)

s_single <- max(45, b)

preIRS <- 200
IRSDur <- 10
T_YEAR <- 360
layers <- sort(c(c(preIRS - 1, preIRS + 1)*T_YEAR  + 300,
                 c(preIRS - 1, preIRS + 1)*T_YEAR  + 180))

p_microscopy <- 1.0 # conversion factor between PCR and miscroscopy for 1-5 year old children
ageCutoff <- 5
ageGroupLabel <- "0-5yrs"
nums_w_reps <- NULL
for (i in 1:length(nums)) {
  num <- nums[i]
  if (num %in% nums_w_reps) {
    reps <- 0:2
  } else {
    reps <- 0
  }
  sampledHostsSaveDir4 <- paste0(sampledHostsSaveDir3, num, "/")
  dir.create(sampledHostsSaveDir4)
  negHostsSaveDir4 <- paste0(negHostsSaveDir3, num, "/")
  dir.create(negHostsSaveDir4)
  for (r in reps) {
    sampleSqlFile<-paste(wd, seasonality, "/", openness, "/",prefix, "_", num, "/sqlitesDir/", prefix, "_", num, "_r", r, "_sd.sqlite",sep="")
    print(sampleSqlFile)
    db<-dbConnect(SQLite(),dbname = sampleSqlFile)
    
    sc <- "select * from hosts"
    hosts <- fetchdb(db, sc)
    colnames(hosts)[1:2] <- c("host_id", "pop_id")
    
    load(paste0(readDir, seasonality, "/", openness, "/", prefix, "_", num, "_r", r, ".RData"))
    
    infStrain_pre <- infStrain_pre %>% left_join(hosts, by = c("host_id", "pop_id"))
    infStrain_pre <- infStrain_pre %>% mutate(age = (time - birth_time)/T_YEAR)
    
    df <- NULL
    df_supp <- NULL
    for (j in 1:length(layers)) {
      layer <- layers[j]
      infStrain_pre_sub <- infStrain_pre %>% filter(presence == 1, time == layer, age <= ageCutoff)
      set.seed(0) 
      hostsSub <- sample(unique(infStrain_pre_sub$host_id), round(p_microscopy*length(unique(infStrain_pre_sub$host_id))))
      microscopyPosHosts <- hostsSub
      save(microscopyPosHosts, file = paste0(sampledHostsSaveDir4, "rep_", r, "_time_", layer, "_", ageGroupLabel, ".RData"))
      microscopyNegHosts <- setdiff(unique(infStrain_pre_sub$host_id), hostsSub)
      save(microscopyNegHosts, file = paste0(negHostsSaveDir4, "rep_", r, "_time_", layer, "_", ageGroupLabel, ".RData"))
      
      infStrain_pre_sub <- infStrain_pre_sub %>% filter(host_id %in% hostsSub)
      s_temp_dat <- infStrain_pre_sub %>% group_by(pop_id, host_id, age) %>% summarise(n = n_distinct(gene_id))
      
      s_temp <- s_temp_dat$n
      names <- s_temp_dat$host_id
      popids <- s_temp_dat$pop_id
      ages <- s_temp_dat$age
      df_sub <- NULL
      for (k in 1:length(s_temp)) {
        s <- s_temp[k]
        if (s >= a & s <= s_single*MOI_max) {
          numerator <- 0
          for (b in 1:MOI_max) {
            if (!is.na(p_s_givenMOI[[as.character(b)]][as.character(s)])) {
              numerator <- numerator + p_s_givenMOI[[as.character(b)]][as.character(s)]*MOI_priors[as.character(b)]
            }
          }
          
          if (numerator == 0) {
            p_c_givens_all <- rep(0, length(1:MOI_max))
          } else {
            p_c_givens_all <- rep(NA, length(1:MOI_max))
            for (c in 1:MOI_max) {
              if (!is.na(p_s_givenMOI[[as.character(c)]][as.character(s)])) {
                temp <- p_s_givenMOI[[as.character(c)]][as.character(s)]*MOI_priors[as.character(c)]/numerator
                names(temp) <- NULL
              } else {
                temp <- 0
              }
              p_c_givens_all[c] <- temp
            }
          }
          
          print(sum(p_c_givens_all))
          
          df_sub_temp <- data.frame("p" = p_c_givens_all, "MOI" = 1:MOI_max, "numDBLaTypes" = s, 
                           "host_id" = names[k], "age" = ages[k], "time" = layer,
                           "pop_id" = popids[k])
          df_sub <- rbind(df_sub, df_sub_temp)
        }
      }
      df <- rbind(df, df_sub)
      
      set.seed(1)
      names_supp_temp <- sample(names, length(microscopyNegHosts), replace = T)
      # names_supp_temp_df <- data.frame("host_id" = names_supp_temp, "true_host_id" = microscopyNegHosts)
      df_supp_temp <- lapply(names_supp_temp, function(x){df_sub %>% filter(host_id == x)}) 
      df_supp_temp <- do.call(rbind, df_supp_temp)
      if (!is.null(df_supp_temp)) {
        # df_supp_temp <- df_supp_temp %>% left_join(names_supp_temp_df, by = "host_id") %>% select(-host_id) %>% mutate(host_id = true_host_id) %>% select(-true_host_id) 
        df_supp <- bind_rows(df_supp, df_supp_temp)
      }
    }
    
    # zeros
    sc <- paste0("select * from all_sampled_hosts where time IN (", paste(noquote(layers), collapse = ","), ")")
    all_sampled_hosts <- fetchdb(db, sc)
    all_sampled_hosts <- left_join(all_sampled_hosts, hosts, by = c("host_id")) %>% 
      mutate(age = (time - birth_time)/T_YEAR)
    df_zeros <- NULL
    for (m in 1:length(layers)) {
      t <- layers[m]
      all_sampled_hosts_sub <- all_sampled_hosts %>% filter(age <= ageCutoff, time == t)
      df_sub <- df %>% filter(time == t)
      # if (!is.null(df_supp)) {
      #   df_supp_temp <- df_supp %>% filter(time == t)
      # } else {
      #   df_supp_temp <- NULL
      # }
      load(file = paste0(negHostsSaveDir4, "rep_", r, "_time_", t, "_", ageGroupLabel, ".RData"))
      stopifnot(unique(df_sub$host_id) %in% unique(all_sampled_hosts_sub$host_id))
      stopifnot(unique(microscopyNegHosts) %in% unique(all_sampled_hosts_sub$host_id))
      hostIDs <- setdiff(unique(all_sampled_hosts_sub$host_id), union(unique(df_sub$host_id), unique(microscopyNegHosts)))
      all_sampled_hosts_sub_sub <- all_sampled_hosts_sub %>% filter(host_id %in% hostIDs)
      df_zeros_sub <- all_sampled_hosts_sub_sub %>% select(time, host_id, age, pop_id) 
      
      df_zeros_sub <- as_tibble(df_zeros_sub)
      
      df_zeros_sub_all <- df_zeros_sub %>% mutate(MOI = 0, p = 1, numDBLaTypes = 0)
      for (n in 1:MOI_max) {
        temp <- df_zeros_sub %>% mutate(MOI = n, p = 0, numDBLaTypes = 0)
        df_zeros_sub_all <- rbind(df_zeros_sub_all, temp)
      }
      df_zeros <- rbind(df_zeros, df_zeros_sub_all)
    }
    
    dfAll <- bind_rows(df, df_supp, df_zeros)
    save(dfAll, file = paste0(saveDir3, "MOI_", num, "_rep", r, ".RData"))
  }
}
