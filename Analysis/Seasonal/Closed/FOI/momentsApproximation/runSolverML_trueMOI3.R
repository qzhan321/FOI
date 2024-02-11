rm(list = ls())
suppressPackageStartupMessages({
  library(RSQLite)
  library(reshape2)
  library(vegan)
  library(tidyverse)
  library(cowplot)
  library(gridExtra)
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

c <- 20
r <- 0

source("/home/qizhan/others/PhD/projects/FOI/utils/solver.R")
preIRS <- 200
T_YEAR <- 360
IRSDur <- 2
postIRS <- 10-IRSDur
timesList <- list(c((preIRS - 1)*T_YEAR+180, (preIRS - 1)*T_YEAR+300),
                  c((preIRS + IRSDur - 1)*T_YEAR+180, (preIRS + IRSDur - 1)*T_YEAR+300))
labels <- c("Pre-IRS", "IRS") 

seasonality <- "seasonal"
openness <- "closed"
run <- 5
MOIReadDir0 <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/files", run, "/actualRuns/MOI/")
MOIReadDir1 <- paste0(MOIReadDir0, seasonality, "/")
MOIReadDir2 <- paste0(MOIReadDir1, openness, "/")
MOIstate <- "true"
MOIReadDir3 <- paste0(MOIReadDir2, MOIstate, "/")

wd <- paste0("/scratch/midway2/qizhan/PhD/projects/FOI/simulation", run, "/actualRuns/")
nums <- 1:3
prefix <- "sim"

saveDir1 <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/files", run, "/actualRuns/FOI/twoMoments/")
dir.create(saveDir1)
saveDir2 <- paste0(saveDir1, seasonality, "/")
dir.create(saveDir2)
saveDir3 <- paste0(saveDir2, openness, "/")
dir.create(saveDir3)
saveDir4 <- paste0(saveDir3, MOIstate, "/")
dir.create(saveDir4)

ageCutoff <- 5

VarInterArrivals_hightrans = seq(0,40000,100)
meanInterarrivals_hightrans = seq(1,90,1)
VarInterArrivals_midtrans = seq(0,90000,100)
meanInterarrivals_midtrans = seq(1,200,1)
VarInterArrivals_lowtrans = seq(0,1000000,1000)
meanInterarrivals_lowtrans = seq(200,900,2)

if (seasonality == "seasonal") {
  forward <- 60
  backward <- 180
} else if (seasonality == "non-seasonal") {
  forward <- 0
  backward <- 180
}

nums_w_reps <- NULL
for (a in 3:3) {
  no <- nums[a]
  if (no %in% nums_w_reps) {
    reps <- 0:2
  } else {
    reps <- 0
  }
  for (rep in reps) {
    file <- paste0(MOIReadDir3, "MOI_", no, "_rep", rep, ".RData")
    load(file)
    
    MOIsAll <- dfAll
    
    sampleSqlFile<-paste(wd, seasonality, "/", openness, "/", prefix, "_", no,"/sqlitesDir/", prefix, "_", no, "_r", rep, "_sd.sqlite",sep="")
    print(sampleSqlFile)
    db<-dbConnect(SQLite(),dbname = sampleSqlFile)
    
    if (no %in% c(1,4,7)) {
      ds <- 1:length(timesList)
    } else {
      ds <- 2
    }
    
    for (d in ds) {
      label <- labels[d]
      times <- timesList[[d]]
      
      sc <- "select * from hosts"
      hosts <- fetchdb(db, sc)
      colnames(hosts)[1:2] <- c("host_id", "pop_id")
      
      MOIs <- MOIsAll %>% filter(time %in% times, age <= ageCutoff)
      MOI_temp <- MOIs$MOI
      print(dim(MOIs))
      
      sc <- paste0("select * from sampled_duration where time > ", min(times) - backward, " AND time <= ", max(times) + forward)
      sampled_dur <- fetchdb(db, sc)
      print(dim(sampled_dur))
      sampled_dur <- left_join(sampled_dur, hosts, by = c("host_id", "pop_id"))
      
      sampled_dur <- sampled_dur %>% mutate(age = (time - birth_time)/T_YEAR)
      dursEmpi <- sampled_dur %>% filter(age <= ageCutoff) 
      dursEmpi <- dursEmpi$duration - 14
      
      sc <- "select * from summary"
      summary <- fetchdb(db, sc)
      if (openness == "semi-open") {
        summary1 <- summary %>% filter(time > min(times) - backward, time <= max(times) + forward, pop_id == 0)
        summary2 <- summary %>% filter(time > min(times) - backward, time <= max(times) + forward, pop_id == 1)
        FOI_actual1 <- sum(summary1$FOI)
        FOI_actual2 <- sum(summary2$FOI)
      } else {
        summary <- summary %>% filter(time > min(times) - backward, time <= max(times) + forward, pop_id == 0)
        FOI_actual <- sum(summary$FOI)
      }
      
      VarServiceT = var(dursEmpi)
      meanServiceT = mean(dursEmpi) 
      
      if (no %in% c(2,5,8) & label %in% labels[2]) {
        VarInterArrivals = VarInterArrivals_midtrans
        meanInterarrivals = meanInterarrivals_midtrans
      } else if (no %in% c(3,6,9) & label %in% labels[2]) {
        VarInterArrivals = VarInterArrivals_lowtrans
        meanInterarrivals = meanInterarrivals_lowtrans
      } else if (label %in% labels[1] | no %in% c(1,4,7)) {
        VarInterArrivals = VarInterArrivals_hightrans
        meanInterarrivals = meanInterarrivals_hightrans
      }
      
      FOI_est <- NULL
      for (i in 1:length(meanInterarrivals)) {
        meanInterarrival <- meanInterarrivals[i]
        for (j in 1:length(VarInterArrivals)) {
          VarInterArrival <- VarInterArrivals[j]
          P_i_all <- estQLDist(Va = VarInterArrival, ma = meanInterarrival,
                               Vs = VarServiceT, ms = meanServiceT, c = c, r = r)
          l <- lh_nlogT2(p = P_i_all, MOI = MOI_temp)
          
          # plot(x=0:(c+r), y=P_i_all, type = "l", ylim = c(0, max(c(P_i_all, prop.table(table(MOI_temp)))))) + title(paste(meanInterarrival, "-", VarInterArrival, "-", l))
          # points(x = as.numeric(names(prop.table(table(MOI_temp)))), y = prop.table(table(MOI_temp)), col = "red", type = "l")
          
          df <- data.frame("likelihood" = l$lh, "minp" = l$minp, "meanInterarrival" = meanInterarrival,
                           "VarInterArrival" = VarInterArrival)
          FOI_est <- rbind(FOI_est, df)
        } 
      }
      
      if (openness == "semi-open") {
        save(FOI_est, FOI_actual1, FOI_actual2, dursEmpi, MOIs, MOI_temp, file = paste0(saveDir4, prefix, "_", no, "rep", rep, "_", label, "_", MOIstate, ".RData"))
      } else {
        save(FOI_est, FOI_actual, dursEmpi, MOIs, MOI_temp, file = paste0(saveDir4, prefix, "_", no, "rep_", rep, "_", label, "_", MOIstate, ".RData"))
      }
    }
  }
}

x <- FOI_est %>% arrange(-desc(likelihood))
