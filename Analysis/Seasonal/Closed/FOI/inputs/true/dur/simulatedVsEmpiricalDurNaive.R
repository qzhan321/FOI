rm(list = ls())
suppressPackageStartupMessages({
  library(dplyr)
  library(RSQLite)
  library(readxl)
  library(ggplot2)
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
sizeV <- 10
arrivalType <- "exponential"
folder <- "eLifeSubAugust2024"
wd <- paste0("/scratch/midway2/qizhan/PhD/projects/FOI/", folder, "/round", readRoundIndex, "/simulation/", arrivalType, "/")
empiricalDurations <- read_excel(paste0("/project2/pascualmm/QZ/PhD/projects/FOI/eLifeSubOct2025/files/empirical/MalariaTherapy_Smith_OpenMalaria_Subset.xlsx"))
empiricalDurations <- data.frame("duration" = empiricalDurations$LASTPOSDAY, "state" = "Empirical")
seasonality <- "seasonal"
openness <- "closed"
prefix <- "sim"
nums <- c(1:3)
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
  if (num %in% nums_w_reps) {
    reps <- 0:2
  } else {
    reps <- 0
  }
  if (num %in% c(1,7)) {
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
      sampled_dur <- fetchdb(db, sc) %>% filter(infection_count == 0)
      print(dim(sampled_dur))
      sampled_dur <- left_join(sampled_dur, hosts, by = c("host_id", "pop_id")) 
      sampled_dur <- sampled_dur %>% mutate(age = (time - birth_time)/T_YEAR)
      sampled_dur <- sampled_dur %>% filter(age <= ageCutoff, time > min(layers) - backward, time <= max(layers) + forward) 
      # sampled_dur <- sampled_dur[sample(nrow(sampled_dur), 1000), ]
      simulatedDurations <- data.frame("duration" = sampled_dur$duration - LIVER_STAGE, "state" = "Simulation")
      print(var(simulatedDurations$duration))
      durationsCombined <- bind_rows(empiricalDurations, simulatedDurations)
      
      p1 <- ggplot(durationsCombined, aes(duration, fill = state, col = state)) +
        geom_density(alpha = 0.5) + xlab("Duration (Days)") + ylab("Density") +
        theme(
          plot.title = element_text(color = "black", size = sizeV, hjust = 0.5),
          axis.title.x = element_text(color = "black", size = sizeV),
          axis.title.y = element_text(color = "black", size = sizeV),
          axis.text.x = element_text(color = "black", size = sizeV, angle = 0),
          axis.text.y = element_text(color = "black", size = sizeV, angle = 0),
          legend.text = element_text(color = "black", size = sizeV, angle = 0),
          legend.title = element_text(color = "black", size = sizeV, angle = 0))
    }
    dbDisconnect(db)
  }
}