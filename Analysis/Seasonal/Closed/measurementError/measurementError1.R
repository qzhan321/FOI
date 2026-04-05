rm(list = ls())
suppressPackageStartupMessages({
  library(RSQLite)
  library(reshape2)
  library(vegan)
  library(tidyverse)
  library(cowplot)
  library(gridExtra)
})
wdRoundIndex <- 1
arrivalType <- "exponential"
folder <- "eLifeSubAugust2024"
wd <- paste0("/scratch/midway2/qizhan/PhD/projects/FOI/", folder, "/round", wdRoundIndex, "/simulation/", arrivalType, "/")
seasonality <- "seasonal"
openness <- "closed"

saveRoundIndex <- 3
saveDir0 <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/", folder, "/round", saveRoundIndex, "/files/", arrivalType, "/")
if (!dir.exists(saveDir0)) {
  dir.create(saveDir0)
}
fName <- "infTablePresence"
saveDir1 <- paste0(saveDir0, fName, "/")
if (!dir.exists(saveDir1)) {
  dir.create(saveDir1)
}
saveDir2 <- paste0(saveDir1, seasonality, "/")
if (!dir.exists(saveDir2)) {
  dir.create(saveDir2)
}
saveDir3 <- paste0(saveDir2, openness, "/")
if (!dir.exists(saveDir3)) {
  dir.create(saveDir3)
}

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

nums <- 1:3
prefix <- "sim"

f <- read.table(paste0("/project2/pascualmm/QZ/PhD/projects/FOI/eLifeSubAugust2024/round1/files/exponential/FOI/inputs/MOIInd/measurementError/NbVarGenes_MOI1_upsBC_AllSurveys_Weight.txt"), header = T)
m_e <- rep(f$DBLa_upsBC_rep_size, f$n)
measurement_error <- function(infTable) {
  strains_len <- infTable %>% group_by(uniqStrain) %>% summarise(n=n())
  stopifnot(all(strains_len$n == 45))
  indices <- sample(m_e, size = nrow(strains_len), replace = T)
  pre_ab <- NULL
  for (i in 1:nrow(strains_len)) {
    index <- indices[i]
    strain <- strains_len$uniqStrain[i]
    pre_index <- sample(1:45, index)
    pre <- rep(0, 45)
    pre[pre_index] <- 1
    pre_ab_sub <- data.frame("uniqStrain" = strain, "presence" = pre)
    pre_ab <- rbind(pre_ab, pre_ab_sub)
  }
  stopifnot(all(infTable$uniqStrain==pre_ab$uniqStrain))
  infTable_pre <- cbind(infTable, pre_ab[,"presence", drop=F])
  return(infTable_pre)
}
preIRS <- 200
T_YEAR <- 360
IRSDur <- 3
nums_w_reps <- NULL
nRealizations <- 200
for (a in 1:length(nums)) {
  num <- nums[a]
  print(num)
  
  if (num %in% nums_w_reps) {
    reps <- 0:2
  } else {
    reps <- 0
  }
  if (num == min(nums)) {
    times <- c((preIRS - 2)*T_YEAR + 300, (preIRS - 1)*T_YEAR + 180,
               (preIRS + 1)*T_YEAR + 300, (preIRS + 2)*T_YEAR + 180)
  } else {
    times <- c((preIRS + 1)*T_YEAR + 300, (preIRS + 2)*T_YEAR + 180)
  }
  for (r in reps) {
    sampleSqlFile<-paste(wd, seasonality, "/", openness, "/", prefix, "_", num, "/sqlitesDir/", prefix, "_", num, "_r", r, "_sd.sqlite",sep="")
    print(sampleSqlFile)
    db<-dbConnect(SQLite(),dbname = sampleSqlFile)
    
    sc<-"select * from genes"
    genes<-fetchdb(db, sc)
    colnames(genes)[1]<-"gene_id"
    
    sc<-"select * from alleles"
    alleleInfo<-fetchdb(db, sc)
    geneAllele<-dcast(alleleInfo, gene_id~locus, value.var="allele")
    colnames(geneAllele)[2:ncol(geneAllele)]<-paste("l",1:(ncol(geneAllele)-1),sep="")
    
    sc<-paste0("select time, host_id, pop_id, infection_id, strain_id from sampled_infections where gene_id > -1 and time IN (", paste(noquote(times), collapse = ","), ")")
    sampledInf<-fetchdb(db,sc)
    infStrain<-sampledInf %>% mutate(uniqStrain = 1:nrow(sampledInf))
    
    strain_ids <- unique(infStrain$strain_id)
    sc<-paste0("select id, ind, gene_id from sampled_strains where id IN (", paste(noquote(strain_ids), collapse = ","), ")")
    strainInfo<-fetchdb(db, sc)
    colnames(strainInfo)[1]<-"strain_id"
    
    infStrain<-left_join(infStrain, strainInfo, by = "strain_id")
    infStrain<-left_join(infStrain, genes, by = "gene_id")
    infStrain<-left_join(infStrain, geneAllele, by = "gene_id")
    
    infStrain <- infStrain %>% mutate(gene_id = paste0(l1, "-", l2)) %>% select(-c(l1, l2, source, is_functional, infection_id, strain_id))
    
    for (nRealization in 1:nRealizations) {
      print(nRealization)
      set.seed(nrow(infStrain)*nRealization)
      infStrain_pre <- measurement_error(infStrain) 
      save(infStrain_pre, file = paste0(saveDir3, prefix, "_", num, "_r", r, "_realization", nRealization, ".RData"))
    }
    dbDisconnect(db)
  } 
}

