rm(list = ls())
suppressPackageStartupMessages({
  library(RSQLite)
  library(reshape2)
  library(vegan)
  library(tidyverse)
  library(cowplot)
  library(gridExtra)
})
run <- 5
wd <- paste0("/scratch/midway2/qizhan/PhD/projects/FOI/simulation", run, "/")
seasonality <- "seasonal"
openness <- "closed"
runCatgory <- "actualRuns"

saveDir0 <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/files", run, "/", runCatgory, "/")
dir.create(saveDir0)
fName <- "infTablePresence"
saveDir1 <- paste0(saveDir0, fName, "/")
dir.create(saveDir1)
saveDir2 <- paste0(saveDir1, seasonality, "/")
dir.create(saveDir2)
saveDir3 <- paste0(saveDir2, openness, "/")
dir.create(saveDir3)

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

nums <- c(1:3)
prefix <- "sim"

f <- read.table(paste0("~/others/papersOfficial/FOI/analysis/utils/NbVarGenes_MOI1_upsBC_AllSurveys_Weight.txt"), header = T)
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
IRSDur <- 2
postIRS <- 10-IRSDur
times <- sort(c(c(preIRS - 1, preIRS + IRSDur - 1, preIRS + IRSDur + 1)*T_YEAR + 300,
                c(preIRS - 1, preIRS + IRSDur - 1, preIRS + IRSDur + 1)*T_YEAR + 180))
nums_w_reps <- NULL
for (a in 1:1) {
  num <- nums[a]
  print(num)
  
  if (num %in% nums_w_reps) {
    reps <- 0:2
  } else {
    reps <- 0
  }
  
  for (r in reps) {
    sampleSqlFile<-paste(wd, runCatgory, "/", seasonality, "/", openness, "/", prefix, "_", num, "/sqlitesDir/", prefix, "_", num, "_r", r, "_sd.sqlite",sep="")
    print(sampleSqlFile)
    db<-dbConnect(SQLite(),dbname = sampleSqlFile)
    
    sc<-"select * from genes"
    genes<-fetchdb(db, sc)
    colnames(genes)[1]<-"gene_id"
    
    sc<-"select * from alleles"
    alleleInfo<-fetchdb(db, sc)
    geneAllele<-dcast(alleleInfo, gene_id~locus, value.var="allele")
    colnames(geneAllele)[2:ncol(geneAllele)]<-paste("l",1:(ncol(geneAllele)-1),sep="")
    
    sc<-paste0("select time, host_id, pop_id, strain_id from sampled_infections where gene_id > -1 and time IN (", paste(noquote(times), collapse = ","), ")")
    sampledInf<-fetchdb(db,sc)
    infStrain<-sampledInf %>% mutate(uniqStrain = 1:nrow(sampledInf))
    
    strain_ids <- unique(infStrain$strain_id)
    sc<-paste0("select id, ind, gene_id from sampled_strains where id IN (", paste(noquote(strain_ids), collapse = ","), ")")
    strainInfo<-fetchdb(db, sc)
    colnames(strainInfo)[1]<-"strain_id"
    
    infStrain<-left_join(infStrain, strainInfo, by = "strain_id")
    infStrain<-left_join(infStrain, genes, by = "gene_id")
    infStrain<-left_join(infStrain, geneAllele, by = "gene_id")
    
    infStrain <- infStrain %>% mutate(gene_id = paste0(l1, "-", l2))
    
    infStrain_pre <- measurement_error(infStrain) 
    dbDisconnect(db)
    save(infStrain_pre, file = paste0(saveDir3, prefix, "_", num, "_r", r, ".RData"))
  } 
}
