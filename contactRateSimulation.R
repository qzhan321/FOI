rm(list = ls())
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
round <- "round1"
wd  <- paste0("/scratch/midway2/qizhan/PhD/projects/FOI/simulation", run, "/actualRuns/")
saveDir0 <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/writings/", round, "/plots/CR/")
dir.create(saveDir0)
seasonality <- "seasonal"
saveDir1 <- paste0(saveDir0, seasonality, "/")
dir.create(saveDir1)
openness <- "semi-open"
saveDir2 <- paste0(saveDir1, openness, "/")
dir.create(saveDir2)
prefix <- "sim"
nums <- 1:3
summaryAll <- NULL
for (i in 1:length(nums)) {
  num <- nums[i]
  sampleSqlFile<-paste(wd, seasonality, "/", openness, "/", prefix, "_", num, "/sqlitesDir/", prefix, "_", num, "_r0_sd.sqlite",sep="")
  print(sampleSqlFile)
  db<-dbConnect(SQLite(),dbname = sampleSqlFile)
  
  sc <- "select * from summary"
  summary <- fetchdb(db, sc)
  summary <- summary %>% mutate(no = num)
  summaryAll <- rbind(summaryAll, summary)
}

preIRS <- 150
T_YEAR <- 360
N <- 20000
sizeV <- 25
p1 <- ggplot(summaryAll %>% filter(time >= (preIRS - 3)*T_YEAR), 
             aes(x=time/T_YEAR - preIRS, y=n_total_bites/N, col = as.factor(no)))+ 
  geom_line(size = 1.9)+
  xlab("Years Since IRS Starts \n(Transient IRS)") + ylab("Effective Contact Rate \nper Host per Day") +
  theme_bw() + theme(
    plot.title = element_text(color="black", size=sizeV, hjust = 0.5),
    axis.title.x = element_text(color="black", size=sizeV),
    axis.title.y = element_text(color="black", size=sizeV),
    axis.text.x = element_text(color="black", 
                               size=sizeV, angle=0),
    axis.text.y = element_text(color="black", 
                               size=sizeV, angle=0),
    legend.text = element_text(color="black", 
                               size=sizeV, angle=0),
    legend.title = element_text(color="black", 
                                size=sizeV, angle=0),
    strip.text = element_text(color="black", 
                              size=sizeV, angle=0),
    aspect.ratio = 1) + scale_color_manual(name = "", labels = c("Low-coverage IRS", "Mid-coverage IRS", "High-coverage IRS"),
                                           values = c("#00AFBB", "#E7B800", "#FC4E07")) + 
  theme(legend.position = "none")
print(p1)
ggsave(paste0(saveDir2, "IRSContactRate.pdf"), p1, width = 6.25, height = 6.25)


