rm(list = ls())
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(stringr)
  library(ggpubr)
  library(cowplot)
  library(RSQLite)
})

readDir <- "/home/qizhan/others/PhD/projects/FOI/utils/"
f <- read.table(paste0(readDir, "NbVarGenes_MOI1_upsBC_AllSurveys_Weight.txt"), header = T)
round <- 1
saveDir <- paste0("/project2/pascualmm/QZ/PhD/projects/FOI/writings/round", round, "/plots/me/")
dir.create(saveDir)
xlabel <- expression(paste("Number of Non-upsA ", italic("var"), " Genes Per Repertoire"))
xlabel <- bquote(atop(paste("Number of Non-upsA"), paste(italic("var"), " Genes Per Repertoire")))
sizeV <- 25

p1 <- ggplot(f, aes(x = DBLa_upsBC_rep_size, y = n))+ 
  geom_bar(stat = "identity")+
  xlab(xlabel) + ylab("Count") +
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
                              size=sizeV, angle=0)) 
p1 <- p1 + guides(fill = "none", col = "none") 
print(p1)
ggsave(paste0(saveDir, "measurementError.pdf"), p1, width = 6.25, height = 6.25)

