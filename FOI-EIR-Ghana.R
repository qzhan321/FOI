rm(list = ls())
suppressPackageStartupMessages({
  library(stringr)
  library(ggplot2)
  library(cowplot)
  library(ggpubr)
  library(dplyr)
  library(gridExtra)
})
# historical paired EIR-FOI values
FOIms <- c(3.8, 3, 12.1, 0.12, 4.1, 16.9, 16.2, 3.9, (2.5-0.83)/2+0.83, 10.1, 
           (3.3-1.5)/2+1.5, 7.6, 5.4, 3.2, 6.3, 6.2, 2.9, 5.2, 4.2, 3.1, (0.87-0.8)/2+0.8, (14.6-9)/2+9, (4.1-1.2)/2+1.2, 0.93)
FOIls <- c(rep(NA, 8), 0.83, 6.3, 1.5, rep(NA, 9), 0.8, 9, 1.2, NA)
FOIus <- c(rep(NA, 8), 2.5, 11, 3.3, rep(NA, 9), 0.87, 14.6, 4.1, NA)
EIRms <- c(222,11,470,0.25,7.3,335,117, (53-27)/2+27, (27-13)/2+13, (26-12)/2+12, (14-2)/2+2, 37, 64, 18, 
           68, 129, 18, 132, 48, 116, 169, (307-193)/2+193, 418, 12)
EIRls <- c(rep(NA, 5), 270, NA, 27, 13, 12, 2, rep(NA, 9), 45, 193, NA, NA)
EIRus <- c(rep(NA, 5), 400, NA, 53, 27, 26, 14, rep(NA, 9), 293, 307, NA, NA)
EIRAndFOI <- data.frame("label" = LETTERS[1:24], "EIR_mean" = EIRms,
                        "EIR_upper" = EIRus, "EIR_lower" = EIRls,
                        "FOI_upper" = FOIus, "FOI_lower" = FOIls, 
                        "FOI_mean" = FOIms)
EIRAndFOI <- as_tibble(EIRAndFOI)
a <- 4.6
b <- 0.55
t <- 43

# Ghana surveys
T_YEAR <- 365
fois1 <- c(77, 73, 70)
pointData1 <- data.frame("EIR_mean" = (18.45729 + 25.27857)/2, 
                         "FOI_mean" = mean(T_YEAR/fois1),
                         "label" = "Pre-IRS")

fois4 <- c(4.878125, 5.158363, 5.309642)
pointData2 <- data.frame("EIR_mean" = (18.45729 + 25.27857)/2, 
                         "FOI_mean" = mean(fois4),
                         "label" = "Pre-IRS")
pointData12 <- bind_rows(pointData1 %>% mutate(method = "Two-Moment Approximation"), 
                         pointData2 %>% mutate(method = "Little's Law"))
pointData12$label <- factor(pointData12$label, levels = c("Pre-IRS"))
pointData12$method <- factor(pointData12$method, levels = c("Two-Moment Approximation", "Little's Law"))

al <- 4.6
bl <- 0.27
tl <- 7

ah <- 10
bh <- 0.55
th <- 60

sizeV <- 18
sizeVFactor <- 1.175
saveDir <- "/project2/pascualmm/QZ/PhD/projects/FOI/writings/round1/plots/FOI-EIR/"
dir.create(saveDir)
p1 <- ggplot(data = EIRAndFOI, aes(x = log10(EIR_mean), y = FOI_mean)) + geom_point(size = 2) + 
  geom_errorbar(aes(ymin = FOI_lower, ymax = FOI_upper)) +
  geom_errorbarh(aes(xmin = log10(EIR_lower), xmax = log10(EIR_upper))) +
  stat_function(fun=function(x) log(1+a*b*(10^(x)/T_YEAR)*t)/(a*t)*T_YEAR, col = "red") +
  geom_point(data = pointData12, aes(x = log10(EIR_mean), y = FOI_mean, col = label, shape = method), size = 4) +
  theme_classic() +
  theme(
    plot.title = element_text(color="black", size=sizeV, hjust = 0.5),
    axis.title.x = element_text(color="black", size=sizeV),
    axis.title.y = element_text(color="black", size=sizeV),
    axis.text.x = element_text(color="black", 
                               size=sizeV, angle=0),
    axis.text.y = element_text(color="black", 
                               size=sizeV, angle=0),
    legend.text = element_text(color="black", 
                               size=sizeV/sizeVFactor, angle=0),
    legend.title = element_text(color="black", 
                                size=sizeV, angle=0),
    strip.text = element_text(color="black", 
                              size=sizeV, angle=0)) +
  xlab("Annual EIR") + ylab("Annual FOI") +
  coord_cartesian(xlim = c(-1, 4)) +
  scale_x_continuous(breaks=c(0, 1, 2, 3, 4),
                     labels=c("1", "10", "100", "1000", "10000")) +
  scale_color_manual(name = "",
                     values = c("dark green"),
                     labels = c("Pre-IRS")) +
  scale_shape_manual(name = "",
                     values = c(5,3),
                     labels = c("Two-Moment \nApproximation", "Little's Law")) +
  theme(legend.position = c(0.7, 0.25)) + coord_flip() 
print(p1)
ggsave(paste0(saveDir, "FOI-EIR-Ghana.pdf"), p1, width = 5.5, height = 5)




ems <- c(58, 3.7, 39, 2.1, 1.8, 20, 7.2, (14-6.9)/2+6.9, 
         ((16+33)/2+(5.2+11)/2)/2,
         (1.2+2.6)/2,
         ((1.3+9.3)/2+(0.6+4.2)/2)/2,
         4.9, 12, 5.6, 11, 21, 6.2, 25, 11, 37,
         (211-194)/2+194,
         21, (348-102)/2+102, 13)
eus <- c(rep(NA, 5), 24, NA, 14, (16+33)/2, (1.9+4.1)/2, (9.3+1.3)/2, rep(NA, 9), (366+337)/2, NA, 348, NA)
els <- c(rep(NA, 5), 16, NA, 6.9, (5.2+11)/2, (1.1+2.4)/2, (0.6+4.2)/2, rep(NA, 9), (52+56)/2, NA, 102, NA)
efficiency <- data.frame("label" = LETTERS[1:24], "e_mean" = 1/ems,
                         "e_upper" = 1/els, "e_lower" = 1/eus,
                         "EIR_mean" = EIRms,
                         "EIR_upper" = EIRus, "EIR_lower" = EIRls)
pointData1 <- data.frame("EIR_mean" = (18.45729 + 25.27857)/2, 
                         "e_mean" = mean(T_YEAR/fois1)/((18.45729 + 25.27857)/2),
                         "label" = c("Pre-IRS"))
pointData2 <- data.frame("EIR_mean" = (18.45729 + 25.27857)/2, 
                         "e_mean" = mean(fois4)/((18.45729 + 25.27857)/2),
                         "label" = c("Pre-IRS"))
pointData12 <- bind_rows(pointData1 %>% mutate(method = "Two-Moment Approximation"), 
                         pointData2 %>% mutate(method = "Little's Law"))
pointData12$label <- factor(pointData12$label, levels = c("Pre-IRS"))
pointData12$method <- factor(pointData12$method, levels = c("Two-Moment Approximation", "Little's Law"))
p2 <- ggplot(data = efficiency, aes(x = log10(EIR_mean), y = log(e_mean))) + geom_point(size = 2) + 
  geom_errorbar(aes(ymin = log(e_lower), ymax = log(e_upper))) +
  geom_errorbarh(aes(xmin = log10(EIR_lower), xmax = log10(EIR_upper))) +
  stat_function(fun=function(x) log(log(1+a*b*(10^(x)/T_YEAR)*t)/(a*t)*T_YEAR/10^(x)), col = "red") +
  geom_point(data = pointData12, aes(x = log10(EIR_mean), y = log(e_mean), col = label, shape = method), size = 4) +
  theme_classic() +
  theme(
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
                              size=sizeV, angle=0)) +
  xlab("Annual EIR") + ylab("FOI/EIR (Transmsision Efficiency)") +
  coord_cartesian(xlim = c(-1, 3), ylim = c(-7, 0)) +
  scale_x_continuous(breaks=c(0, 1, 2, 3),
                     labels=c("1", "10", "100", "1000")) +
  scale_color_manual(name = "",
                     values = c("dark green"),
                     labels = c("Pre IRS")) +
  scale_shape_manual(name = "",
                     values = c(5,3),
                     labels = c("Two-Moment \nApproximation", "Little's Law")) +
  theme(legend.position = c(0.25, 0.4)) + 
  scale_y_continuous(breaks = c(-0.5108256, -2.302585, -6.214608),
                     labels = c("3/5", "1/10", "1/500")) + guides(shape = "none", color = "none")
print(p2)
ggsave(paste0(saveDir, "FOI-EIR-Ghana2.pdf"), p2, width = 5.5, height = 5)
