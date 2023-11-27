rm(list = ls())
suppressPackageStartupMessages({
  library(readxl)
})

EIR <- read_excel("~/others/PhD/projects/FOI/utils/GhanaSurveyEntomologyData.xlsx")
View(EIR)

df1 <- data.frame("EIR_month" = c(2.20590615, 2.50119774, 0, 1.67567568, NA, NA, 0, 5.92207792, 0, NA, NA, 
                                  0, 2.17382813, 0, 0, 1.33203125, 0, 0, 0, 0, 0, 0, 0,
                                  1.60724432, 0, 1.47347689, 0, 0, 0, 0, 0, 0.68181818), 
                  "loc" = "Vea/Gorie", 
                  "month" = c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", 
                              "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
                              "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"),
                  "year" = c(rep(2013, 11), rep(2014, 12), rep(2015, 9)),
                  "IRS" = c(rep("pre IRS", 8), rep("IRS", 17), rep("post IRS", 7)))

df2 <- data.frame("EIR_month" = c(1.09375, 0, 1.265625, 0, NA, NA, 10.6856061, 3.80739796, 6.9002193, NA, NA,
                                  0,0,0,0,0,0,0,0,0,0,0,0.96875,
                                  0,0,0.484375,0,0,0,0,0.8476563,0), 
                  "loc" = "Soe", 
                  "month" = c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", 
                              "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
                              "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"),
                  "year" = c(rep(2013, 11), rep(2014, 12), rep(2015, 9)),
                  "IRS" = c(rep("pre IRS", 8), rep("IRS", 17), rep("post IRS", 7)))
sum(df1$EIR_month[1:8], na.rm = T)/8*12
sum(df2$EIR_month[1:8], na.rm = T)/8*12