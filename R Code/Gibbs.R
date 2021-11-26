#ensure empty environment
rm(list = ls())
###set working directory
setwd("/mnt/h/UbuntuRv2/STC/Final")

library(IBGS)

#data
STC <- read.csv("STC.csv")

#predictors
x   <- as.matrix(STC[,10:45])
colnames(x) <- c("DMSLP.Aug", "TMSLP.Aug",  "DMI.Aug",    "DMIE.Aug",    "DMIW.Aug",    "QBO.Aug",
                 "SOI.Aug",   "N12.Aug",    "N34.Aug",    "N3.Aug",      "N4.Aug",      "EMI.Aug",
                 "DMSLP.Sep", "TMSLP.Sep",  "DMI.Sep",    "DMIE.Sep",    "DMIW.Sep",    "QBO.Sep" ,
                 "SOI.Sep"  , "N12.Sep" ,   "N34.Sep" ,   "N3.Sep" ,     "N4.Sep" ,     "EMI.Sep" ,
                 "DMSLP.Oct", "TMSLP.Oct",  "DMI.Oct" ,   "DMIE.Oct" ,   "DMIW.Oct",    "QBO.Oct" ,
                 "SOI.Oct" ,  "N12.Oct" ,   "N34.Oct",    "N3.Oct"  ,    "N4.Oct" ,     "EMI.Oct")
#response variables
w <- STC[,2:9]
m <- length(w)

n <- dim(x)[1]
p <- dim(x)[2]

### Model selection -----------------------------------------------------------
#Gibbs sampler results
TC.gs    <- list()
for(i in 1:m){
  y <- w[,i]
  TC.gs[[i]] <- GibbsSampler(y, x, n.models = 5, k = 2,
                             info = "AICc", family = "poisson")
}

save.image("STC.RData")
