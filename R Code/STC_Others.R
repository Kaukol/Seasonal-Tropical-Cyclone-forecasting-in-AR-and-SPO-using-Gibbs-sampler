#ensure empty environment
rm(list = ls())
###set working directory
setwd("/mnt/h/UbuntuRv2/STC/Final")
source("StepAICc.R")
library(doParallel)
registerDoParallel(5)
set.seed(1)

#data
STC <- read.csv("STC.csv")

#Loocv for glm
CV <- function(glmfit){
  data <- glmfit$model
  n <- nrow(data)
  glm.y <- glmfit$y
  seq_len <- 1:n
  Hindcast <- vector()
  Call <- glmfit$call
  for(i in 1:n) {
    j.out <- seq_len == i
    j.in <- seq_len != i
    ## we want data from here but formula from the parent.
    Call$data <- data[j.in, , drop=FALSE]
    d.glm <- eval.parent(Call)
    Hindcast[i] <- predict(d.glm, data[j.out, , drop=FALSE], type = "link")
  }
  return(Hindcast)
}

#predictors
x   <- as.matrix(STC[,10:45])
colnames(x) <- c("DMSLP.Aug", "TMSLP.Aug",  "DMI.Aug",    "DMIE.Aug",    "DMIW.Aug",    "QBO.Aug",
                 "SOI.Aug",   "N12.Aug",    "N34.Aug",    "N3.Aug",      "N4.Aug",      "EMI.Aug",
                 "DMSLP.Sep", "TMSLP.Sep",  "DMI.Sep",    "DMIE.Sep",    "DMIW.Sep",    "QBO.Sep" ,
                 "SOI.Sep"  , "N12.Sep" ,   "N34.Sep" ,   "N3.Sep" ,     "N4.Sep" ,     "EMI.Sep" ,
                 "DMSLP.Oct", "TMSLP.Oct",  "DMI.Oct" ,   "DMIE.Oct" ,   "DMIW.Oct",    "QBO.Oct" ,
                 "SOI.Oct" ,  "N12.Oct" ,   "N34.Oct",    "N3.Oct"  ,    "N4.Oct" ,     "EMI.Oct")
x.s <- scale(x)

w   <- STC[,2:9]

n <- dim(x.s)[1]
p <- dim(x.s)[2]

STC0  <- read.csv("TC_original_data_Nino.csv")
x5var <- STC0$X5VAR[1:45]
Year  <- STC0$Year[1:45]

#LOOCV 5VAR and NULL

u.n5  <- vector()

for(i in 1:8){
  y <- w[,i]

  u.v <- foreach(j = 1:n, .combine=rbind) %dopar% {
    fit0 <- glm(y[-j]~1, family = "poisson")
    fit1 <- glm(y[-j]~x5var[-j], family = "poisson")

    u.0  <- exp(fit0$coefficients[1])
    u.1  <- exp(fit1$coefficients[1] + x5var[j]*fit1$coefficients[2])

    c(u.0, u.1)
  }

  u.n5 <- cbind(u.n5, u.v)
}

#StepAICc
u.aicc <- vector()

for(i in 1:8){
  y <- w[,i]
  z <- as.data.frame(cbind(y,x))

  full <- glm(y~., data = z, family = "poisson")
  fit.step <- stepAICc(full, trace = FALSE)
  u.v <- CV(fit.step)
  u.aicc <- cbind(u.aicc, exp(u.v))
}

#MSE ------------------------------------------------------------------
MSE.n <- rep(0,8)
MSE.x <- rep(0,8)

for(i in 1:8){
  y <- w[,i]

  MSE.n[i] <- sum((y- u.n5[,2*i-1])^2)/45
  MSE.x[i] <- sum((y- u.n5[,2*i])^2)/45
}

MSE.s <- rep(0,8)
for(i in 1:8){
  y <- w[,i]

  MSE.s[i] <- sum((y- u.aicc[,i])^2)/45
}

save.image("STC_Others.RData")
