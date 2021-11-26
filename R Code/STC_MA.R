load("STC.RData")

#Hindcasting analysis function -------------------------------------------------
CV <- function(glmfit){
  data <- glmfit$model
  n <- nrow(data)
  seq_len <- 1:n
  Hindcast <- vector()
  for(i in 1:n) {
    j.out <- seq_len == i
    j.in <- seq_len != i
    ## we want data from here but formula from the parent.
    z <- data[j.in, , drop=FALSE]
    d.glm <- glm(y~., data = z, family = glmfit$family)
    Hindcast[i] <- predict(d.glm, data[j.out, , drop=FALSE], type = "link")
  }
  return(Hindcast)
}

### Model averaging ---------------------------------------------------------------
ma.loocv <- vector()
for(i in 1:8){
  w0 <- TC.gs[[i]]$c.models$weights
  u  <- matrix(rep(0,n*5),nrow = n)
  for(j in 1:5){
    fit  <- TC.gs[[i]]$c.models$models[[j]]
    u[,j] <- CV(fit)
  }
  v  <- u%*%w0
  ma.loocv <- cbind(ma.loocv, exp(u[,1]), exp(v))
}

# MSE -------------------------------------------------------------------------
MSE.g <- rep(0,8)
MSE.b <- rep(0,8)

for(i in 1:8){
  y <- w[,i]

  MSE.b[i] <- sum((y - ma.loocv[,2*i-1])^2)/45
  MSE.g[i] <- sum((y - ma.loocv[,2*i])^2)/45
}

save.image("STC_MA.RData")

