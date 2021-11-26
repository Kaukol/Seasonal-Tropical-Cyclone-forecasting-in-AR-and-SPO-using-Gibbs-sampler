load("STC_MA.RData")
load("STC_Others.RData")

#AR -------------
y.ar   <- w[,1]
y.ar.x <- u.n5[,2]
y.ar.g <- ma.loocv[,2]

year.0 <- paste(1970:1998,"/", 71:99, sep = "")
year.1 <- paste(1999:2008,"/0", 0:9, sep = "")
year.2 <- paste(2009:2014,"/", 10:15, sep = "")
Year   <- c(year.0, year.1, year.2)

jpeg(file = paste("H:/UbuntuRv2/STC/Final/figures/", colnames(w)[1],".png", sep = "" ),
     width = 800, height = 600)
plot(1:45, y.ar, main = "", xlab = "", ylab = "TC Counts", ylim = c(4,20), xaxt = "n",
     pch = 1, cex = 1, lwd = 2, type = "o")
lines(1:45, y.ar.x, pch = 2, cex = 1, lwd = 2, type = "o", col = "blue")
lines(1:45, y.ar.g, pch = 5, cex = 1, lwd = 2, type = "o", col = "red")
mtext(Year, side = 1, line = 0.25, at = 1:45, las = 2, cex = 1)
legend("topright", c("Actural", "X5VAR","GMA"),pch = c(1,2,5),col=c("black","blue","red"),bg ="white")
dev.off();

##SP ---------------------------------------------------------------------
y.sp   <- w[,6]
y.sp.x <- u.n5[,2*6]
y.sp.g <- ma.loocv[,2*6]

jpeg(file = paste("H:/UbuntuRv2/STC/Final/figures/", colnames(w)[6],".png", sep = "" ),
     width = 800, height = 600)
plot(1:45, y.sp, main = "", xlab = "", ylab = "TC Counts", ylim = c(0,20), xaxt = "n",
     pch = 1, cex = 1, lwd = 2, type = "o")
lines(1:45, y.sp.x, pch = 2, cex = 1, lwd = 2, type = "o", col = "blue")
lines(1:45, y.sp.g, pch = 5, cex = 1, lwd = 2, type = "o", col = "red")
mtext(Year, side = 1, line = 0.25, at = 1:45, las = 2, cex = 1)
legend("topright", c("Actural", "X5VAR","GMA"),pch = c(1,2,5),col=c("black","blue","red"),bg ="white")
dev.off();

##SS core ----------------------------------------------------------
MSE.tc <- rbind(MSE.n, MSE.x, MSE.s, MSE.b, MSE.g)

SS <- function(v){
  return(1-v/v[1])
}

SS.tc <- apply(MSE.tc, 2, SS)
