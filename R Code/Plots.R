load("STC.RData")

#I charts
for(i in 1:m){
  jpeg(file = paste("H:/UbuntuRv2/STC/Final/figures/Ichart/", colnames(w)[i],".png", sep = "" ),
       width = 400, height = 300)
  plots.ichart(TC.gs[[i]])
  dev.off();
}

#variable ranking
for(i in 1:m){
  jpeg(file = paste("H:/UbuntuRv2/STC/Final/figures/VariRank/", colnames(w)[i],".png", sep = "" ),
       width = 400, height = 300)
  plots.vr(TC.gs[[i]], n.vars = 10)
  dev.off();
}

#Model frequency
for(i in 1:m){
  jpeg(file = paste("H:/UbuntuRv2/STC/Final/figures/ModelFreq/", colnames(w)[i],".png", sep = "" ),
       width = 400, height = 300)
  plots.mf(TC.gs[[i]])
  dev.off();
}
