library(maptools)

resultf2<-lm(s5fhpolrigaug~s5lrgdpch,data=F2)
R2.2=signif(summary(resultf2)$r.squared,digit=4)
R="R^2="

plot(s5fhpolrigaug~s5lrgdpch,data=F2,type="n",
     ylab="Change in Freedom House measure of democracy",
     xlab="Change in Log GDP per pacita(Penn World Tables)",
     main="Figure2, Change in Democracy and Income . 1970-1995",
     sub=paste(R,R2.2))
pointLabel(x=F2$s5lrgdpch,y=F2$s5fhpolrigaug,labels=F2$code,
           col="black")

abline(resultf2)
