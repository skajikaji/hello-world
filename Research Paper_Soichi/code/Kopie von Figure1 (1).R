library(maptools)

result<-lm(fhpolrigaug~lrgdpch,data=F1)
R2=signif(summary(result)$r.squared,digit=4)
R="R^2="

plot(fhpolrigaug~lrgdpch,data=F1,col="white",
     ylab="Freedom House measure of democracy",xlab="Log GDP per pacita(Penn World Tables)",
     main="Figure1.Democacy and Income , 1990s",sub=paste(R,R2))
pointLabel(x=F1$lrgdpch,y=F1$fhpolrigaug,labels=F1$code,col="black")

abline(result)
