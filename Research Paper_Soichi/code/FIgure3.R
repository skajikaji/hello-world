library(maptools)

result3<-lm(s5polity4~s5lrgdpch,data=F3)
R2.3=signif(summary(resultf3)$r.squared,digit=4)
R="R^2="

plot(s5polity4~s5lrgdpch,data=F3,type="n",
     ylab="Change in Freedom House measure of democracy",
     xlab="Change in Log GDP per pacita(Penn World Tables)",
     main="Figure2, Change in Democracy and Income . 1970-1995",
     sub=paste(R,R2.3))
pointLabel(x=F3$s5polity4,y=F3$s5fhpolrigaug,labels=F3$code,
           col="black")

abline(result3)

