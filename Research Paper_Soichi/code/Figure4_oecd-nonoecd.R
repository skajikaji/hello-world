result_noecd<-lm(formula=fhpolrigaug~lrgdpch,data=subset(F1,OECD==0))
result_oecd<-lm(formula=fhpolrigaug~lrgdpch,data=subset(F1,OECD==1))
R.oecd=signif(summary(result_oecd)$r.squared,digit=4)
Rsqu="OECD countires R^2="
Rnsqu=", Non-OECD countires R^2="
R.noecd=signif(summary(result_noecd)$r.squared,digit=4)

plot(fhpolrigaug~lrgdpch,data=F1,
     pch=20,col=ifelse(OECD==1,"red","blue"),
     ylab="Freedom House measure of democracy",
     xlab="Log GDP per pacita(Penn World Tables)",
     main="Figure4. Difference of OECD Contries and Non-OECD Countries",
     sub=paste(Rsqu,R.oecd,Rnsqu,R.noecd))

text(9.7,0.9,"OECD")
text(10,0.7,"Non-OECD")
text(6.5,1,"OECD",col="red")
text(6.5,0.95,"Non-OECD",col="blue")

abline(result_noecd)
abline(result_oecd)
