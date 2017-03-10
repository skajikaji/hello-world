[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **Deocracy And Income, Figures** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet:  'Democracy and Income'
Published in:      'Income and Democracy'
Description:       'Plot average of Freedom House democracy index and log GDP per capita in 1990s' 
Keywords:          'democracy, income per capita, plot'
See also:          
Author:            'Hyerin Park, Sharong Jiang, Soichi Kajisa'
Submitted:         'Fri, March 10 2017 by Soichi Kajisa'
Datafile:          'ave1990s.xlsx, change_FH.xlsx, change_polity4.xlsx, X5yr_panel.xlsx'
Input:  
Output:  
Example:  

```

![Picture1](f1notitle.pdf)
![Picture2](f2notitle.pdf)
![Picture3](f3notitle.pdf)
![Picture4](f4notitle.pdf)

### R Code:
```r
#Income and Democracy, 1990s
library(maptools)

result=lm(fhpolrigaug~lrgdpch,data=ave1990s)
R2=signif(summary(result)$r.squared,digit=4)
R="R^2="

plot(fhpolrigaug~lrgdpch,data=ave1990s,col="white",
     ylab="Freedom House measure of democracy",xlab="Log GDP per pacita(Penn World Tables)",
     sub=paste(R,R2))
pointLabel(x=ave1990s$lrgdpch,y=ave1990s$fhpolrigaug,labels=ave1990s$code,col="black")

abline(result)

#Change in Democracy and Income, 1970-1995
result2=lm(s5fhpolrigaug~s5lrgdpch,data=change_FH)
R2.2=signif(summary(resultf2)$r.squared,digit=4)
R="R^2="

plot(s5fhpolrigaug~s5lrgdpch,data=change_FH,type="n",
     ylab="Change in Freedom House measure of democracy",
     xlab="Change in Log GDP per pacita(Penn World Tables)",
     sub=paste(R,R2.2))
pointLabel(x=change_FH$s5lrgdpch,y=change_FH$s5fhpolrigaug,labels=change_FH$code,
           col="black")

abline(result2)

#Change in Democracy and Income, 1970-1995
result3=lm(s5polity4~s5lrgdpch,data=change_polity4)
R2.3=signif(summary(result3)$r.squared,digit=4)
R="R^2="

plot(s5polity4~s5lrgdpch,data=change_polity4,type="n",
     ylab="Change in Polity4 measure of democracy",
     xlab="Change in Log GDP per pacita(Penn World Tables)",
     sub=paste(R,R2.3))
text(x=change_polity4$s5polity4,y=change_polity4$s5fhpolrigaug,labels=change_polity4$code,
           col="black")

abline(result3)

#5year panel data

par(mfrow = c(3,4 ))          ##par is graphic parameter, mflow is to devide the graph in 12 areas
par(cex = 0.6)                #zoom rate for characters
par(mar = c(0, 0, 0, 0), oma = c(4, 4, 4, 2))　　#oma is for setting vacant space

x=1945
for (i in 1:11) {
  x=x+5
  plot(fhpolrigaug~lrgdpch,data=X5yr_panel, subset=year==x,
       xlim=c(6,10),ylim=c(0,1),ann=F, xaxt="n",yaxt="n")  #setting for length of graph by xlim and ylim, and erase whole title and axis by ann=F
  text(6.3,0.95,x, cex=1.5)　　　　　#label setting for each graph
  if (i %in% c (8:11) ) { axis (1 , col = " black ", col.axis= " black ", at = seq (6 , 10 , 2) ) }
  if (i %in% c(1 , 5 , 9) ) { axis (2 , col = " black ", col.axis = " black ", at = seq (0 , 1 , 0.5) ) }
  result<-lm(formula=fhpolrigaug~lrgdpch, data=X5yr_panel, subset=year==x)
  abline(result, col="blue")
  box(col = "grey60")
}
mtext("Freedom House measure of democracy", 
      side = 2, outer = TRUE, cex = 1, line = 2.2,col = "grey20")
mtext("Log GDP per capita(Penn World Tables)", 
      side = 1, outer = TRUE, cex = 1, line = 2.2,col = "grey20")

```
