#rm(list=ls())
getwd()
setwd("D:/MA Studium/WS16.17/Statistical Programming/Project")



#Import data set 
One=read.csv("Annual_panel.csv")
Five=read.csv("5yr_panel.csv")
Ten=read.csv("10yr_panel.csv")
Twenty=read.csv("20yr_panel.csv")

require(plm)
#Base sample with Five-year data
#T2. C1
Pooled_pols = plm(fhpolrigaug ~ lag(fhpolrigaug)+lag(lrgdpch)+factor(year), data = Five, subset = sample == "1", index = c("code","year"), model = "pooling")
summary(F_pols)

#T2. C2
FE_fols_2 = plm(fhpolrigaug ~ lag(fhpolrigaug)+lag(lrgdpch)+factor(country)+factor(year), data = Five, subset = sample == "1",index = c("country","year"), model = "within", effect = "individual")
summary(F_fols)

#T2. C5
FE_folswod_5= plm(fhpolrigaug ~ lag(lrgdpch)+factor(year)+factor(country), data = Five, subset = sample =="1", index = c("code","year"), model = "within", effect = "individual")
summary(F_folswod)


#T2. C6
FE_fols_6 =  plm(fhpolrigaug ~ lag(fhpolrigaug, 1:5)+lag(lrgdpch, 1:5)+factor(country)+factor(year), data = One, subset = sample == "1",index = c("country","year"), model = "within", effect = "individual")
summary(O_fols)


#T2. C7
FE_fols_7 = plm(fhpolrigaug ~ lag(fhpolrigaug)+lag(lrgdpch)+factor(country)+factor(year), data = Ten, subset = sample == "1",index = c("country","year"), model = "within", effect = "individual")
summary(T_fols)

#T2. C9
FE_fols_9 = plm(fhpolrigaug ~ lag(fhpolrigaug)+lag(lrgdpch)+factor(country)+factor(year), data = Twenty, subset = sample == "1",index = c("country","year"), model = "within", effect = "individual")
summary(Tw_fols)

library(stargazer)
stargazer(Pooled_pols, FE_fols_2, FE_folswod_5, FE_fols_7, FE_fols_9,  style = "aer", type= "html",
          se=list(clse(Pooled_pols),clse(FE_fols_2),clse(FE_folswod_5), clse(FE_fols_7),clse(FE_fols_9)),
          column.labels = c("Five-year data", "Ten-year data", "Twenty-year data"), column.separate = c(3,1,1),
          object.names = TRUE,
          no.space = TRUE, dep.var.labels=c("Dependent variable is democracy"), omit = c("factor", "Constant"), omit.stat = c("f"),
          covariate.labels=c("Democracy_{t-1}", "Log GDP per capita_{t-1}"),
          title="Table 2-Fixed Effects Results Using Freedom House Measure of Democracy", align=TRUE, out = "Table 2.htm")


