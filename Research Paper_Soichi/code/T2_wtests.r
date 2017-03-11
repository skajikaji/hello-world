#rm(list=ls())
getwd()
#setwd()
setwd("C:/myR/Dem_data")


#Import data set 
One=read.csv("Annual_panel.csv")
Five=read.csv("5yr_panel.csv")
Ten=read.csv("10yr_panel.csv")
Twenty=read.csv("20yr_panel.csv")

require(plm)
require(lmtest)
require(tseries)
require(car)
#Base sample with Five-year data
#T2. C1
F_pols = plm(fhpolrigaug ~ lag(fhpolrigaug)+lag(lrgdpch)+factor(year), data = Five, subset = sample == "1", index = c("code","year"), model = "pooling")
summary(F_pols)


#Test for multicollinearity by using pooled data
#VIF(Variance inflation factior)
vif(F_pols)

#Breusch-Godfrey test
pbgtest(F_pols)

#Breusch-Pagan test for homoskedastisity
plmtest(F_pols, type="bp")


#T2. C2 
F_fols = plm(fhpolrigaug ~ lag(fhpolrigaug)+lag(lrgdpch)+factor(country)+factor(year), data = Five, subset = sample == "1",index = c("country","year"), model = "within", effect = "individual")
summary(F_fols)

#Breusch-Godfrey test
pbgtest(F_fols)

#Breusch-Pagan test for homoskedastisity
plmtest(F_fols, type="bp")


#T2. C5
F_folswod = plm(fhpolrigaug ~ lag(lrgdpch)+factor(year)+factor(country), data = Five, subset = sample =="1", index = c("code","year"), model = "within", effect = "individual")
summary(F_folswod)

#Breusch-Godfrey test
pbgtest(F_folswod)

#Breusch-Pagan test for homoskedastisity
plmtest(F_folswod, type="bp")


#T2. C6 
O_fols =  plm(fhpolrigaug ~ lag(fhpolrigaug, 1:5)+lag(lrgdpch, 1:5)+factor(country)+factor(year), data = One, subset = sample == "1",index = c("country","year"), model = "within", effect = "individual")
summary(O_fols)

#Breusch-Godfrey test
pbgtest(O_fols)

#Breusch-Pagan test for homoskedastisity
plmtest(O_fols, type="bp")



#T2. C7
T_fols = plm(fhpolrigaug ~ lag(fhpolrigaug)+lag(lrgdpch)+factor(country)+factor(year), data = Ten, subset = sample == "1",index = c("country","year"), model = "within", effect = "individual")
summary(T_fols)

#Breusch-Godfrey test
pbgtest(T_fols)

#Breusch-Pagan test for homoskedastisity
plmtest(T_fols, type="bp")


#T2. C9
Tw_fols = plm(fhpolrigaug ~ lag(fhpolrigaug)+lag(lrgdpch)+factor(country)+factor(year), data = Twenty, subset = sample == "1",index = c("country","year"), model = "within", effect = "individual")
summary(Tw_fols)

#Breusch-Godfrey test
pbgtest(Tw_fols)

#Breusch-Pagan test for homoskedastisity
plmtest(Tw_fols, type="bp")
