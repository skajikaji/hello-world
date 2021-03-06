#rm(list=c("D", "lag.fhpolrigaug", "lag.fhpolrigaug."))

require(plyr)

#T5.C1
TwoSLS=plm(fhpolrigaug~lag(lrgdpch)+factor(year)+factor(country)|.-lag(lrgdpch)+lag(nsave, 2)+factor(year)+factor(country), data=Five, index = c("code","year"),subset = sample =="1", model = "within", effect="individual") 
#Extract observations used in Two stage least squaure regression model. 
A = model.frame(TwoSLS)
#rename the variables
Five_inst = rename(A, c("factor(year)" = "year",  "factor(country)" = "country"))
F_pols_inst = plm(fhpolrigaug ~ lag.lrgdpch.+factor(year), data = Five_inst,index = c("country","year"), model = "pooling")
summary(F_pols_inst)

#Test for multicollinearity by using pooled data
#VIF(Variance inflation factior)
vif(F_pols_inst)

#Breusch-Godfrey test
pbgtest(F_pols_inst)

#Breusch-Pagan test for homoskedastisity
plmtest(F_pols_inst, type="bp")


#T5.C2
F_folswod_inst = plm(fhpolrigaug ~ lag.lrgdpch.+factor(year)+factor(country), data = Five_inst, index = c("country","year"), model = "within", effect = "individual")
summary(F_folswod_inst)

#Breusch-Godfrey test
pbgtest(F_folswod_inst)

#Breusch-Pagan test for homoskedastisity
plmtest(F_folswod_inst, type="bp")



#T5.C3
#To make a observation data set used in column (5)
TwoSLS_1=plm(fhpolrigaug~lag(lrgdpch)+lag(fhpolrigaug)+factor(year)+factor(country)|.-lag(lrgdpch)+lag(nsave, 2)+factor(year)+factor(country), data=Five, index = c("code","year"),subset = sample =="1", model = "within", effect="individual") 
#Extract observations used in Two stage least squaure regression model. 
B = model.frame(TwoSLS_1)
#Rename the variables in data B
Five_inst_1 = rename(B, c("factor(year)" = "year",  "factor(country)" = "country"))

F_fols_inst = plm(fhpolrigaug ~ lag.fhpolrigaug.+lag.lrgdpch.+factor(country)+factor(year), data = Five_inst_1, index = c("country","year"), model = "within", effect = "individual")
summary(F_fols_inst)

#Breusch-Godfrey test
pbgtest(F_fols_inst)

#Breusch-Pagan test for homoskedastisity
plmtest(F_fols_inst, type="bp")




#T5.C4
# First stage regression
Firststage_4=plm(lag(lrgdpch)~lag(nsave, 2)+factor(year)+factor(country), data = Five, subset = sample == "1", na.action = na.omit, index = c("code","year"), model = "within", effect="individual")
summary(Firststage_4)

# 2SLS regression by hand
#fit_lrgdpch=predict(Firststage)
#length(fit_lrgdpch)
#Five$fit_lrgdpch=predict(Firststage)

TwoSLS=plm(fhpolrigaug~lag(lrgdpch)+factor(year)+factor(country)|.-lag(lrgdpch)+lag(nsave, 2)+factor(year)+factor(country), data=Five, index = c("code","year"),subset = sample =="1", model = "within", effect="individual")  # Second Stage: Regress outcome on x and fitted values from the first stage
summary(TwoSLS)

#Breusch-Godfrey test
pbgtest(TwoSLS)

#Breusch-Pagan test for homoskedastisity
plmtest(TwoSLS, type="bp")



#T5. C5
TwoSLS_5=plm(fhpolrigaug~lag(fhpolrigaug)+lag(lrgdpch)+factor(year)+factor(country)|.-lag(lrgdpch)+lag(fhpolrigaug)+lag(nsave, 2)+factor(year)+factor(country), data=Five, index = c("code","year"),subset = sample =="1", model = "within", effect="individual")  # Second Stage: Regress outcome on x and fitted values from the first stage
summary(TwoSLS_5)

Firststage_5=plm(lag(lrgdpch)~lag(fhpolrigaug)+lag(nsave, 2)+factor(year)+factor(country), data = Five, subset = sample == "1", na.action = na.omit, index = c("code","year"), model = "within", effect="individual")
summary(Firststage_5)

#Breusch-Godfrey test
pbgtest(TwoSLS_5)

#Breusch-Pagan test for homoskedastisity
plmtest(TwoSLS_5, type="bp")


#T5. C7
TwoSLS_7=plm(fhpolrigaug~lag(lrgdpch)+lag(laborshare)+factor(year)+factor(country)|.-lag(lrgdpch)+lag(laborshare)+lag(nsave, 2)+factor(year)+factor(country), data=Five, index = c("code","year"),subset = sample =="1", model = "within", effect="individual")  # Second Stage: Regress outcome on x and fitted values from the first stage
summary(TwoSLS_7)

#Breusch-Godfrey test
pbgtest(TwoSLS_7)

#Breusch-Pagan test for homoskedastisity
plmtest(TwoSLS_7, type="bp")



#T5. C8
TwoSLS_8=plm(fhpolrigaug~lag(fhpolrigaug, 1:3)+lag(lrgdpch)+factor(year)+factor(country)|.-lag(lrgdpch)+lag(fhpolrigaug)+lag(nsave, 2)+factor(year)+factor(country), data=Five, index = c("code","year"),subset = sample =="1", model = "within", effect="individual")  # Second Stage: Regress outcome on x and fitted values from the first stage
summary(TwoSLS_8)
(R <- matrix(c(1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0),byrow=TRUE,nrow=3))
(r <- c(0,0,0))
linearHypothesis(TwoSLS_8, hypothesis.matrix=R, rhs = r)
(R <- matrix(c(1,0,0,0,0,0,0,0,0,0,0), byrow=TRUE, nrow = 1))
(r = c(0))
linearHypothesis(TwoSLS_8, hypothesis.matrix=R, rhs = r)

#Breusch-Godfrey test
pbgtest(TwoSLS_8)

#Breusch-Pagan test for homoskedastisity
plmtest(TwoSLS_8, type="bp")


#T5. C9
TwoSLS_9=plm(fhpolrigaug~lag(lrgdpch)+factor(year)+factor(country)|.-lag(lrgdpch)+lag(nsave, 2:3)+factor(year)+factor(country), data=Five, index = c("code","year"),subset = sample =="1", model = "within", effect="individual")  # Second Stage: Regress outcome on x and fitted values from the first stage
summary(TwoSLS_9)

#Breusch-Godfrey test
pbgtest(TwoSLS_9)

#Breusch-Pagan test for homoskedastisity
plmtest(TwoSLS_9, type="bp")

