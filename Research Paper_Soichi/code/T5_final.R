#rm(list=c("D", "lag.fhpolrigaug", "lag.fhpolrigaug."))

require(plyr)

#T5.C1
TwoSLS_4=plm(fhpolrigaug~lag(lrgdpch)+factor(year)+factor(country)|.-lag(lrgdpch)+lag(nsave, 2)+factor(year)+factor(country), data=Five, index = c("code","year"),subset = sample =="1", model = "within", effect="individual") 
#Extract observations used in Two stage least squaure regression model in column(4). 
A = model.frame(TwoSLS_4)
#rename the variables
Five_inst = rename(A, c("factor(year)" = "year",  "factor(country)" = "country"))
F_pols_inst = plm(fhpolrigaug ~ lag.lrgdpch.+factor(year), data = Five_inst,index = c("country","year"), model = "pooling")
summary(F_pols_inst)


#T5.C2
F_folswod_inst = plm(fhpolrigaug ~ lag.lrgdpch.+factor(year)+factor(country), data = Five_inst, index = c("country","year"), model = "within", effect = "individual")
summary(F_folswod_inst)

#T5.C3
#To make a observation data set used in column (5)
TwoSLS_1=plm(fhpolrigaug~lag(lrgdpch)+lag(fhpolrigaug)+factor(year)+factor(country)|.-lag(lrgdpch)+lag(nsave, 2)+factor(year)+factor(country), data=Five, index = c("code","year"),subset = sample =="1", model = "within", effect="individual") 
#Extract observations used in Two stage least squaure regression model. 
B = model.frame(TwoSLS_5)
#Rename the variables in data B
Five_inst_5 = rename(B, c("lag(fhpolrigaug)"= "lag.fhpolrigaug", "lag(lrgdpch)"="lag.lrgdpch", "lag(nsave, 2)"="lag.2.nsave","factor(year)" = "year",  "factor(country)" = "country"))

F_fols_inst = plm(fhpolrigaug ~ lag.fhpolrigaug+lag.lrgdpch+factor(country)+factor(year), data = Five_inst_5, index = c("country","year"), model = "within", effect = "individual")
summary(F_fols_inst)


#T5.C4
# 2SLS regression by hand
#fit_lrgdpch=predict(Firststage)
#length(fit_lrgdpch)
#Five$fit_lrgdpch=predict(Firststage)

TwoSLS_4=plm(fhpolrigaug~lag(lrgdpch)+factor(year)+factor(country)|.-lag(lrgdpch)+lag(nsave, 2)+factor(year)+factor(country), data=Five, index = c("code","year"),subset = sample =="1", model = "within", effect="individual")  # Second Stage: Regress outcome on x and fitted values from the first stage
summary(TwoSLS_4)
# In first stage regression, we have to use F_pols_inst to consistent with the number of observations in 2nd stage. 
Five_inst_4= rename(A, c("lag(lrgdpch)" = "lag.lrgdpch", "lag(nsave, 2)" = "lag.2.nsave", "factor(year)" = "year",  "factor(country)" = "country") )
Firststage_4=plm(lag.lrgdpch~lag.2.nsave+factor(year), data = Five_inst_4, na.action = na.omit, index = c("code","year"), model = "within", effect="individual")
summary(Firststage_4)


#T5. C5
TwoSLS_5=plm(fhpolrigaug~lag(fhpolrigaug)+lag(lrgdpch)+factor(year)+factor(country)|.-lag(lrgdpch)+lag(fhpolrigaug)+lag(nsave, 2)+factor(year)+factor(country), data=Five, index = c("code","year"),subset = sample =="1", model = "within", effect="individual")  # Second Stage: Regress outcome on x and fitted values from the first stage
summary(TwoSLS_5)
Firststage_5=plm(lag.lrgdpch~lag.fhpolrigaug+lag.2.nsave+factor(year)+factor(country), data = Five_inst_5, na.action = na.omit, index = c("code","year"), model = "within", effect="individual")
summary(Firststage_5)


#T5. C7
TwoSLS_7=plm(fhpolrigaug~lag(lrgdpch)+lag(laborshare)+factor(year)+factor(country)|.-lag(lrgdpch)+lag(laborshare)+lag(nsave, 2)+factor(year)+factor(country), data=Five, index = c("code","year"),subset = sample =="1", model = "within", effect="individual")  # Second Stage: Regress outcome on x and fitted values from the first stage
summary(TwoSLS_7)

C = model.frame(TwoSLS_7)
Five_inst_7 = rename(C, c("lag(laborshare)"="lag.laborshare", "lag(lrgdpch)"="lag.lrgdpch", "lag(nsave, 2)"="lag.2.nsave","factor(year)" = "year",  "factor(country)" = "country"))

Firststage_7 = plm(lag.lrgdpch~lag.laborshare+lag.2.nsave+ factor(year) + factor(country), data = Five_inst_7, na.action = na.omit, index = c("code", "year"), model = "within", effect = "individual")
summary(Firststage_7)

#T5. C8
TwoSLS_8=plm(fhpolrigaug~lag(fhpolrigaug)+lag(fhpolrigaug,2)+lag(fhpolrigaug, 3)+lag(lrgdpch)+factor(year)+factor(country)|.-lag(lrgdpch)+lag(fhpolrigaug)+lag(nsave, 2)+factor(year)+factor(country), data=Five, index = c("code","year"),subset = sample =="1", model = "within", effect="individual")  # Second Stage: Regress outcome on x and fitted values from the first stage
summary(TwoSLS_8)
(R <- matrix(c(1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0),byrow=TRUE,nrow=3))
(r <- c(0,0,0))
linearHypothesis(TwoSLS_8, hypothesis.matrix=R, rhs = r)


D = model.frame(TwoSLS_8)
Five_inst_8 = rename(D, c("lag(fhpolrigaug)"="lag.fhpolrigaug","lag(fhpolrigaug, 2)"="lag.2.fhpolrigaug","lag(fhpolrigaug, 3)"="lag.3.fhpolrigaug" ,"lag(lrgdpch)"="lag.lrgdpch", "lag(nsave, 2)"="lag.2.nsave","factor(year)" = "year",  "factor(country)" = "country"))

Firststage_8=plm(lag.lrgdpch ~ lag.fhpolrigaug + lag.2.fhpolrigaug +lag.3.fhpolrigaug + lag.2.nsave+factor(year)+factor(country), data=Five_inst_8, index = c("code","year"), model = "within", effect="individual")  # Second Stage: Regress outcome on x and fitted values from the first stage
summary(Firststage_8)

(R <- matrix(c(1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0),byrow=TRUE,nrow=3))
(r <- c(0,0,0))
linearHypothesis(Firststage_8, hypothesis.matrix=R, rhs = r)


#T5. C9
TwoSLS_9=plm(fhpolrigaug~lag(lrgdpch)+factor(year)+factor(country)|.-lag(lrgdpch)+lag(nsave, 2) + lag(nsave, 3)+factor(year)+factor(country), data=Five, index = c("code","year"),subset = sample =="1", model = "within", effect="individual")  # Second Stage: Regress outcome on x and fitted values from the first stage
summary(TwoSLS_9)
E = model.frame(TwoSLS_9)
Five_inst_9 = rename(E, c("lag(lrgdpch)"="lag.lrgdpch", "lag(nsave, 2)"="lag.2.nsave", "lag(nsave, 3)"="lag.3.nsave","factor(year)" = "year",  "factor(country)" = "country"))


Firststage_9 = plm(lag.lrgdpch~lag.2.nsave+lag.3.nsave+factor(year)+factor(country), data=Five_inst_9, index = c("code","year"), model = "within", effect="individual")  # Second Stage: Regress outcome on x and fitted values from the first stage
summary(Firststage_9)
