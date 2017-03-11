#rm(list=c("D", "lag.fhpolrigaug", "lag.fhpolrigaug."))

require(plyr)
require(reshape)
require(stargazer)



#T5.C1
TwoSLS_1=plm(fhpolrigaug~lag(lrgdpch)+factor(year)+factor(country)|.-lag(lrgdpch)+lag(nsave, 2)+factor(year)+factor(country), data=Five, index = c("code","year"),subset = sample =="1", model = "within", effect="individual") 
#Extract observations used in Two stage least squaure regression model. 
A = model.frame(TwoSLS)
#rename the variables
Five_inst_1 = rename(A, c("factor(year)" = "year",  "factor(country)" = "country"))
F_pols_inst_1 = plm(fhpolrigaug ~ lag(lrgdpch)+factor(year), data = Five_inst,index = c("country","year"), model = "pooling")
summary(F_pols_inst_1)


#T5.C2
F_folswod_inst_2= plm(fhpolrigaug ~ lag(lrgdpch)+factor(year)+factor(country), data = Five_inst, index = c("country","year"), model = "within", effect = "individual")
summary(F_folswod_inst_2)


#T5.C3
#To make a observation data set used in column (5)
TwoSLS_3=plm(fhpolrigaug~lag(lrgdpch)+lag(fhpolrigaug)+factor(year)+factor(country)|.-lag(lrgdpch)+lag(nsave, 2)+factor(year)+factor(country), data=Five, index = c("code","year"),subset = sample =="1", model = "within", effect="individual") 
#Extract observations used in Two stage least squaure regression model. 
B = model.frame(TwoSLS_3)
#Rename the variables in data B
Five_inst_3 = rename(B, c("factor(year)" = "year",  "factor(country)" = "country"))

F_fols_inst_3 = plm(fhpolrigaug ~ lag(fhpolrigaug)+lag.lrgdpch.+factor(country)+factor(year), data = Five_inst_1, index = c("country","year"), model = "within", effect = "individual")
summary(F_fols_inst_3)





#T5.C4
# First stage regression
Firststage_4=plm(lag(lrgdpch)~lag(nsave, 2)+factor(year)+factor(country), data = Five, subset = sample == "1", na.action = na.omit, index = c("code","year"), model = "within", effect="individual")
summary(Firststage_4)

# 2SLS regression by hand
#fit_lrgdpch=predict(Firststage)
#length(fit_lrgdpch)
#Five$fit_lrgdpch=predict(Firststage)

TwoSLS_4=plm(fhpolrigaug~lag(lrgdpch)+factor(year)+factor(country)|.-lag(lrgdpch)+lag(nsave, 2)+factor(year)+factor(country), data=Five, index = c("code","year"),subset = sample =="1", model = "within", effect="individual")  # Second Stage: Regress outcome on x and fitted values from the first stage
summary(TwoSLS_4)



#T5. C5
TwoSLS_5=plm(fhpolrigaug~lag(fhpolrigaug)+lag(lrgdpch)+factor(year)+factor(country)|.-lag(lrgdpch)+lag(fhpolrigaug)+lag(nsave, 2)+factor(year)+factor(country), data=Five, index = c("code","year"),subset = sample =="1", model = "within", effect="individual")  # Second Stage: Regress outcome on x and fitted values from the first stage
summary(TwoSLS_5)

Firststage_5=plm(lag(lrgdpch)~lag(fhpolrigaug)+lag(nsave, 2)+factor(year)+factor(country), data = Five, subset = sample == "1", na.action = na.omit, index = c("code","year"), model = "within", effect="individual")
summary(Firststage_5)


#T5. C7
TwoSLS_7=plm(fhpolrigaug~lag(lrgdpch)+lag(laborshare)+factor(year)+factor(country)|.-lag(lrgdpch)+lag(laborshare)+lag(nsave, 2)+factor(year)+factor(country), data=Five, index = c("code","year"),subset = sample =="1", model = "within", effect="individual")  # Second Stage: Regress outcome on x and fitted values from the first stage
summary(TwoSLS_7)




#T5. C8
TwoSLS_8=plm(fhpolrigaug~lag(fhpolrigaug, 1:3)+lag(lrgdpch)+factor(year)+factor(country)|.-lag(lrgdpch)+lag(fhpolrigaug)+lag(nsave, 2)+factor(year)+factor(country), data=Five, index = c("code","year"),subset = sample =="1", model = "within", effect="individual")  # Second Stage: Regress outcome on x and fitted values from the first stage
summary(TwoSLS_8)
(R <- matrix(c(1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0),byrow=TRUE,nrow=3))
(r <- c(0,0,0))
linearHypothesis(TwoSLS_8, hypothesis.matrix=R, rhs = r)
(R <- matrix(c(1,0,0,0,0,0,0,0,0,0,0), byrow=TRUE, nrow = 1))
(r = c(0))
linearHypothesis(TwoSLS_8, hypothesis.matrix=R, rhs = r)


#T5. C9
TwoSLS_9=plm(fhpolrigaug~lag(lrgdpch)+factor(year)+factor(country)|.-lag(lrgdpch)+lag(nsave, 2:3)+factor(year)+factor(country), data=Five, index = c("code","year"),subset = sample =="1", model = "within", effect="individual")  # Second Stage: Regress outcome on x and fitted values from the first stage
summary(TwoSLS_9)


stargazer(F_pols_inst_1, F_folswod_inst_2, F_fols_inst_3, TwoSLS_4, TwoSLS_5, TwoSLS_7, TwoSLS_8, TwoSLS_9, type="html", style = "aer",
          se=list(clse(F_pols_inst_1),clse(F_folswod_inst_2),clse(F_fols_inst_3), clse(TwoSLS_4), clse(TwoSLS_5), clse(TwoSLS_7), clse(TwoSLS_8), clse(TwoSLS_9)),
          column.labels = c("Balanced panel, 1970-2000", "Base sample, 1960-2000, without former socialist countries", "Base sample, 1960-2000"), column.separate = c(1,1,2),
          no.space = TRUE, dep.var.labels="Dependent variable is democracy using five-year data and fixed effects OLS", omit = c("factor", "Constant", "age"), omit.stat = c("f"),
          covariate.labels=c("Democracy_{t-1}", "Log GDP per capita_{t-1}"),
          title="Table 5-Fixed Effects Results Using Freedom House Measure of Democracy: Two-Stage Least Squares with Savings Rate Instrument", align=TRUE, out = "Table 5.htm")



