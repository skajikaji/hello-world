#T3. C1
Pooled_pols_p = plm(polity4 ~ lag(polity4)+lag(lrgdpch)+factor(year), data = Five, subset = sample == "1", index = c("code","year"), model = "pooling")
summary(Pooled_pols_p)

#T3. C2
FE_fols_p_2 = plm(polity4 ~ lag(polity4)+lag(lrgdpch)+factor(country)+factor(year), data = Five, subset = sample == "1",index = c("country","year"), model = "within", effect = "individual")
summary(FE_fols_p_2) 

#T3. C5
FE_folswod_p = plm(polity4 ~ lag(lrgdpch)+factor(year)+factor(country), data = Five, subset = sample =="1", index = c("code","year"), model = "within", effect = "individual")
summary(FE_folswod_p)


#T3. C6
FE_fols_p_6 =  plm(polity4 ~ lag(polity4, 1:5)+lag(lrgdpch, 1:5)+factor(country)+factor(year), data = One, subset = sample == "1",index = c("country","year"), model = "within", effect = "individual")
summary(FE_fols_p_6)


#T2. C7
FE_fols_p_7 = plm(polity4 ~ lag(polity4)+lag(lrgdpch)+factor(country)+factor(year), data = Ten, subset = sample == "1",index = c("country","year"), model = "within", effect = "individual")
summary(FE_fols_p_7)

#T2. C9
FE_fols_p_9 = plm(polity4 ~ lag(polity4)+lag(lrgdpch)+factor(country)+factor(year), data = Twenty, subset = sample == "1",index = c("country","year"), model = "within", effect = "individual")
summary(FE_fols_p_9)


library(stargazer)
stargazer(Pooled_pols_p, FE_fols_p_2, FE_folswod_p, FE_fols_p_7, FE_fols_p_9, type="html", style = "aer",
          se=list(clse(Pooled_pols_p),clse(FE_fols_p_2),clse(FE_folswod_p), clse(FE_fols_p_7),clse(FE_fols_p_9)),
          column.labels = c("Five-year data", "Ten-year data", "Twenty-year data"), column.separate = c(3,1,1),
          no.space = TRUE, dep.var.labels="Dependent variable is democracy", omit = c("factor", "Constant"), omit.stat = c("f"),
          covariate.labels=c("Democracy_{t-1}", "Log GDP per capita_{t-1}"),
          object.names = TRUE, 
          title="Table 3-Fixed Effects Results Using Polity Measure of Democracy", align=TRUE, out = "Table 3.htm")

