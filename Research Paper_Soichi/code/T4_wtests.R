#T4.C1
F_fols_b = plm(fhpolrigaug ~ lag(fhpolrigaug)+lag(lrgdpch)+factor(year)+factor(country), data = Five, subset = samplebalancefe == "1", index = c("code","year"), model = "within", effect = "individual")
summary(F_fols_b)

#Breusch-Godfrey test
pbgtest(F_fols_b)

#Breusch-Pagan test for homoskedastisity
plmtest(F_fols_b, type="bp")


#T4.C3
F_fols_c = plm(fhpolrigaug ~ lag(fhpolrigaug)+lag(lrgdpch)+factor(year)+factor(country), data = Five, subset= sample == "1" & socialist !="1", index = c("code","year"), model = "within", effect = "individual")
summary(F_fols_c)

#Breusch-Godfrey test
pbgtest(F_fols_c)

#Breusch-Pagan test for homoskedastisity
plmtest(F_fols_c, type="bp")


#T4.C5
#reg fhpolrigaug L.fhpolrigaug L.lrgdpch L.lpop L.medage L.age_veryyoung L.age_young L.age_midage L.age_old  yr* cd* if sample==1, cluster(code)

F_fols_add = plm(fhpolrigaug ~ lag(fhpolrigaug)+lag(lrgdpch)+ lag(lpop)+lag(medage)+ lag(age_veryyoung)+ lag(age_young)+lag(age_midage)+lag(age_old)+factor(year)+factor(country), data = Five, subset = sample == "1", index = c("code", "year"), model="within", effect="individual")
summary(F_fols_add)

#Breusch-Godfrey test
pbgtest(F_fols_add)

#Breusch-Pagan test for homoskedastisity
plmtest(F_fols_add, type="bp")


#T4.C7
#  reg fhpolrigaug L.fhpolrigaugL.education L.lrgdpch L.lpop L.medage L.age_veryyoung L.age_young L.age_midage L.age_old  yr* cd* if sample==1, cluster(code)
F_fols_we = plm(fhpolrigaug ~ lag(fhpolrigaug)+lag(lrgdpch)+ lag(lpop)+lag(education)+lag(medage)+ lag(age_veryyoung)+ lag(age_young)+lag(age_midage)+lag(age_old)+factor(year)+factor(country), data = Five, subset = sample == "1", index = c("code", "year"), model="within", effect="individual")
summary(F_fols_we)

#Breusch-Godfrey test
pbgtest(F_fols_we)

#Breusch-Pagan test for homoskedastisity
plmtest(F_fols_we, type="bp")



library(stargazer)
stargazer(F_fols_b, F_fols_c, F_fols_add, F_fols_we, type="html", style = "aer",
          se=list(clse(F_fols_b),clse(F_fols_c),clse(F_fols_add), clse(F_fols_we)),
          column.labels = c("Balanced panel, 1970-2000", "Base sample, 1960-2000, without former socialist countries", "Base sample, 1960-2000"), column.separate = c(1,1,2),
          no.space = TRUE, dep.var.labels="Dependent variable is democracy using five-year data and fixed effects OLS", omit = c("factor", "Constant", "age"), omit.stat = c("f"),
          covariate.labels=c("Democracy_{t-1}", "Log GDP per capita_{t-1}", "Log population_{t-1}", "Education_{t-1}"),
          title="Table 4-Fixed Effects Results Using Freedom House Measure of Democracy: Robustness Checks", align=TRUE, out = "Table 4.htm")

