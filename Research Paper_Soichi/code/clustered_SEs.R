require("sandwich")
require("lmtest")
require("stargazer")



clse = function(reg) { 
  # index(reg, "id") returns the id or entity variable vector 
  G = length(unique(index(reg,"id")))
  N = length(index(reg,"id"))
  dfa = (G/(G - 1))*(N-1)/reg$df.residual   # note Bluhm multiplies this by finite-sample df adjustment
  rob = sqrt(diag(dfa*vcovHC(reg, type = "HC0", 
                             cluster = "group", adjust = T)))
  return(rob)
}

stargazer(F_pols, F_fols, F_folswod,se=list(clse(F_pols),clse(F_fols),clse(F_folswod)), title="Panel regression, clustered SEs", type="text", column.labels=c("Pooled OLS", "Fixed Effects OLS", "Fixed Effects OLS"), df=FALSE, digits=3)

