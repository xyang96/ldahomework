
## This code answers HW1 part 5 question 2

## load nlme
library(nlme)

## load files
camp <- read.csv("camp_primary.csv")

## create data with no missingness in posfev
campdat <- camp %>% filter(complete.cases(POSFEV))


## fit the random intercept + exponential model
fit_pt3_ri_exp <- lme(POSFEV~visitc + trt:visitc, data=campdat,
                      random=~1|id, correlation=corExp(form=~visitc|id), method="ML")

## calculate AIC
AIC(fit_pt3_ri_exp)

## Part 5 Question 2: calculate model-estimated within subject correlation at 12 month lag
tau2 <-  as.numeric(VarCorr(fit_pt3_ri_exp)[1,1])
sig2 <-  as.numeric(VarCorr(fit_pt3_ri_exp)[2,1])

rho  <- exp(-1/coef(fit_pt3_ri_exp$modelStruct, unconstrained = FALSE)[2])

(tau2 + (rho^(12))*sig2)/(tau2 + sig2)

## can also find tau2, sigma2, and rho (calculate from range) from the summary output
summary(fit_pt3_ri_exp)
