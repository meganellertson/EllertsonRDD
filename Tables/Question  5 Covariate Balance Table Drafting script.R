## Question 5: Covariate Balance

put under the pdf line for print out if this doesnt work
pdf_document: default

rdplotdensity(density, RDDdata$bac1)
##  RDestimate option, issue is there is no SE 
malecov <- RDestimate(formula = male ~ bac1 | aged + acc + white + dui + bac1*dui, data = RDDdata, cutpoint = 0.08, bw = 0.05, kernel = "rectangular", se.type = "HC1")
cov1 <- lm_robust(male ~ bac1 + aged + acc + white + dui + dui*bac1, data = RDDdata, se_type = "HC1")
cov1

agecov <- RDestimate(formula = aged ~ bac1 | male + acc + white + dui + bac1*dui, data = RDDdata, cutpoint = 0.08, bw = 0.05, kernel = "rectangular", se.type = "HC1")


acccov <- RDestimate(formula = acc ~ bac1 | aged + male + white + dui + bac1*dui, data = RDDdata, cutpoint = 0.08, bw = 0.05, kernel = "rectangular", se.type = "HC1")

whitecov <- RDestimate(formula = white ~ bac1 | aged + male + acc + dui + bac1*dui, data = RDDdata, cutpoint = 0.08, bw = 0.05, kernel = "rectangular", se.type = "HC1")

CovBal = data.frame(
  Variables = c("Male", "Age", "Accident", "White"),
  M_1 = c(malecov$est[1], agecov$est[1], 
          acccov$est[1], whitecov$est[1]),
  S_1 = c(malecov$se[1], agecov$se[1], acccov$se[1],
          whitecov$se[1])
)
kable(
  CovBal,
  col.names = c("Variables", "Estimate", "SE"),
  digits = 3,
  caption = "Table 2: Covariate Balance"
)

stargazer(malecov, agecov, acccov, whitecov,
  type = "html",
  out = "EllertsonRDD/Figures/CovBalanceTable.htm"
)
## The following pulls in the correct SE however I can not get them to line up in a table.        
malecov$se[1]
agecov$se[1]
acccov$se[1]
whitecov$se[1]

## The summary of these objects will provide the standard error but we do not know how to plug into a table
data.frame(Male = (summary(malecov))$est[1])
data.frame(Male = summalecov$est[1])
data.frame(unclass(summary(malecov)))


# ALTERNATIVE seems like the variables are not in the right spot. 

bw <- with(RD_data, IKbandwidth(bac1, recidivsim, cutpoint = 0.08))
rdd_simple <- RDestimate(recidivism ~ bac1, data = RDDdata, cutpoint = 0.08, bw = bw)
summary(rdd_simple)

RD_est <- function(mod, covariates) {
  RD_fit <- RDestimate(as.formula(paste(mod, covariates)), 
                       data = RDDdata, cutpoint = 0.08)
  with(RD_fit, c(est = est[[1]], se = se[1], p = p[1]))
}

covariates <- list("No covariates" = "",
                   "male" = "| male",
                   "aged" = "| aged",
                   "acc" = "| acc",
                   "white" = "| white")

library(plyr)
ldply(covariates, RD_est, mod = "recidivism ~ bac1", .id = "Specification")

## Best Option 
malecov <- RDestimate(formula = male ~ bac1 | aged + acc + white + dui + bac1*dui, data = RDDdata, cutpoint = 0.08, bw = 0.05, kernel = "rectangular", se.type = "HC1")


agecov <- RDestimate(formula = aged ~ bac1 | male + acc + white + dui + bac1*dui, data = RDDdata, cutpoint = 0.08, bw = 0.05, kernel = "rectangular", se.type = "HC1")


acccov <- RDestimate(formula = acc ~ bac1 | aged + male + white + dui + bac1*dui, data = RDDdata, cutpoint = 0.08, bw = 0.05, kernel = "rectangular")

whitecov <- RDestimate(formula = white ~ bac1 | aged + male + acc + dui + bac1*dui, data = RDDdata, cutpoint = 0.08, bw = 0.05, kernel = "rectangular")

data.frame(male = malecov$est[1], age = agecov$est[1], accident = acccov$est[1], white = whitecov$est[1]) +
  
  
  
  
  t2 <- vector(mode = "list", length = 4)
t2[[1]] <- malecov
t2[[2]] <- whitecov
t2[[3]] <- agecov
t2[[4]] <- acccov
stargazer(t2, type = "text")

stargazer(malecov, whitecov, agecov, acccov, title = "Covariate Balance", align=TRUE)
