McCrary Density 

libary(rddensity)
rdbwdensity(
  bac1,
  c = 0.8 & 0.15,
  p = 2,
  fitselect = "",
  kernel = "",
  vce = "",
  masPoint = TRUE,
  regularize = TRUE,
  nLocalMin = NULL,
  nUniqueMin = NULL
  
)


formula <- lm(recidivism ~ bac1 + male + white + aged + acc + cutoff + bac1*cutoff, data = RDDdata)
RDestimate(formula, data = RDDdata, cutpoint = 0.08,)


library(readr)
library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(estimatr)
library(rdrobust)
library(rddensity)

library("tinytex", lib.loc="~/R/win-library/3.6")

rmarkdown::render('RDDReplication1Ellertson.Rmd', output_format = 'pdf_document')

include in the library chunk 
library("tinytex", lib.loc="~/R/win-library/3.6")

rmarkdown::render('RDDReplication1Ellertson.Rmd', output_format = 'pdf_document')


rdr <- rdrobust(y = RDDdata$recidivism,
                x = RDDdata$bac1, c = 0.08)
summary(rdr)


cli::cli_text("Table 2: Regression Discontinuity Estimates for the Effect of Exceeding BAC Thresholds on Predetermined Characteristics")
texreg::screenreg(list(reg1, reg2, reg3, reg4), type="text")


#aggregating the data

categories <- RDDdata$bac1
demeans <- split(RDDdata$recidivism, cut(RDDdata$bac1, 100)) %>%
  lapply(mean) %>%
  unlist()
agg_RDDdata <- data.frame(recidivism = demmeans, bac1 = seq(0.01, 0.5, by = 0.01))
#plotting
plottingdata <- RDDdata %>%
  mutate(gg_group = case_when(bac1>= 0.08 ~ 1, TRUE ~ 0))

ggplot(plottingdata, aes(bac1, recidivism)) +
  geom_point(aes(x=bac1, y = recidivism), data = agg_RDDdata) +
  stat_smooth(aes(bac1, recidivism, group = gg_group), method = "lm",
              formula = y ~ x + I(x^2)) +
  xlim(0,0.2) + ylim(0,1) +
  geom_vline(xintercept = 0.08)


#Possible option for cleaning the variables 
agecontrolled <- lm_robust(aged ~ cutoff + bac1 + bac1*cutoff, data = RDDdata)
summary(agecontrolled)

whitecontrolled <- lm_robust(white ~ cutoff + bac1 +bac1*cutoff, data = RDDdata)
summary(whitecontrolled)

acccontrolled <- lm_robust(acc ~ cutoff + bac1 + bac1&cutoff, data = RDDdata)
summary(acccontrolled)

malecontrolled <- lm_robust(male ~ cutoff + bac1 +bac1*cutoff, data = RDDdata)
summary(malecontrolled)

agefitted = predict(agecontrolled, RDDdata)
plot(agefitted ~ bac1, data = RDDdata, ylim=range(0.3, 0.38))

whitefitted = predict(whitecontrolled, RDDdata)
plot(whitefitted ~ bac1, data = RDDdata, ylim = range(0.8, 0.9))

accfitted = predict(acccontrolled, RDDdata)
plot(accfitted ~ bac1, data = RDDdata, ylim = range(0.05, 0.25))

malefitted = predict(malecontrolled, RDDdata)
plot(malefitted ~ bac1, data = RDDdata, ylim = range(0.74, 0.82))

Table3C1 = lm(recidivism ~ cutoff + bac1 + male + aged + white + acc + bac1*cutoff, data = RDDdata)
summary(Table3C1)

# OPTION FOR 5 AND 7 
RDestimate(bac1 ~ male, data = RDDdata)
RDestimate(bac1 ~ male|
             
             
             
RDestimate(bac1 ~ male | aged + acc + white + cutoff + bac1*cutoff, cutpoint = 0.08, bw = 0.05, kernel = "rectangular" )



RDDdata5 <- RDDdata %>%
  filter(bac1 >= 0.08)
reg1 <- lm_robust(recidivism ~ male, data = RDDdata5)
reg2 <- lm_robust(recidivism ~ aged, data = RDDdata5)
reg3 <- lm_robust(recidivism ~ acc, data = RDDdata5)
reg4 <- lm_robust(recidivism ~ white, data = RDDdata5)
## this option is probably not right 

malechar = lm_robust(cutoff ~ bac1 + male, data = RDDdata)
summary(malechar)
agechar = lm_robust(cutoff ~ bac1 + aged, data = RDDdata)
summary(agechar) 
accchar = lm_robust(cutoff ~ bac1 + acc, data = RDDdata)
summary(accchar)
whitechar = lm_robust(cutoff ~ bac1+ white, data = RDDdata)
summary(whitechar)

or 

#additional option 
option = lm_robust(recidivism ~ white + aged + acc + male + cutoff + bac1 + bac1*cutoff, data = RDDdata)
summary(option)

#another option still need to plug into items



stargazer(reg1, reg2, reg3, reg4, title="Table 2 - Regression Discontinuity Estimates for the Effect of Exceeding BAC Thresholds on Predetermined Characteristics", align = TRUE, dep.var.labels =c("male", "age", "acc", "white"), no.space=TRUE)

or

cli::cli_text("Table 2: Regression Discontinuity Estimates for the Effect of Exceeding BAC Thresholds on Predetermined Characteristics")
texreg::screenreg(list(reg1, reg2, reg3, reg4), type="text")

lm_1 <- lm_robust(recidivism ~ bac1, data = RDDdata_subset1)
lm_2 <- lm_robust(recidivism ~ bac1*cutoff, data = RDDdata_subset1)
RDDdata_subset1a <- RDDdata_subset1 %>%
  mutate(bac1sq = bac1^2)
lm_3 <- lm_robust(recidivism ~ bac1*cutoff + bac1sq*cutoff, data = RDDdata_subset1a)

summary(lm_1)
summary(lm_2)
summary(lm_3)

lm_4 <- lm_robust(recidivism ~ bac1, data = RDDdata_subset2)
lm_5 <- lm_robust(recidivism ~ bac1 + cutoff + bac1*cutoff, data = RDDdata_subset2)
RDDdata_subset2a <- RDDdata_subset2 %>%
  mutate(bac1sq = bac1^2)
lm_6 <- lm_robust(recidivism ~ bac1*cutoff + bac1sq*cutoff, data = RDDdata_subset2a)

summary(lm_4)
summary(lm_5)
summary(lm_6)


cli::cli_text("Table 3")
texreg::screenreg(list(lm_1, lm_2, lm_3, lm_4, lm_5, lm_6), type="text")