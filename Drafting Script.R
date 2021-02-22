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

RDestimate(bac1 ~ male, data = RDDdata)
RDestimate(bac1~male, data = RDDdata, subset = NULL, cutpoint = NULL, bw = 0.002,
           kernel = "triangular", se.type = "HC1", cluster = NULL,
           verbose = FALSE, model = FALSE, frame = FALSE)
