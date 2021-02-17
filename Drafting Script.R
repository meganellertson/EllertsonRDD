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
categories <- lmb_data$lagdemvoteshare

categories <- RDDdata$bac1

demmeans <- split(lmb_data$score, cut(lmb_data$lagdemvoteshare, 100)) %>% 
  lapply(mean) %>% 
  unlist()

demeans <- split(RDDdata$recidivism, cut(RDDdata$bac1, 100)) %>%
  lapply(mean) %>%
  unlist()

agg_lmb_data <- data.frame(score = demmeans, lagdemvoteshare = seq(0.01,1, by = 0.01))

agg_RDDdata <- data.fram(recidivism = demmeans, bac1 = seq(0.01, 0.5, by = 0.01))

#plotting
lmb_data <- lmb_data %>% 
  mutate(gg_group = case_when(lagdemvoteshare > 0.5 ~ 1, TRUE ~ 0))

plottingdata <- RDDdata %>%
  mutate(gg_group = case_when(bac1>= 0.08 ~ 1, TRUE ~ 0))


ggplot(lmb_data, aes(lagdemvoteshare, score)) +
  geom_point(aes(x = lagdemvoteshare, y = score), data = agg_lmb_data) +
  stat_smooth(aes(lagdemvoteshare, score, group = gg_group), method = "lm", 
              formula = y ~ x + I(x^2)) +
  xlim(0,1) + ylim(0,100) +
  geom_vline(xintercept = 0.5)

ggplot(plottingdata, aes(bac1, recidivism)) +
  geom_point(aes(x=bac1, y = recidivism), data = agg_RDDdata) +
  stat_smooth(aes(bac1, recidivism, group = gg_group), method = "lm",
              formula = y ~ x + I(x^2)) +
  xlim(0,0.2) + yline(0,1) +
  geom_vline(xintercept = 0.08)

ggplot(lmb_data, aes(lagdemvoteshare, score)) +
  geom_point(aes(x = lagdemvoteshare, y = score), data = agg_lmb_data) +
  stat_smooth(aes(lagdemvoteshare, score, group = gg_group), method = "loess") +
  xlim(0,1) + ylim(0,100) +
  geom_vline(xintercept = 0.5)

ggplot(lmb_data, aes(lagdemvoteshare, score)) +
  geom_point(aes(x = lagdemvoteshare, y = score), data = agg_lmb_data) +
  stat_smooth(aes(lagdemvoteshare, score, group = gg_group), method = "lm") +
  xlim(0,1) + ylim(0,100) +
  geom_vline(xintercept = 0.5)


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

malefilled = ppredict(malecontrolled, RDDdata)
plot(malefitted ~ bac1, data = RDDdata, ylim = range(0.74, 0.82))