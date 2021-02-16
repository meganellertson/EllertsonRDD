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