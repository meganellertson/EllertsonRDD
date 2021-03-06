## Question 7 Recreate Table 3

RDDdata <- raw_hansen_dwi %>%
  mutate(dui = car::Recode(bac1, "lo: 0.08; else = 1")) %>%
  mutate(lininteract = dui*bac1) %>%
  mutate(bac1sq = bac1 ^ 2) %>%
  mutate(quadinteract = dui*bac1sq)

## Recentering and weights 
RDD7 <- RDDdata %>%
  mutate(bac7 = bac1 - 0.08) %>%
  mutate(dui7 = ifelse(bac7 >= 0, 1, 0)) %>%
  mutate(lininteract7 = dui7*bac7) %>%
  mutate(bacsq7 = bac7 ^ 2) %>%
  mutate(quadinteract7 = dui7*bacsq7)

##
weights <- rdd:kernelwts(RDD7$bac7, center = 0, bw = 0.05, kernel = "rectangular")
model <- lm(recidivism ~ bac7*dui7 + aged + white + male + acc, 
            data = RDD7,
            weights = weights)
lmtest::coeftest(model, vcov = sandwich::vcovHC(model)) ["dui"]

## LM formula will need to readjust the variable labels. 
RDDdata_subset1 <- RDDdata7 %>% 
  filter(bac1>0.03 & bac1 < 0.13)
lm_1 <- lm_robust(recidivism ~ bac1, data = RDDdata_subset1)
lm_2 <- lm_robust(recidivism ~ lininteract, data = RDDdata_subset1)
lm_3 <- lm_robust(recidivism ~ lininteract + quadinteract, data = RDDdata_subset1)

summary(lm_1)
summary(lm_2)
summary(lm_3)

RDDdata_subset2 <- RDDdata %>% 
  filter(bac1>0.055 & bac1 < 0.105)
lm_4 <- lm_robust(recidivism ~ bac1, data = RDDdata_subset2)
lm_5 <- lm_robust(recidivism ~ lininteract, data = RDDdata_subset2)
lm_6 <- lm_robust(recidivism ~ lininteract + quadinteract, data = RDDdata_subset2)

summary(lm_4)
summary(lm_5)
summary(lm_6)


cli::cli_text("Table 3")
texreg::screenreg(list(lm_1, lm_2, lm_3, lm_4, lm_5, lm_6), type="text")

## RDestimate formula 
## the formula is incorrect and confused. 
RDDdata_subset1 <- RDDdata %>% 
  filter(bac1>0.03 & bac1 < 0.13)

C1PA <- RDestimate(formula = recidivism ~ bac1 | dui + bac1*dui , data = RDDdata_subset1, cutpoint = 0.08, bw = 0.025, kernel = "rectangular")

C2PA <- RDestimate(formula = recidivism ~ bac1 | dui + bac1*dui, data = RDDdata_subset1, cutpoint = 0.08, bw = 0.025, kernel = "rectangular")

C3PA <- RDestimate(formula = recidivism ~ bac1 | bac1*dui + (bac1^2)*dui, data = RDDdata_subset1, cutpoint = 0.08, bw = 0.025, kernel = "rectangular")

## Panel B 
RDDdata_subset2 <- RDDdata %>% 
  filter(bac1>0.055 & bac1 < 0.105)

C1PB <- RDestimate(formula = recidivism ~ bac1 | aged + male + acc + dui + bac1*dui, data = RDDdata_subset2, cutpoint = 0.08, bw = 0.025, kernel = "rectangular")

C2PB <- RDestimate(formula = recidivism ~ bac1*dui | aged + male + acc + dui + bac1*dui, data = RDDdata_subset2, cutpoint = 0.08, bw = 0.025, kernel = "rectangular")

C3PB <- RDestimate(formula = recidivism ~ (bac1^2)*dui | aged + male + acc + dui + bac1*dui + (bac1^2)*dui, data = RDDdata_subset2, cutpoint = 0.08, bw = 0.025, kernel = "rectangular")

## Table format incomplete 
cli::cli_text("Table 3 RDestimate")
texreg::screenreg(list(), type="text")


##Question 8
categories <- RDDdata$bac1 
demmeans <- split(RDDdata$recidivism, cut(RDDdata$bac1, 100)) %>%
  lapply(mean) %>%
  unlist()
agg_RDDdata <- data.frame(recidivism = demmeans, bac1 = seq(0.01, 0.5, by = 0.01))
plottingdata <- RDDdata %>%
  mutate(gg_group = case_when(bac1>= 0.08 ~ 1, TRUE ~ 0))

ggplot(plottingdata, aes(bac1, recidivism)) +
  geom_point(aes(x=bac1, y = recidivism), data = agg_RDDdata) + xlim(0, 0.2) + ylim(0.08, 0.16) +
  stat_smooth(aes(bac1, recidivism, group = gg_group), method = "lm", formula = y ~ x + I(x^2)) +
  xlim(0,0.2) + ylim(0,1) +
  geom_vline(xintercept = 0.08)

ggplot(plottingdata, aes(bac1, recidivism)) +
  geom_point(aes(x=bac1, y = recidivism), data = agg_RDDdata) +
  stat_smooth(aes(bac1, recidivism, group = gg_group), method = "lm") +
  geom_vline(xintercept = 0.08)

##
library(rdd)

RDD7a <- RDDdata %>%
  mutate(bac7 = bac1 - 0.08,
         dui7 = ifelse(bac7 >= 0, 1, 0),
         bacsq7 = bac7^2) %>%
  filter(bac7>=-0.05 & bac7<=0.05)
## CHANGE FILTER
RDD7b <-RDDdata %>%
  mutate(bac7 = bac1 - 0.08,
         dui7 = ifelse(bac7 >= 0, 1, 0),
         bacsq7 = bac7^2) %>%
  filter(bac7>=-0.025 & bac7<=0.025)
## CHANGE FILTER

weightsa <- rdd::kernelwts(RDD7a$bac7, center = 0, bw = 0.05, kernel = "rectangular")
weightsb <- rdd::kernelwts(RDD7b$bac7, center = 0, bw = 0.05, kernel = "rectangular")
C1PA <- lm(recidivism ~ bac7 + dui7 + aged + white + male + acc, 
           data = RDD7a,
           weights = weightsa)
lmtest::coeftest(C1PA, vcov = sandwich::vcovHC(C1PA))
C2PA <- lm(recidivism ~ bac7*dui7 + aged + white + male + acc, 
           data = RDD7a,
           weights = weightsa)
lmtest::coeftest(C2PA, vcov = sandwich::vcovHC(C2PA))
C3PA <- lm(recidivism ~ dui7*(bac7 + bacsq7) + aged + white + male + acc, 
           data = RDD7a,
           weights = weightsa)
lmtest::coeftest(C3PA, vcov = sandwich::vcovHC(C3PA))

C1PB <- lm(recidivism ~ bac7 + dui7 + aged + white + male + acc, 
           data = RDD7b,
           weights = weightsb)
lmtest::coeftest(C1PB, vcov = sandwich::vcovHC(C1PB))
C2PB <- lm(recidivism ~ bac7*dui7 + aged + white + male + acc, 
           data = RDD7b,
           weights = weightsb)
lmtest::coeftest(C2PB, vcov = sandwich::vcovHC(C2PB))
C3PB <- lm(recidivism ~ dui7*(bac7 + bacsq7) + aged + white + male + acc, 
           data = RDD7b,
           weights = weightsb)
lmtest::coeftest(C3PB, vcov = sandwich::vcovHC(C3PB))
##Table formatting 
C1PA$coefficients[3]
C1PA$std.error[3]
C1PA$p.value[3]
C2PA$coefficients[3]
C2PA$std.error[3]
C2PA$p.value[3]
C3PA$coefficients[2]
C3PA$std.error[2]
C3PA$p.value[2]
C1PB$coefficients[3]
C1PB$std.error[3]
C1PB$p.value[3]
C2PB$coefficients[3]
C2PB$std.error[3]
C2PB$p.value[3]
C3PB$coefficients[2] 
C3PB$std.error[2] 
C3PA$p.value[2]


star.1 <- stargazer(linear.1, linear.2, probit.model,
                    title="Title: Regression Results",
                    align=TRUE,
                    type = "html",
                    style = "ajs", # "ajs"
                    notes="this is a test note"
)
kable(star.1)