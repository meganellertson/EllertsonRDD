## Question 7 Recreate Table 3

## LM formula 

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