## Question 6 Covariate Graphs

## set up for each graph 
categories <- lmb_data$lagdemvoteshare
demmeans <- split(lmb_data$score, cut(lmb_data$lagdemvoteshare, 100)) %>% 
  lapply(mean) %>% 
  unlist()
agg_lmb_data <- data.frame(score = demmeans, lagdemvoteshare = seq(0.01,1, by = 0.01))
#plotting
lmb_data <- lmb_data %>% 
  mutate(gg_group = case_when(lagdemvoteshare > 0.5 ~ 1, TRUE ~ 0))
ggplot(lmb_data, aes(lagdemvoteshare, score)) +
  geom_point(aes(x = lagdemvoteshare, y = score), data = agg_lmb_data) +
  stat_smooth(aes(lagdemvoteshare, score, group = gg_group), method = "lm", 
              formula = y ~ x + I(x^2)) +
  xlim(0,1) + ylim(0,100) +
  geom_vline(xintercept = 0.5)
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
## exactly 
categries <- RDDdata$bac1
demmeans <- split(RDDdata$aged, cut(RDDdata$bac1, 50)) %>%
  lapply(mean) %>%
  unlist()

agg_RDDdata_data <- data.frame(aged = demmeans, bac1 = seq(0.002, 1, by = 0.002))

RDDdata_data <- RDDdata %>%
  mutate(gg_group = case_when(bac1> 0.08 ~ 0.15, TRUE ~ 0))

ggplot(RDDdata_data, aes(bac1, aged)) +
  geom_point(aes(x = bac1, y = aged), data = agg_RDDdata_data) +
  stat_smooth(aes(bac1, aged, group = gg_group), method = "lm",
              formula = y ~ x) +
  xlim(0,0.2) + ylim(0,80) +
  geom_vline(xintercept = 0.08) 


## male 
categories <- RDDdata$bac1
malemeans <- split(RDDdata$male, cut(RDDdata$bac1, 100)) %>%
  lapply(mean) %>%
  unlist()
agg_malecov_data <- data.frame(male = malemeans, bac1 = seq(0.002, 1, by = 0.002))


maledata <- RDDdata %>%
  mutate(gg_group = case_when(bac1 >= 0.08 ~ 0.15, TRUE ~ 0))
ggplot(maledata, aes(bac1, male)) +
  geom_point(aes(x = bac1, y = male), alpha = 1, data = agg_malecov_data) +
stat_smooth(aes(bac1, male, group = gg_group), method = "lm", 
            formula = y ~ x + I(x^2)) +
  xlim(0,0.2)+ylim(0, 0.84) +
  geom_vline(xintercept = 0.08) +
  theme_minimal()
  
ggplot(maledata, aes(bac1, male)) +
  geom_point(aes(x = bac1, y = male), data = agg_malecov_data) +
  stat_smooth(aes(bac1, male, group = gg_group), method = "lm", 
              formula = y ~ x +
  xlim(0,0.2)+ylim(0,0.84) +
  geom_vline(xintercept = 0.08)
  