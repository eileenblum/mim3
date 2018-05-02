# Load packages

library(lme4)
library(tidyverse)
library(here)

# Load data

m3_data <- read_csv(here("data_tidy", "m3data_tidy.csv"))

# Isolate data for [a], [ə]

adata <- m3_data %>%
  filter(., vowel == "a")  %>%
  mutate(., dur_c = dur_norm - mean(dur_norm)) 

ədata <- m3_data %>%
  filter(., vowel == "ə") %>% 
  mutate(., dur_c = dur_norm - mean(dur_norm)) 

# Models

## [a] duration

a_dur_model_full <- 
  lmer(dur_norm ~ stress + syllable + frame +
         (1 | file), data = adata, REML = F)
summary(a_dur_model_full)

a_dur_model_frame <- 
  lmer(dur_norm ~ stress + syllable +
         (1 | file), data = adata, REML = F)
summary(a_dur_model_frame)

a_dur_model_syll <- 
  lmer(dur_norm ~ stress +
         (1 | file), data = adata, REML = F)
summary(a_dur_model_syll)

a_dur_model_null <- 
  lmer(dur_norm ~ 1 +
         (1 | file), data = adata, REML = F)
summary(a_dur_model_null)

a_dur_anova <- anova(a_dur_model_full, a_dur_model_frame, a_dur_model_syll,  a_dur_model_null)
summary(a_dur_anova)
### described stress affected duration (χ^2(1)=0.97, p=3.255 e-01) lowering it by 6.796 ms +/-6.9 SE 

a_dur_model_stress <- 
  lmer(dur_norm ~ syllable +
         (1 | file), data = adata, REML = F)
summary(a_dur_model_stress)
anova(a_dur_model_stress, a_dur_model_null)
### syllable position affected duration (χ^2(1)=26.443, p=2.714e-07) increasing it by 44.707 ms +/-8.167 SE

#Duration was tested as a function of the continuous variables F1 and F2, the categorical factors described stress (0 or 1), syllable position (1 or 2), and frame sentence (0 or 1). These were all shown to have a significant effect on duration (p < 0.05). Statistical significance was assessed using hierarchical partitioning of variance via nested model comparisons.

### Residuals
#Visual inspection of the residual plots for the full [a] vowel duration model revealed both normal distribution and homoskedasticity.
adur_resid <- plot(fitted(a_dur_model_full), residuals(a_dur_model_full))
#homoskedastic
adur_resid2 <- qqnorm(residuals(a_dur_model_full))
s
adur_resid4 <- hist(residuals(a_dur_model_full))
#normal distribution


## [a] F1

