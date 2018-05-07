# Load packages
library(lme4)
library(tidyverse)
library(here)

# Load data
m3_data <- read_csv(here("data_tidy", "m3data_tidy.csv"))

# Isolate data for [a], [ə]
adata <- m3_data %>%
  filter(., vowel == "a")  %>%
  mutate(., dur_c = dur_norm - mean(dur_norm)) %>%
  mutate(., f1_c = F1 - mean(F1)) %>%
  mutate(., f2_c = F2 - mean(F2))

aadata <- m3_data %>%
  filter(., vowel == "a:")  %>%
  mutate(., dur_c = dur_norm - mean(dur_norm)) %>%
  mutate(., f1_c = F1 - mean(F1)) %>%
  mutate(., f2_c = F2 - mean(F2))

dput(adata)

ədata <- m3_data %>%
  filter(., vowel == "ə") %>% 
  mutate(., dur_c = dur_norm - mean(dur_norm)) %>%
  mutate(., f1_c = F1 - mean(F1)) %>%
  mutate(., f2_c = F2 - mean(F2))

# Plots
adur_plot <- adata %>% 
  ggplot(., aes(x=as.factor(syllable), y=dur_c, color=as.factor(stress))) +
  geom_jitter() +
  labs(title="[a] Durations", x="Syllable", y="Duration (ms)", color="Stress")

aadata %>% 
  ggplot(., aes(x=as.factor(syllable), y=dur_norm)) +
  geom_boxplot() +
  labs(title="[a:] Durations", x="Syllable", y="Duration (ms)")

aplot <- adata %>% 
  ggplot(., aes(x=as.factor(syllable), y=F1, color=as.factor(stress))) +
  geom_boxplot() +
  labs(title="[a] F1", x="Syllable", y="F1", color="Stress")

aadata %>% 
  ggplot(., aes(x=as.factor(syllable), y=F1)) +
  geom_boxplot() +
  labs(title="[a:] F1", x="Syllable", y="F1")

adata %>% 
  ggplot(., aes(x=as.factor(), y=f2_c, color=as.factor(stress))) +
  geom_jitter() +
  labs(title="Centered F2 means for [a]", x="", y="F2", color="Stress")

ədur_plot <- ədata %>%
  ggplot(., aes(x=as.factor(syllable), y=dur_norm, color=as.factor(stress))) +
  geom_jitter() +
  labs(title="[ə] Durations", x="Syllable", y="Duration (ms)", color="Stress")

əf1_plot <- ədata %>%
  ggplot(., aes(x=as.factor(syllable), y=F1, color=as.factor(stress))) +
  geom_boxplot() +
  labs(title="[ə] F1", x="Syllable", y="F1", color="Stress")

m3_data$vowel <- as.factor(vowel)

vform_plot <- m3_data %>%
  filter(., vowel %in% c("a", "ə", "i", "e", "u", "o")) %>% 
  ggplot(., aes(x = F2, y = F1, color = vowel, label = vowel, shape = vowel)) +
  labs(title="Short Vowels", x="F2", y="F1", color="Vowel") +
  geom_text() +
  stat_ellipse(aes(color = vowel)) +
  scale_y_reverse() + 
  scale_x_reverse() +
  scale_color_brewer(palette = "Set1") + 
  facet_grid(~stress) +
  guides(colour = guide_legend(override.aes = list(size = 3, shape = 15))) 

v:form_plot <- m3_data %>%
  filter(., vowel %in% c("u:", "a:", "i:", "e:", "o:")) %>%
  ggplot(., aes(x = F2, y = F1, color = vowel, label = vowel, shape = vowel)) +
  labs(title="Long Vowels", x="F2", y="F1", color="Vowel") +
  geom_text() +
  stat_ellipse(aes(color = vowel)) +
  scale_y_reverse() + 
  scale_x_reverse() +
  scale_color_brewer(palette = "Set1") + 
  facet_grid(~stress) +
  guides(colour = guide_legend(override.aes = list(size = 3, shape = 15)))

# Models

## [a] duration

a_dur_model_full <- 
  lmer(dur_norm ~ stress +  syllable + frame +
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
###  position affected duration (χ^2(1)=26.443, p=2.714e-07) increasing it by 44.707 ms +/-8.167 SE

#Duration was tested as a function of the categorical factors described stress (0 or 1),  position (1 or 2), and frame sentence (0 or 1) with a random effect of item (file name) Stress and  position were shown to have a significant effect on duration (p < 0.05), but frame sentence did not. Statistical significance was assessed using hierarchical partitioning of variance via nested model comparisons.

###Interactions
a_dur_model_int <- 
  lmer(dur_norm ~ stress:syllable +
         (1 | file), data = adata, REML = F)
summary(a_dur_model_int)
anova(a_dur_model_frame, a_dur_model_int, a_dur_model_null)

### Residuals
#Visual inspection of the residual plots for the full [a] vowel duration model revealed both normal distribution and homoskedasticity.
plot(fitted(a_dur_model_full), residuals(a_dur_model_full))
#homoskedastic
qqnorm(residuals(a_dur_model_full))
hist(residuals(a_dur_model_full))
#normal distribution


## [a] F1
a_f1_mod_full <- 
  lmer(F1 ~ stress + syllable + frame +
         (1 | file), data = adata, REML = F)
summary(a_f1_mod_full)

a_f1_mod_frame <- 
  lmer(F1 ~ stress + syllable +
         (1 | file), data = adata, REML = F)
summary(a_f1_mod_frame)

a_f1_mod_syll <- 
  lmer(F1 ~ stress +
         (1 | file), data = adata, REML = F)
summary(a_f1_mod_syll)

a_f1_mod_null <- 
  lmer(F1 ~ 1 +
         (1 | file), data = adata, REML = F)
summary(a_f1_mod_null)

a_f1_mod_stress <- 
  lmer(F1 ~ syllable +
         (1 | file), data = adata, REML = F)
summary(a_f1_mod_stress)
anova(a_f1_mod_stress, a_f1_mod_null)
#syllable position did not significantly affect F1 (χ^2(1)=0.2704, p=0.6031)
a_f1_anova <- anova(a_f1_mod_full, a_f1_mod_frame, a_f1_mod_syll,  a_f1_mod_null)
#stress significantly affected F1 (χ^2(1)=27.966, p=1.235e-07) increased by 73.06 +/-12.94

#F1 was tested as a function of the categorical factors described stress (0 or 1), syllable position (1 or 2), and frame sentence (0 or 1) with a random effect of item (file name). Only stress was shown to have a significant effect on F1 (p < 0.05), but syllable position and frame sentence did not. Statistical significance was assessed using hierarchical partitioning of variance via nested model comparisons.

#Interaction
a_f1_mod_int <- 
  lmer(F1 ~ stress:syllable +
         (1 | file), data = adata, REML = F)
summary(a_f1_mod_int)

anova(a_f1_mod_frame, a_f1_mod_int, a_f1_mod_null)

### Residuals
#Visual inspection of the residual plots for the full [a] vowel F1 model revealed both normal distribution and homoskedasticity.
plot(fitted(a_f1_mod_full), residuals(a_f1_mod_full))
#homoskedastic
qqnorm(residuals(a_f1_mod_full))
hist(residuals(a_f1_mod_full))
#normal distribution

##[a] F2
a_f2_mod_full <- 
  lmer(F2 ~ stress + syllable + frame +
         (1 | file), data = adata, REML = F)
summary(a_f2_mod_full)

a_f2_mod_frame <- 
  lmer(F2 ~ stress + syllable +
         (1 | file), data = adata, REML = F)
summary(a_f2_mod_frame)

a_f2_mod_syll <- 
  lmer(F2 ~ stress +
         (1 | file), data = adata, REML = F)
summary(a_f2_mod_syll)

a_f2_mod_null <- 
  lmer(F2 ~ 1 +
         (1 | file), data = adata, REML = F)
summary(a_f2_mod_null)

a_f2_mod_stress <- 
  lmer(F2 ~ syllable +
         (1 | file), data = adata, REML = F)
summary(a_f2_mod_stress)
anova(a_f2_mod_stress, a_f2_mod_null)
#syllable position did not significantly affect F2 (χ^2(1)=2.89, p=0.089)
a_f2_anova <- anova(a_f2_mod_full, a_f2_mod_frame, a_f2_mod_syll,  a_f2_mod_null)
#described stress did not significantly affect F2 (χ^2(1)=1.337, p=0.25)

#F2 was tested as a function of the categorical factors described stress (0 or 1), syllable position (1 or 2), and frame sentence (0 or 1) with a random effect of item (file name). None of these were shown to have a significant effect on F2 (p > 0.05). Statistical significance was assessed using hierarchical partitioning of variance via nested model comparisons.

#Interactions
a_f2_mod_int <- 
  lmer(F2 ~ stress:syllable +
         (1 | file), data = adata, REML = F)
summary(a_f2_mod_int)
anova(a_f2_mod_frame, a_f2_mod_int, a_f2_mod_null)

### Residuals
#Visual inspection of the residual plots for the full [a] vowel F1 model revealed both normal distribution and homoskedasticity.
plot(fitted(a_f2_mod_full), residuals(a_f2_mod_full))
#homoskedastic
qqnorm(residuals(a_f2_mod_full))
hist(residuals(a_f1_mod_full))
#normal distribution


# ə duration

ə_durmod_full <- 
  lmer(dur_norm ~ stress + syllable + frame +
         (1 | file), data = ədata, REML = F)
summary(ə_durmod_full)

ə_durmod_frame <- 
  lmer(dur_norm ~ stress + syllable +
         (1 | file), data = ədata, REML = F)
summary(ə_durmod_frame)

ə_durmod_syll <- 
  lmer(dur_norm ~ stress +
         (1 | file), data = ədata, REML = F)
summary(ə_durmod_syll)

ə_durmod_stress <- 
  lmer(dur_norm ~ syllable +
         (1 | file), data = ədata, REML = F)
summary(ə_durmod_stress)

ə_durmod_frame2 <- 
  lmer(dur_norm ~ frame +
         (1 | file), data = ədata, REML = F)
summary(ə_durmod_frame2)

ə_durmod_null <- 
  lmer(dur_norm ~ 1 +
         (1 | file), data = ədata, REML = F)
summary(ə_durmod_null)

anova(ə_durmod_syll,  ə_durmod_null)
#described stress affected duration (χ^2(1)=29.827, p=4.723e-08) lowering it by 32.226 ms +/- 5.629 SE
anova(ə_durmod_stress, ə_durmod_null)
#syllable position affected duration (χ^2(1)=48.692, p=2.996e-12) increasing it by 38.668 ms +/- 5.318 SE
anova(ə_durmod_frame2, ə_durmod_null)
#frame sentence affected duration (χ^2(1)=15.954, p=6.489e-05) increasing it by 11.850 ms +/- 2.887 SE

#Duration of [ə] vowels was tested as a function of the categorical factors described stress (0 or 1), syllable position (1 or 2), and frame sentence (0 or 1) with a random effect of item (file name). All of these were shown to have a significant effect on duration (p > 0.05). Statistical significance was assessed using hierarchical partitioning of variance via nested model comparisons.

#Interaction
ə_durmod_int <- 
  lmer(dur_norm ~ stress:syllable +
         (1 | file), data = ədata, REML = F)
summary(ə_durmod_int)
anova(ə_durmod_int, ə_durmod_frame, ə_durmod_null)

###Residuals
plot(fitted(ə_durmod_full), residuals(ə_durmod_full))
#homoskedastic
qqnorm(residuals(ə_durmod_full))
hist(residuals(ə_durmod_full))
#normal distribution

# ə F1

ə_f1mod_full <- 
  lmer(F1 ~ stress + syllable + frame +
         (1 | file), data = ədata, REML = F)
summary(ə_f1mod_full)

ə_f1mod_frame <- 
  lmer(F1 ~ stress + syllable +
         (1 | file), data = ədata, REML = F)
summary(ə_f1mod_frame)

ə_f1mod_syll <- 
  lmer(F1 ~ stress +
         (1 | file), data = ədata, REML = F)
summary(ə_f1mod_syll)

ə_f1mod_stress <- 
  lmer(F1 ~ syllable +
         (1 | file), data = ədata, REML = F)
summary(ə_f1mod_stress)

ə_f1mod_frame2 <- 
  lmer(F1 ~ frame +
         (1 | file), data = ədata, REML = F)
summary(ə_f1mod_frame2)

ə_f1mod_null <- 
  lmer(F1 ~ 1 +
         (1 | file), data = ədata, REML = F)
summary(ə_f1mod_null)

anova(ə_f1mod_syll,  ə_f1mod_null)
#described stress affected F1 (χ^2(1)=9.5367, p=0.002014) lowering it by 50.712 ms +/- 15.837 SE
anova(ə_f1mod_stress, ə_f1mod_null)
#syllable position affected F1 (χ^2(1)=6.4637, p=0.01101) increasing it by 40.34 ms +/- 15.50 SE
anova(ə_f1mod_frame2, ə_f1mod_null)
#frame sentence affected F1 (χ^2(1)=40.431, p=2.036e-10) decreasing it by 62.251 ms +/- 9.216 SE

#F1 of [ə] vowels was tested as a function of the categorical factors described stress (0 or 1), syllable position (1 or 2), and frame sentence (0 or 1) with a random effect of item (file name). All of these were shown to have a significant effect on duration (p > 0.05). Statistical significance was assessed using hierarchical partitioning of variance via nested model comparisons.

#Interaction
ə_f1mod_int <- 
  lmer(F1 ~ stress:syllable +
         (1 | file), data = ədata, REML = F)
summary(ə_f1mod_int)
anova(ə_f1mod_int, ə_f1mod_frame, ə_f1mod_null)

###Residuals
plot(fitted(ə_f1mod_full), residuals(ə_f1mod_full))
#homoskedastic
qqnorm(residuals(ə_f1mod_full))
hist(residuals(ə_f1mod_full))
#normal distribution

# ə F2

ə_f2mod_full <- 
  lmer(F2 ~ stress + syllable + frame +
         (1 | file), data = ədata, REML = F)
summary(ə_f2mod_full)

ə_f2mod_frame <- 
  lmer(F2 ~ stress + syllable +
         (1 | file), data = ədata, REML = F)
summary(ə_f2mod_frame)

ə_f2mod_syll <- 
  lmer(F2 ~ stress +
         (1 | file), data = ədata, REML = F)
summary(ə_f2mod_syll)

ə_f2mod_stress <- 
  lmer(F2 ~ syllable +
         (1 | file), data = ədata, REML = F)
summary(ə_f2mod_stress)

ə_f2mod_frame2 <- 
  lmer(F2 ~ frame +
         (1 | file), data = ədata, REML = F)
summary(ə_f2mod_frame2)

ə_f2mod_null <- 
  lmer(F2 ~ 1 +
         (1 | file), data = ədata, REML = F)
summary(ə_f2mod_null)

anova(ə_f2mod_syll,  ə_f2mod_null)
#described stress did not significantly affect F2 (χ^2(1)=0.4965, p=0.481) increasing it by 23.23 +/-31.79 SE
anova(ə_f2mod_stress, ə_f2mod_null)
#syllable position did not significantly affect F2 (χ^2(1)=0.0069, p=0.9338) increasing it by 2.574 ms +/- 30.956 SE
anova(ə_f2mod_frame2, ə_f2mod_null)
#frame sentence affected F2 (χ^2(1)=22.432, p=2.177e-06) decreasing it by 97.43 ms +/- 19.88 SE

#F2 of [ə] vowels was tested as a function of the categorical factors described stress (0 or 1), syllable position (1 or 2), and frame sentence (0 or 1) with a random effect of item (file name). Only frame sentence was shown to have a significant effect on duration (p > 0.05).The effects of both syllable and stress were found not to be significant. Statistical significance was assessed using hierarchical partitioning of variance via nested model comparisons.

#Interaction
ə_f2mod_int <- 
  lmer(F2 ~ stress:syllable +
         (1 | file), data = ədata, REML = F)
summary(ə_f2mod_int)
anova(ə_f2mod_int, ə_f2mod_frame, ə_f2mod_null)

###Residuals
plot(fitted(ə_f2mod_full), residuals(ə_f2mod_full))
#homoskedastic
qqnorm(residuals(ə_f2mod_full))
hist(residuals(ə_f2mod_full))
#normal distribution
