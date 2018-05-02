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