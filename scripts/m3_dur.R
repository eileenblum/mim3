#MI-M3_dur_norm
library(untidydata)
library(plot3D)
library(tidyverse)

#load data
m3_dur_norm <- read.csv("../data/M3dur_norm.csv")
head(m3_dur_norm)
View(m3_dur_norm)

#plot duration in ms for various vowels under various stress conditions and positions
##bar graph
m3_dur_norm %>%
  ggplot(., aes(x = as.factor(syllable), y = dur_norm, color = as.factor(frame))) +
  geom_col() + 
  labs(x = "Syllable", y = "Duration(s)", color = "Frame", title = "Vowel durations")

m3_dur_norm %>%
  ggplot(., aes(x = as.factor(syllable), y = dur_norm, color = as.factor(stress))) +
  geom_col() + 
  facet_grid(. ~ vowel)
  labs(x = "Syllable", y = "Duration(ms)", color = "Stress", title = "Vowel durations")

##boxplot
m3_dur_norm %>%
  ggplot(., aes(x = as.factor(syllable), y = dur_norm, color = as.factor(stress))) +
  geom_boxplot() + 
  labs(x = "Syllable", y = "Duration(ms)", color = "Stress", title = "Vowel durations")

m3_dur_norm %>%
  ggplot(., aes(x = as.factor(syllable), y = dur_norm, color = as.factor(shape))) +
  geom_boxplot() + 
  labs(x = "Syllable", y = "Duration(s)", color = "Shape", title = "Vowel durations")

##per syllable
m3_dur_a <- m3_dur_norm %>%
  filter(., vowel %in% c('a', 'a:')) #filter both long and short [a]. code all [aː] as [a:]. not reading IPA length symbol, so just use colon
m3_dur_a %>%
  ggplot(., aes(x = as.factor(syllable), y = dur_norm, color = as.factor(stress))) +
  geom_boxplot() + 
  labs(x = "Syllable", y = "Duration(ms)", color = "Stress", title = "[a] and [a:] Durations") 

#no longer necessary
m3_dur_aː <- m3_dur_norm %>%
  filter(., vowel == 'aː')
m3_dur_aː %>%
  ggplot(., aes(x = as.factor(syllable), y = dur_norm)) +
  geom_boxplot() + 
  labs(x = "Syllable", y = "Duration(ms)", title = "Long [a] durations")

m3_dur_ə <- m3_dur_norm %>%
  filter(., vowel == 'ə')
m3_dur_ə %>%
  ggplot(., aes(x = as.factor(syllable), y = dur_norm, color = as.factor(stress))) +
  geom_boxplot() + 
  labs(x = "Syllable", y = "Duration(ms)", color = "Stress", title = "[ə] durations")

m3_dur_i <- m3_dur_norm %>%
  filter(., vowel %in% c('i', 'i:'))
m3_dur_i %>%
  ggplot(., aes(x = as.factor(syllable), y = dur_norm, color = as.factor(stress))) +
  geom_boxplot() + 
  labs(x = "Syllable", y = "Duration(ms)", color = "Stress", title = "[i] and [i:] Durations")

m3_dur_e <- m3_dur_norm %>%
  filter(., vowel %in% c('e', 'e:'))
m3_dur_e %>%
  ggplot(., aes(x = as.factor(syllable), y = dur_norm, color = as.factor(stress))) +
  geom_boxplot() + 
  labs(x = "Syllable", y = "Duration(ms)", color = "Stress", title = "[e] and [e:] Durations")

m3_dur_o <- m3_dur_norm %>%
  filter(., vowel %in% c('o', 'o:'))
m3_dur_o %>%
  ggplot(., aes(x = as.factor(syllable), y = dur_norm, color = as.factor(stress))) +
  geom_boxplot() + 
  labs(x = "Syllable", y = "Duration(ms)", color = "Stress", title = "[o] and [o:] Durations")

m3_dur_u <- m3_dur_norm %>%
  filter(., vowel %in% c('u', 'u:'))
m3_dur_u %>%
  ggplot(., aes(x = as.factor(syllable), y = dur_norm, color = as.factor(stress))) +
  geom_boxplot() + 
  labs(x = "Syllable", y = "Duration(ms)", color = "Stress", title = "[u] and [u:] Durations")