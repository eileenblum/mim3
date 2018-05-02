#MI-M3 formant means

library(tidyverse)
library(tidyr)
library(dplyr)

#load data
m3_fm <- read.csv("./data_tidy/M3fm.csv")
head(m3_fm)
summary(m3_fm)
str(m3_fm)

# convert wide to long
m3_fm_v2 <- m3_fm %>%
  select(file.2:F3.3) %>%
  gather(., key = file, value = file,)
  
