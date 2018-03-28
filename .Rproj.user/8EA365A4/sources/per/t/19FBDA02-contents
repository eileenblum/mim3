library(untidydata)
library(plot3D)
library(tidyverse)
#4 load dataset
my_fric_data <- read_csv("./data_raw/fricatives/fricatives.csv")
head(my_fric_data)
View(my_fric_data)

#5 tidy data
my_fric_long_cog <- my_fric_data %>%
  select(., alveolar = s_cog, postAlveolar = sh_cog) %>%
  gather(., key = phoneme, value = centerOfGravity)

my_fric_long_skew <- my_fric_data %>%
  select(., alveolar = s_skewness, postAlveolar = sh_skewness) %>%
  gather(., key = phoneme, value = skewness)

#6 
select(my_fric_data, -obs) %>%
  summary() %>% 
  knitr::kable(., digits = 2)

#7 make a boxplot of cog as a function of phoneme. in another slide plot skewness as a function of phoneme.
my_fric_long_cog %>% 
  ggplot(., aes(x = phoneme, y = centerOfGravity)) +
  geom_boxplot()

my_fric_long_skew %>% 
  ggplot(., aes(x = phoneme, y = skewness)) +
  geom_jitter()
  #[-ant] = positive skew, [+ant] = negative skew

#8 fit a model to cog as a function of skewness for [s]. make a table of model summary
fric_cog_skew <- my_fric_data %>% 
  select(., s_cog, s_skewness)
fric_cog_skew_mod <- lm(s_cog ~ s_skewness, data = fric_cog_skew)
summary(fric_cog_skew_mod)

#9 make a scatter plot
my_fric_data %>% 
  select(., s_cog, s_skewness) %>%
  ggplot(., aes(x = s_cog, y = s_skewness)) + 
  geom_point()

#10 check model diagnostics. make plots
mean(fric_cog_skew_mod$residuals)
acf(fric_cog_skew_mod$residuals)
plot(fric_cog_skew_mod$residuals)


