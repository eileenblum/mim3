##load libraries##
library(here)
source(here("scripts", "libs.R"))

##load tidy data##
data <- read_csv(here("data_clean", "data_clean.csv"))

#isolate the relevant data for [u] and [i] vowels

udata <- data %>%
  filter(., TextGridLabel %in% c("u2", "U2", "u5", "U5",
                                 "u20", "U20", "u50", "U50"))%>%
  filter(., Filename != "M1A74", Filename != "M1B25") %>%     #two files for which f0 data did not show up
  mutate(., f0_c = f0_midpoint - mean(f0_midpoint))

idata <- data %>%
  filter(., TextGridLabel %in% c("i2", "I2", "i5", "I5",
                                 "i20", "I20", "i50", "I50"))

##models##

#[u] Duration

u.dur.model <- 
  lmer(NormalizedDuration ~ Stress + Segment + Frame + Syllnum +
         (1+ Segment + Frame + Repetition | Word), data = udata, REML = F)

u.dur.model.null <- 
  lmer(NormalizedDuration ~ Segment + Frame + Syllnum +
         (1+ Segment + Frame + Repetition | Word), data = udata, REML = F)

#both models converge

u_dur_sum <- summary(u.dur.model)
#estimate: 11.62; error: 3.51; t = 6.482

u_dur_anov <- anova(u.dur.model.null, u.dur.model)
#chi^2 = 33.163, p = 8.475 e-09

#Residuals

udur_resid <- plot(fitted(u.dur.model), residuals(u.dur.model))
#homoskedastic

udur_resid2 <- qqnorm(residuals(u.dur.model))
udur_resid3 <- qqline(residuals(u.dur.model))
#normally distributed


#[i] Duration

i.dur.model <- 
  lmer(NormalizedDuration ~ Stress + Segment + Frame + Syllnum +
         (1+ Segment + Frame + Repetition | Word), data = idata, REML = F)

i.dur.model.null <- 
  lmer(NormalizedDuration ~ Segment + Frame + Syllnum +
         (1+ Segment + Frame + Repetition | Word), data = idata, REML = F)

#both models converge

i_dur_sum <- summary(i.dur.model)
#estimate: 12.85; error: 2.26; t = 5.682

i_dur_anov <- anova(i.dur.model.null, i.dur.model)
#chi^2 = 28.298, p = 1.04 e-07

#Residuals

idur_resid <- plot(fitted(i.dur.model), residuals(i.dur.model))
#homoskedastic

idur_resid2 <- qqnorm(residuals(i.dur.model))
idur_resid3 <- qqline(residuals(i.dur.model))
#normally distributed, but with a few outliers


#[u] Intensity
u.int.model <- 
  lmer(Intensity ~ Stress + Segment + Frame + Syllnum +
         (1+ Segment + Frame + Repetition | Word), data = udata, REML = F)

u.int.model.null <- 
  lmer(Intensity ~ Segment + Frame + Syllnum +
         (1+ Segment + Frame + Repetition | Word), data = udata, REML = F)

#both models converge

u_int_sum <- summary(u.int.model)
#estimate: 3.27; error: .37; t = 8.77

u_int_anov <- anova(u.int.model.null, u.int.model)
#chi^2 = 58.289, p = 2.263 e-14

#Residuals

uint_resid <- plot(fitted(u.int.model), residuals(u.int.model))
#homoskedastic

uint_resid2 <- qqnorm(residuals(u.int.model))
uint_resid3 <- qqline(residuals(u.int.model))
#normally distributed



#[i] Intensity
i.int.model <- 
  lmer(Intensity ~ Stress + Segment + Frame + Syllnum +
         (1+ Segment + Frame + Repetition | Word), data = idata, REML = F)

i.int.model.null <- 
  lmer(Intensity ~ Segment + Frame + Syllnum +
         (1+ Segment + Frame + Repetition | Word), data = idata, REML = F)

#both models converge

i_int_sum <- summary(i.int.model)
#estimate: 3.28; error: .42; t = 7.88

i_int_anov <- anova(i.int.model.null, i.int.model)
#chi^2 = 39.409, p = 3.437e-10

#Residuals

iint_resid <- plot(fitted(i.int.model), residuals(i.int.model))
#homoskedastic

iint_resid2 <- qqnorm(residuals(i.int.model))
iint_resid3 <- qqline(residuals(i.int.model))
#normally distributed, but with a few outliers


#[u] F1
u.F1.model <- 
  lmer(F1_midpoint ~ Stress + Segment + Frame + Syllnum +
         (1+ Segment + Frame + Repetition | Word), data = udata, REML = F)

u.F1.model.null <- 
  lmer(F1_midpoint ~ Segment + Frame + Syllnum +
         (1+ Segment + Frame + Repetition | Word), data = udata, REML = F)

#both models converge

u_F1_sum <- summary(u.F1.model)
#estimate: 12.34; error: 9.27; t = 1.330

u_F1_anov <- anova(u.F1.model.null, u.F1.model)
#chi^2 = 1.59, p = 0.2062


#Residuals

uF1_resid <- plot(fitted(u.F1.model), residuals(u.F1.model))
#looks like there is an influential data point

uF1_resid2 <- qqnorm(residuals(u.F1.model))
uF1_resid3 <- qqline(residuals(u.F1.model))



#[i] F1
i.F1.model <- 
  lmer(F1_midpoint ~ Stress + Segment + Frame + Syllnum +
         (1+ Segment + Frame + Repetition | Word), data = idata, REML = F)

i.F1.model.null <- 
  lmer(F1_midpoint ~ Segment + Frame + Syllnum +
         (1+ Segment + Frame + Repetition | Word), data = idata, REML = F)

#both models converge

i_F1_sum <- summary(i.F1.model)
#estimate: 11.19; error: 5.20; t = 2.15

i_F1_anov <- anova(i.F1.model.null, i.F1.model)
#chi^2 = 4.127, p = 0.0422

#Residuals

iF1_resid <- plot(fitted(i.F1.model), residuals(i.F1.model))
#homoskedastic

iF1_resid2 <- qqnorm(residuals(i.F1.model))
iF1_resid3 <- qqline(residuals(i.F1.model))
#normally distributed, with a single outlier at the bottom



#[u] F2
u.F2.model <- 
  lmer(F2_midpoint ~ Stress + Segment + Frame + Syllnum +
         (1+ Segment + Frame + Repetition | Word), data = udata, REML = F)

u.F2.model.null <- 
  lmer(F2_midpoint ~ Segment + Frame + Syllnum +
         (1+ Segment + Frame + Repetition | Word), data = udata, REML = F)

#both models converge

u_F2_sum <- summary(u.F2.model)
#estimate: -89.36; error: 49.79; t = -1.795

u_F2_anov <- anova(u.F2.model.null, u.F2.model)
#chi^2 = 2.7143, p = 0.0995

#Residuals

uF2_resid <- plot(fitted(u.F2.model), residuals(u.F2.model))
#looks like there's an influential data point

uF2_resid2 <- qqnorm(residuals(u.F2.model))
uF2_resid3 <- qqline(residuals(u.F2.model))



#[i] F2
i.F2.model <- 
  lmer(F2_midpoint ~ Stress + Segment + Frame + Syllnum +
         (1+ Segment + Frame + Repetition | Word), data = idata, REML = F)

i.F2.model.null <- 
  lmer(F2_midpoint ~ Segment + Frame + Syllnum +
         (1+ Segment + Frame + Repetition | Word), data = idata, REML = F)

#both models converge

i_F2_sum <- summary(i.F2.model)
#estimate: 28.56; error: 32.37; t = 0.882

i_F2_anov <- anova(i.F2.model.null, i.F2.model)
#chi^2 = 0.7222, p = 0.3954

#Residuals

plot(fitted(i.F2.model), residuals(i.F2.model))
#homoskedastic

qqnorm(residuals(i.F2.model))
qqline(residuals(i.F2.model))
#normally distributed, with a few ouliers



#[u] F0
u.F0.model <- 
  lmer(f0_midpoint ~ Stress + Segment + Frame + Syllnum +
         (1+ Segment + Frame + Repetition | Word), data = udata, REML = F)

u.F0.model.null <- 
  lmer(f0_midpoint ~ Segment + Frame + Syllnum +
         (1+ Segment + Frame + Repetition | Word), data = udata, REML = F)

#both models converge

u_F0_sum <- summary(u.F0.model)
#estimate: 43.029; error: 2.71; t = 15.855

u_F0_anov <- anova(u.F0.model.null, u.F0.model)
#chi^2 = 121.1, p = 2.2 e-16

#Residuals

plot(fitted(u.F0.model), residuals(u.F0.model))
#bimodally distributed, centering the data does not help

qqnorm(residuals(u.F0.model))
qqline(residuals(u.F0.model))


#[i] F0
i.F0.model <- 
  lmer(f0_midpoint ~ Stress + Segment + Frame + Syllnum +
         (1+ Segment + Frame + Repetition | Word), data = idata, REML = F)

i.F0.model.null <- 
  lmer(f0_midpoint ~ Segment + Frame + Syllnum +
         (1+ Segment + Frame + Repetition | Word), data = idata, REML = F)

#both models converge

i_F0_sum <- summary(i.F0.model)
#estimate: 36.349; error: 2.733; t = 13.300

i_F0_anov <- anova(i.F0.model.null, i.F0.model)
#chi^2 = 88.482, p = 2.2 e-16

#Residuals

plot(fitted(i.F0.model), residuals(i.F0.model))
#bimodally distributed

qqnorm(residuals(i.F0.model))
qqline(residuals(i.F0.model))



