##################################################
### Load libraries
##################################################

library(broom)
library(corrr)
library(educate)
library(ggridges)
library(patchwork)
library(tidyverse)



###################################################
### Read in the data
###################################################

fert = read_csv(file = "https://raw.githubusercontent.com/zief0002/modeling/master/data/fertility.csv")
head(fert)



##################################################
### Explore data for interaction b/w female education level and contraceptive use
##################################################

# Examine range of female education variable
fert %>%
  select(educ_female) %>%
  summary()


# Discretize female education level
fert = fert %>%
  mutate(
    female_educ_discrete = case_when(
      educ_female < 4.8 ~ "Quartile 1",
      educ_female >= 4.8 & educ_female < 8 ~ "Quartile 2",
      educ_female >= 8 & educ_female < 10.25 ~ "Quartile 3",
      educ_female >= 10.25 ~ "Quartile 4"
    )
  )


# View data
head(fert)


# Plot data
ggplot(data = fert, aes(x = contraceptive, y = fertility_rate, color = female_educ_discrete)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  xlab("Contraception use") +
  ylab("Fertility rate") +
  scale_color_brewer(palette = "Set2") +
  facet_wrap(~female_educ_discrete, nrow = 1) +
  guides(color = FALSE)



##################################################
### Fit and test interaction model
##################################################

# Fit model
lm.a = lm(fertility_rate ~ 1 + educ_female + contraceptive + educ_female:contraceptive, data = fert)



print(glance(lm.a), width = Inf) # Model-level
tidy(lm.a)                       # Coefficient-level



##################################################
### Plot results from fitted model
##################################################

ggplot(data = fert, aes(x = contraceptive, y = fertility_rate)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = 4.79, slope = -0.03, color = "#022f40", linetype = "dotted") +
  geom_abline(intercept = 3.56, slope = -0.018, color = "#f26419", linetype = "dashed") +
  geom_abline(intercept = 2.74, slope = -0.01, color = "#758e4f", linetype = "solid") +
  theme_bw() +
  xlab("Contraceptive use") +
  ylab("Fertility rate")
  
  

##################################################
### Interpret interaction model effects
##################################################

tidy(lm.a)



##################################################
### Include covariates in the model
##################################################

# Fit model
lm.b = lm(fertility_rate ~ 1 + educ_female + contraceptive + infant_mortality + educ_female:contraceptive, data = fert)

print(glance(lm.b), width = Inf) # Model-level
tidy(lm.b)                       # Coefficient-level



##################################################
### Plot the model results
##################################################

# Plot the fitted model (infant mortality rate = 7)
p1 = ggplot(data = fert, aes(x = contraceptive, y = fertility_rate)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = 3.61, slope = -0.02, color = "#f26419", linetype = "solid") +
  geom_abline(intercept = 2.25, slope = -0.01, color = "#022f40", linetype = "dashed") +
  theme_bw() +
  xlab("Contraceptive use") +
  ylab("Fertility rate") +
  ggtitle("Infant mortality rate at the first quartile (7%)")


# Plot the fitted model (infant mortality rate = 41)
p2 = ggplot(data = fert, aes(x = contraceptive, y = fertility_rate)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = 4.29, slope = -0.02, color = "#f26419", linetype = "solid") +
  geom_abline(intercept = 2.93, slope = -0.01, color = "#022f40", linetype = "dashed") +
  theme_bw() +
  xlab("Contraceptive use") +
  ylab("Fertility rate") +
  ggtitle("Infant mortality rate at the third quartile (41%)")


# Layout side-by-side plot
p1 | p2                                                                                                                                  



##################################################
### Higher order interactions
##################################################

# Fit model
lm.c = lm(fertility_rate ~ 1 + educ_female + contraceptive + infant_mortality + 
            educ_female:contraceptive + infant_mortality:contraceptive + educ_female:infant_mortality +
            educ_female:contraceptive:infant_mortality, data = fert)

print(glance(lm.c), width = Inf) # Model-level
tidy(lm.c)                       # Coefficient-level
            
            

##################################################
### Plot the fitted model
##################################################

# Plot the fitted model (infant mortality rate = 7)
p1 = ggplot(data = fert, aes(x = contraceptive, y = fertility_rate)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = 4.49, slope = -0.04, color = "#f26419", linetype = "solid") +
  geom_abline(intercept = 2.09, slope = -0.005, color = "#022f40", linetype = "dashed") +
  theme_bw() +
  xlab("Contraceptive use") +
  ylab("Fertility rate") +
  ggtitle("Infant mortality rate at the first quartile (7%)")


# Plot the fitted model (infant mortality rate = 41)
p2 = ggplot(data = fert, aes(x = contraceptive, y = fertility_rate)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = 4.65, slope = -0.02, color = "#f26419", linetype = "solid") +
  geom_abline(intercept = 3.02, slope = -0.008, color = "#022f40", linetype = "dashed") +
  theme_bw() +
  xlab("Contraceptive use") +
  ylab("Fertility rate") +
  ggtitle("Infant mortality rate at the third quartile (41%)")


# Layout side-by-side plot
p1 | p2


