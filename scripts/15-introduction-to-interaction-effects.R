##################################################
### Load libraries
##################################################

library(broom)
library(corrr)
library(educate)
library(ggridges)
library(tidyverse)



###################################################
### Read in the data
###################################################

work = read_csv(file = "https://raw.githubusercontent.com/zief0002/modeling/master/data/work-demands.csv")
head(work)



###################################################
### Exploration
###################################################

# Density plot of guilt conditioned on female
ggplot(data = work, aes(x = guilt, color = factor(female))) +
  stat_density(geom = "line") +
  theme_bw() +
  scale_color_manual(
    name = "", 
    values = c("#F5853F", "#424651"),
    labels = c("Non-females", "Females")
  ) +
  xlab("Standardized guilt") +
  ylab("Probability density")


# Density plot of boundary-spanning work conditioned on female
ggplot(data = work, aes(x = bound_span_work, color = factor(female))) +
  stat_density(geom = "line") +
  theme_bw() +
  scale_color_manual(
    name = "", 
    values = c("#F5853F", "#424651"),
    labels = c("Non-females", "Females")
  ) +
  xlab("Standardized boundary-spanning work") +
  ylab("Probability density")


# Compute summary statistics
work %>%
  group_by(female) %>%
  summarize(
    M_guilt  = mean(guilt),
    SD_guilt = sd(guilt),
    M_bound  = mean(bound_span_work),
    SD_bound = sd(bound_span_work),
    N = n()
  )


# Correlation matrix
work %>%
  correlate() %>%
  fashion(decimals = 2)



##################################################
### Fit main-effects models
##################################################

# Fit models
lm.a = lm(guilt ~ 1 + bound_span_work,                                data = work)
lm.b = lm(guilt ~ 1 + bound_span_work + female,                       data = work)
lm.c = lm(guilt ~ 1 + bound_span_work + female + authority + married, data = work)


# Coefficient-level output
tidy(lm.a)
tidy(lm.b)
tidy(lm.c)



##################################################
### Plot main-effects model results
##################################################

ggplot(data = work, aes(x = bound_span_work, y = guilt)) +
  geom_point(alpha = 0) +
  theme_bw() +
  xlab("Amount of boundary-spanning work") +
  ylab("Predicted home-life/work guilt") +
  geom_abline(intercept = 0.01, slope = 0.68, linetype = "solid", color = "#424651") +
  geom_abline(intercept = -0.40, slope = 0.68, linetype = "dashed", color = "#f5853f")



##################################################
### Explore data for interaction effect between boundary-spanning work and gender
##################################################

ggplot(data = work, aes(x = bound_span_work, y = guilt, color = factor(female))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  scale_color_manual(
    name = "", 
    values = c("#F5853F", "#424651"),
    labels = c("Non-females", "Females")
  ) +
  xlab("Standardized boundary-spanning work") +
  ylab("Standardized home-life/work guilt") +
  facet_wrap(~female)



##################################################
### Test interaction effect
##################################################

# Create interaction term
work = work %>%
  mutate(
    bound_work_female = bound_span_work * female
  )


# View data
head(work)


# Fit interaction model
lm.d = lm(guilt ~ 1 + bound_span_work + female + bound_work_female, data = work)
lm.d = lm(guilt ~ 1 + bound_span_work + female + bound_span_work:female, data = work)


tidy(lm.d)



##################################################
### Plot the differential effects from the fitted equation
##################################################

ggplot(data = work, aes(x = bound_span_work, y = guilt)) +
  geom_point(alpha = 0) +
  theme_bw() +
  xlab("Amount of boundary-spanning work") +
  ylab("Predicted home-life/work guilt") +
  geom_abline(intercept = 0.24, slope = 0.72, linetype = "solid", color = "#424651", lwd = 2) +
  geom_abline(intercept = -0.32, slope = 0.56, linetype = "dashed", color = "#f5853f", lwd = 2)



##################################################
### Add covariate(s)
##################################################

lm.e = lm(guilt ~ 1 + bound_span_work + female + authority + married + bound_work_female, data = work)
lm.e = lm(guilt ~ 1 + bound_span_work + female + authority + married + 
            bound_span_work:female, data = work)



tidy(lm.e)


lm.f = lm(guilt ~ 1 + bound_span_work + female + authority + married +
            bound_span_work:female + bound_span_work:married, data = work)

tidy(lm.f)


##################################################
### Plot interaction model -- Facet on gender
##################################################

# Female
ggplot(data = work, aes(x = bound_span_work, y = guilt)) +
  geom_point(alpha = 0) +
  theme_bw() +
  xlab("Amount of boundary-spanning work") +
  ylab("Predicted home-life/work guilt") +
  geom_abline(intercept = 0.24, slope = 0.73, linetype = "dashed", color = "#424651") +
  geom_abline(intercept = 0.01, slope = 0.73, linetype = "solid", color = "#424651") +
  ggtitle("Female")


# Non-female
ggplot(data = work, aes(x = bound_span_work, y = guilt)) +
  geom_point(alpha = 0) +
  theme_bw() +
  xlab("Amount of boundary-spanning work") +
  ylab("Predicted home-life/work guilt") +
  geom_abline(intercept = -0.39, slope = 0.61, linetype = "solid", color = "#f5853f") +
  geom_abline(intercept = -0.17, slope = 0.61, linetype = "dashed", color = "#f5853f") +
  ggtitle("Non-female")



##################################################
### Plot interaction model -- Facet on marital status
##################################################

# Non-married
ggplot(data = work, aes(x = bound_span_work, y = guilt)) +
  geom_point(alpha = 0) +
  theme_bw() +
  xlab("Amount of boundary-spanning work") +
  ylab("Predicted home-life/work guilt") +
  geom_abline(intercept = 0.01, slope = 0.73, linetype = "solid", color = "#424651") +
  geom_abline(intercept = -0.39, slope = 0.61, linetype = "dashed", color = "#f5853f") +
  ggtitle("Non-married")

# Married
ggplot(data = work, aes(x = bound_span_work, y = guilt)) +
  geom_point(alpha = 0) +
  theme_bw() +
  xlab("Amount of boundary-spanning work") +
  ylab("Predicted home-life/work guilt") +
  geom_abline(intercept = 0.24, slope = 0.73, linetype = "solid", color = "#424651") +
  geom_abline(intercept = -0.17, slope = 0.61, linetype = "dashed", color = "#f5853f") +
  ggtitle("Married")



##################################################
### Examine model assumptions
##################################################

# Create augmented data
aug_e = augment(lm.e)


# Examine normality assumption
ggplot(data = aug_e, aes(x = .std.resid)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Standardized Residuals") +
  ylab("Probability density")


# Examine other assumptions
ggplot(data = aug_e, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_smooth(method = "loess") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted Values") +
  ylab("Standardized Residuals")



