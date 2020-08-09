##################################################
### Load libraries
##################################################

library(broom)
library(corrr)
library(educate)
library(gridExtra)
library(tidyverse)



###################################################
### Read in the data
###################################################

work = read_csv(file = "~/Documents/github/epsy-8251/data/work-demands.csv")
head(work)



##################################################
### Fit main-effects models
##################################################

# Fit models
lm.1 = lm(guilt ~ 1 + bound_span_work, data = work)
lm.2 = lm(guilt ~ 1 + bound_span_work + female, data = work)

# Coefficient-level output
tidy(lm.1)
tidy(lm.2)



##################################################
### Plot main-effects model results
##################################################

ggplot(data = work, aes(x = bound_span_work, y = guilt)) +
  geom_point(alpha = 0) +
  theme_bw() +
  xlab("Amount of boundary-spanning work") +
  ylab("Predicted guilt") +
  geom_abline(intercept = 3.09, slope = -.003, linetype = "dashed", color = "darkblue") +
  geom_abline(intercept = (3.09 + 0.52), slope = -.003, linetype = "solid", color = "darkorange")



##################################################
### Explore data for differential effects between boundary-spanning work and gender
##################################################

ggplot(data = work, aes(x = bound_span_work, y = guilt, color = factor(female))) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  xlab("Amount of boundary-spanning work") +
  ylab("Predicted guilt") +
  scale_color_manual(name = "", values = c("darkblue", "darkorange")) +
  facet_wrap(~female)



##################################################
### Fit interaction model
##################################################

# Create interaction term
work = work %>%
  mutate(
    bound_work_female = bound_span_work * female
  )

# View data
head(work)


# Fit interaction model
lm.3 = lm(guilt ~ 1 + bound_span_work + female + bound_work_female, data = work)

tidy(lm.3)



##################################################
### Plot the differential effects from the fitted equation
##################################################

ggplot(data = work, aes(x = bound_span_work, y = guilt)) +
  geom_point(alpha = 0) +
  theme_bw() +
  xlab("Amount of boundary-spanning work") +
  ylab("Predicted guilt") +
  geom_abline(intercept = 3.54, slope = -0.23, color = "darkblue", linetype = "dashed") +
  geom_abline(intercept = 3.37, slope = 0.13, color = "darkorange", linetype = "solid")



##################################################
### Add covariate(s)
##################################################

# Fit interaction model with covariate
lm.4 = lm(guilt ~ 1 + authority + bound_span_work + female + bound_work_female, data = work)

tidy(lm.4)



##################################################
### Plot interaction model results
##################################################

p1 = ggplot(data = work, aes(x = bound_span_work, y = guilt)) +
  geom_point(alpha = 0) +
  theme_bw() +
  xlab("Amount of boundary-spanning work") +
  ylab("Predicted guilt") +
  geom_abline(intercept = 3.84, slope = -0.22, color = "darkblue", linetype = "dashed") +
  geom_abline(intercept = 3.65, slope = 0.15, color = "darkorange", linetype = "dashed") +
  ggtitle("No authority")

p2 = ggplot(data = work, aes(x = bound_span_work, y = guilt)) +
  geom_point(alpha = 0) +
  theme_bw() +
  xlab("Amount of boundary-spanning work") +
  ylab("Predicted guilt") +
  geom_abline(intercept = 3.20, slope = -0.22, color = "darkblue", linetype = "solid") +
  geom_abline(intercept = 3.01, slope = 0.15, color = "darkorange", linetype = "solid") +
  ggtitle("Highest level of authority")

grid.arrange(p1, p2, nrow = 1)



##################################################
### Examine model assumptions
##################################################

# Create augmented data
out.4 = augment(lm.4)


# Examine normality assumption
ggplot(data = out.4, aes(x = .std.resid)) +
  stat_watercolor_density(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Standardized Residuals")

# Examine other assumptions
ggplot(data = out.4, aes(x = .fitted, y = .std.resid)) +
  geom_jitter() +
  geom_smooth(se = TRUE) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), linetype = "dotted") +
  theme_bw() +
  xlab("Fitted Values") +
  ylab("Standardized Residuals")



##################################################
### Fit distress model
##################################################

lm.5 = lm(distress ~ 1 + authority + bound_span_work + female + bound_work_female, data = work)

tidy(lm.5)


# Plot results controlling for authority (set to median value)
ggplot(data = work, aes(x = bound_span_work, y = distress)) +
  geom_point(alpha = 0) +
  theme_bw() +
  xlab("Amount of boundary-spanning work") +
  ylab("Predicted psychological distress") +
  geom_abline(intercept = 14.81, slope = -0.70, color = "darkblue", linetype = "dashed") +
  geom_abline(intercept = 14.47, slope = 0.65, color = "darkorange", linetype = "solid")


# Create augmented data
out.5 = augment(lm.5)

# Examine normality assumption
ggplot(data = out.5, aes(x = .std.resid)) +
  stat_watercolor_density(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Standardized Residuals")

# Examine other assumptions
ggplot(data = out.5, aes(x = .fitted, y = .std.resid)) +
  geom_jitter() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-3, 3), linetype = "dotted") +
  theme_bw() +
  xlab("Fitted Values") +
  ylab("Standardized Residuals")




lm.6 = lm(distress ~ 1 + authority + bound_span_work + female + 
             + bound_span_work:female + authority:female, 
          data = work)

tidy(lm.6)

crossing(
  bound_span_work = seq(from = 0, to = 5, by = 1),
  authority = c(0, 4),
  female = c(0, 1)
) %>%
  mutate(
    yhat = predict(lm.6, newdata = .),
    female = ifelse(female == 1, "female", "male"),
    authority = ifelse(authority == 0, "Low", "High"),
  ) %>%
  ggplot(aes(x = bound_span_work, y = yhat, color = female)) +
  geom_line() +
  facet_wrap(~authority)

