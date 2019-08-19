###################################################
### Load libraries
###################################################

library(broom)
library(dplyr)
library(ggplot2)
library(readr)
library(sm)
library(tidyr)



###################################################
### Read in the data
###################################################

mn = read_csv(file = "~/Documents/github/epsy-8251/data/mn-schools.csv")
head(mn)



###################################################
### Relationship between gradRate and SAT
###################################################

ggplot(data = mn, aes(x = sat, y = gradRate)) +
  geom_point() +
  #geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("Estimated median SAT score") +
  ylab("Six-year graduation rate")



###################################################
### Relationship between gradRate and SAT by looking at the residuals
###################################################

# Fit linear model
lm.1 = lm(gradRate ~ 1 + sat, data = mn)


# Obtain residuals
out = augment(lm.1)


# Examine assumption of linearity
sm.density(out$.std.resid, model = "normal")

ggplot(data = out, aes(x = sat, y = .std.resid)) +
  geom_point(size = 5) +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  theme_bw()  +
  xlab("Fitted values") +
  ylab("Standardized residuals")




###################################################
### Fit polynomial model
###################################################

# Create quadratic term
mn = mn %>% 
  mutate(
    sat_quadratic = sat * sat
    )


head(mn)


# Fit polynomial (quadratic) model
lm.2 = lm(gradRate ~ 1 + sat + sat_quadratic, data = mn)

glance(lm.2)
tidy(lm.2)



###################################################
### Fit the model directly in lm()
###################################################

lm.2 = lm(gradRate ~ 1 + sat + I(sat ^ 2), data = mn)

glance(lm.2)
tidy(lm.2)



###################################################
### Plot model results
###################################################

# Set up data and predict
plotData = crossing(
  sat = seq(from = 890, to = 1400, by = 10)
  ) %>%
  mutate(
    yhat = predict(lm.2, newdata = .)
  )


# Examine data
head(plotData)


# Plot
ggplot(data = plotData, aes(x = sat, y = yhat)) +
  geom_line() +
  theme_bw() +
  xlab("Estimated median SAT score") +
  ylab("Predicted graduation rate")




###################################################
### Examine residuals
###################################################

out2 = augment(lm.2)


# Check normality
sm.density(out2$.std.resid, model = "normal")


# Check other assumptions
ggplot(data = out2, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Standardized residuals")




###################################################
### Add covariates
###################################################

# Fit model
lm.3 = lm(gradRate ~ 1 + sat + I(sat ^ 2) + public, data = mn)


# Model-level output
glance(lm.3)


# Coefficient-level output
tidy(lm.3)



###################################################
### Plot model results
###################################################

# Set up data; get predicted values, coerce public into a factor for better plotting
plotData = crossing(
  sat = seq(from = 890, to = 1400, by = 10),
  public = c(0, 1)
  ) %>%
  mutate(
    yhat = predict(lm.3, newdata = .),
    public = factor(public, levels = c(0, 1), labels = c("Private", "Public"))
  )


# Examine data
head(plotData)


# Plot
ggplot(data = plotData, aes(x = sat, y = yhat, group = public, color = public, linetype = public)) +
  geom_line() +
  theme_bw() +
  xlab("Estimated median SAT score") +
  ylab("Predicted graduation rate") +
  scale_color_brewer(name = "Sector", palette = "Set1") +
  scale_linetype_manual(name = "Sector", values = c(1, 2))





