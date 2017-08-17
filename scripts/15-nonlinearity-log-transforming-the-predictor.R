###################################################
### Load libraries
###################################################

library(broom)
library(dplyr)
library(ggplot2)
library(readr)
library(sm)



###################################################
### Read in the data
###################################################

mn = read_csv(file = "~/Dropbox/epsy-8251/data/mnSchools.csv")
head(mn)




###################################################
### Relationship between gradRate and SAT
###################################################

ggplot(data = mn, aes(x = sat, y = gradRate)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("Median SAT score") +
  ylab("Six-year graduation rate")



###################################################
### Example computations of logarithms
###################################################

log(32, base = 2)
log( 4, base = 2)

log(1000, base = 10)



###################################################
### Log-2 transffomation of SAT 
###################################################

mn = mn %>% mutate(L2sat = log(sat, base = 2))
head(mn)



###################################################
### Fit model with log-transformed predictor
###################################################

lm.1 = lm(gradRate ~ 1 + L2sat, data = mn)



###################################################
### Examine linearity assumption
###################################################

# Obtain residuals
out = augment(lm.1)

# Check linearity assumptions
ggplot(data = out, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  theme_bw()



###################################################
### Examine regression ooutput
###################################################

summary(lm.1)



###################################################
### Two hypothetical schools whose L2sat score differs by 1
###################################################

-1013.872 + 106.439 * 10.00843
-1013.872 + 106.439 * 11.00843




###################################################
### Alternative method of fitting the same model
###################################################

lm.1 = lm(gradRate ~ 1 + log(sat, base = 2), data = mn)
summary(lm.1)



###################################################
### Plot model results
###################################################

# Set up data
plotData = expand.grid(
  sat = seq(from = 890, to = 1400, by = 10)
)

# Predict
plotData = plotData %>% mutate(yhat = predict(lm.1, newdata = plotData))

# Examine data
# head(plotData)

# Plot
ggplot(data = plotData, aes(x = sat, y = yhat)) +
  geom_line() +
  theme_bw() +
  xlab("Median SAT score") +
  ylab("Predicted graduation rate")



###################################################
### Use base-10 logarithm
###################################################

# Create predictor in dataset (not necessARY if we use the alternative method of fitting the model)
mn = mn %>% mutate(L10sat = log(sat, base = 10))
head(mn)

# Fit model
lm.2 = lm(gradRate ~ 1 + log(sat, base = 10), data = mn)
summary(lm.2)



###################################################
### Use natural logarithm (base-e)
###################################################

# e^1
exp(1)


# Natural log of 1030
log(1030)


# Fit regression model
lm.3 = lm(gradRate ~ 1 + log(sat), data = mn)
summary(lm.3)



###################################################
### Add covariates
###################################################

lm.4 = lm(gradRate ~ 1 + public + log(sat), data = mn)
summary(lm.4)



###################################################
### Plot fitted model
###################################################

# Set up data
plotData = expand.grid(
  sat = seq(from = 890, to = 1400, by = 10),
  public = c(0, 1)
)

# Predict
plotData = plotData %>% mutate(yhat = predict(lm.4, newdata = plotData))
head(plotData)

# Plot
ggplot(data = plotData, aes(x = sat, y = yhat, color = factor(public))) +
  geom_line() +
  theme_bw() +
  xlab("Median SAT score") +
  ylab("Predicted graduation rate") +
  scale_color_brewer(name = "Sector", palette = "Set1", labels = c("Private", "Public"))



