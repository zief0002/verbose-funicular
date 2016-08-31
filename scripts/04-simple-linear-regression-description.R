##################################################
### Read in data
##################################################

city = read.csv(file = "~/Documents/data/Applied-Regression-Lewis-Beck/riverside_final.csv") 

head(city)
tail(city)



##################################################
### Load libraries
##################################################

library(ggplot2)
library(sm)



##################################################
### Examine outcome/response
##################################################

sm.density(city$income)

mean(city$income)
sd(city$income)



##################################################
### Examine predictor
##################################################

sm.density(city$edu)

mean(city$edu)
sd(city$edu)



##################################################
### Examine conditional distribution of income given edu
##################################################

ggplot(data = city, aes(x = edu, y = income)) + geom_point() +
  theme_bw() +
  xlab("Education (in years)") +
  ylab("Income (in U.S. dollars)")



##################################################
### Correlation
##################################################

cor(city[ , c("income", "edu")])



##################################################
### Regress income on edu
##################################################

lm.1 = lm(income ~ 1 + edu, data = city)
lm.1



##################################################
### Observation, Prediction, and Error
##################################################

city[12, ]

11321 + 2651 * 14  # predicted income

64926 - 48435      # residual



##################################################
### Compute the SSE
##################################################

# Step 1: Compute the predicted values of Y
y_hat = 11321 + 2651 * city$edu
y_hat

# Step 2: Compute the residuals
errors = city$income - y_hat
errors

# Step 3: Compute the squared residuals
sq_errors = errors^2
sq_errors

# Step 4: Compute the sum of the squared residuals
sum(sq_errors)

# Chain all the computations together
sse.1 = sum((city$income - (11321 + 2651 * city$edu))^2)
sse.1



##################################################
### Intercept-Only Model: A Baseline for Comparison
##################################################

# Fit intercept-only model
lm.0 = lm(income ~ 1, data = city)
lm.0

# Plot of the intercept-only model
ggplot(data = city, aes(x = edu, y = income)) +
  geom_point() +
  geom_hline(yintercept = 53742, color = "blue") +
  xlab("Education (in years)") +
  ylab("Income") +
  theme_bw()


# Compute SSE for intercept-only (baseline) model
sse.0 = sum((city$income - 53742)^2)
sse.0


##################################################
### Compute proportion of the reduction in error (PRE)
##################################################

pre = (sse.0 - sse.1) / sse.0
pre

# Correlation ^ 2
cor(city[c("income", "edu")])^2


# Unexplained variation
1 - pre



