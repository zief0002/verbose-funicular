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
### Scatterplot of income versus education level (uncentered, unscaled)
##################################################

ggplot(data = city, aes(x = edu, y = income)) + 
  geom_point() +
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
summary(lm.1)



##################################################
### Education level in months
##################################################

# Create months variable
city$edu_months = city$edu * 12
head(city)

# Correlation
cor(city[ , c("income", "edu_months")])

# Scatterplot
ggplot(data = city, aes(x = edu_months, y = income)) +
  geom_point() +
  theme_bw()

# Fit regression with months as predictor
lm.2 = lm(income ~ 1 + edu_months, data = city)
summary(lm.2)



##################################################
### Centering
##################################################

# Center the predictor
city$edu_centered = city$edu - 12
head(city)

# Examine means for uncentered and centered predictors
mean(city$edu)
mean(city$edu_centered)

# Examine SDs for uncentered and centered predictors
sd(city$edu)
sd(city$edu_centered)

# Examine density plots for uncentered and centered predictors
sm.density(city$edu)
sm.density(city$edu_centered)

# Correlation
cor(city[ , c("income", "edu_centered")])

# Scatterplot
ggplot(data = city, aes(x = edu_centered, y = income)) +
  geom_point() +
  theme_bw()

# Fit regression
lm.3 = lm(income ~ 1 + edu_centered, data = city)
summary(lm.3)



##################################################
### Mean centering
##################################################

# Create mean centered predictor
city$edu_mean_centered = city$edu - mean(city$edu)
head(city)

# Correlation
cor(city[ , c("income", "edu_mean_centered")])

# Scatterplot
ggplot(data = city, aes(x = edu_mean_centered, y = income)) +
  geom_point() +
  theme_bw()

# Fit regression
lm.4 = lm(income ~ 1 + edu_mean_centered, data = city)
summary(lm.4)



##################################################
### Scaling
##################################################

# Create scaled predictor
city$edu_scaled = city$edu / sd(city$edu)
head(city)

# Examine scaled predictor
mean(city$edu_scaled)
sd(city$edu_scaled)

# Correlation
cor(city[ , c("income", "edu_scaled")])

# Scatterplot
ggplot(data = city, aes(x = edu_scaled, y = income)) +
  geom_point() +
  theme_bw()

# Fit regression
lm.5 = lm(income ~ 1 + edu_scaled, data = city)
summary(lm.5)



##################################################
### Standardizing
##################################################

# Create standardized predictor and outcome
city$z_edu = (city$edu - mean(city$edu)) / sd(city$edu)
city$z_income = (city$income - mean(city$income)) / sd(city$income)
head(city)

# Correlation
cor(city[ , c("z_income", "z_edu")])

# Scatterplot
ggplot(data = city, aes(x = z_edu, y = z_income)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()

# Fit regression
lm.6 = lm(z_income ~ 1 + z_edu, data = city)
summary(lm.6)

