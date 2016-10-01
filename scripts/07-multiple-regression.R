##################################################
### Read in data
##################################################

city = read.csv(file = "~/Documents/data/Applied-Regression-Lewis-Beck/riverside_final.csv") 

head(city)
tail(city)



##################################################
### Load libraries
##################################################

library(sm)
library(ggplot2)



##################################################
### Examine marginal distributions
##################################################

# Income (outcome)
sm.density(city$income, xlab = "Income")
mean(city$income)
sd(city$income)

# Education level (predictor)
sm.density(city$edu, xlab = "Education Level")
mean(city$edu)
sd(city$edu)

# Seniority (predictor)
sm.density(city$senior, xlab = "Seniority")
mean(city$senior)
sd(city$senior)



##################################################
### Examine conditional distributions of income
##################################################

# Income vs. education level
ggplot(data = city, aes(x = edu, y = income)) + geom_point() +
  theme_bw() +
  xlab("Education (in years)") +
  ylab("Income (in U.S. dollars)")


# Income vs. seniority
ggplot(data = city, aes(x = senior, y = income)) + geom_point() +
  theme_bw() +
  xlab("Seniority (in years)") +
  ylab("Income (in U.S. dollars)")



##################################################
### Correlataion matrix
##################################################

cor(city[ , c("income", "edu", "senior")])



##################################################
### Fit simple regression models
##################################################

lm.1 = lm(income ~ 1 + edu, data = city)
summary(lm.1)


lm.2 = lm(income ~  1 + senior, data = city)
summary(lm.2)



##################################################
### Fit the multiple regression model
##################################################

lm.3 = lm(income ~ 1 + edu + senior, data = city)
summary(lm.3)


lm.4 = lm(income ~  1 + senior + edu, data = city)
summary(lm.4)



##################################################
### Fit the standardized multiple regression model
##################################################

lm.5 = lm(scale(income) ~ 1 + scale(edu) + scale(senior), data = city)
summary(lm.5)




