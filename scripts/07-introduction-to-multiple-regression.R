##################################################
### Read in data
##################################################

city = read.csv(file = "~/Google Drive/andy/epsy-8251/data/riverside.csv") 
head(city)



##################################################
### Load libraries
##################################################

library(dplyr)
library(ggplot2)
library(sm)



##################################################
### Fit regression model
##################################################

lm.1 = lm(income ~ 1 + education, data = city) 
summary(lm.1)




##################################################
### Examine seniority predictor
##################################################

# Examine the marginal distribution
sm.density(city$seniority, xlab = "Seniority (in years)")


# Compute mean and standard deviation
city %>% summarize(M = mean(seniority), SD = sd(seniority))



##################################################
### Examine relationship between seniority and income
##################################################

# Scatterplot
ggplot(data = city, aes(x = seniority, y = income)) + geom_point() +
  theme_bw() +
  xlab("Seniority (in years)") +
  ylab("Income (in dollars)")


# Correlation matrix
cor(city[ , c("income", "education", "seniority")])



##################################################
### Regress income on seniority
##################################################

lm.2 = lm(income ~ 1 + seniority, data = city) 
summary(lm.2)




##################################################
### Fit the multiple regression model
##################################################

lm.3 = lm(income ~ 1 + education + seniority, data = city) 
summary(lm.3)


# Partitioning the sums of squares
anova(lm.3)




