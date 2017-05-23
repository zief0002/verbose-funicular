##################################################
### Read in data
##################################################

city = read.csv(file = "~/Google Drive/andy/epsy-8251/data/riverside.csv") 

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

sm.density(city$education)

mean(city$education)
sd(city$education)



##################################################
### Examine conditional distribution of income given edu
##################################################

ggplot(data = city, aes(x = education, y = income)) + geom_point() +
  theme_bw() +
  xlab("Education (in years)") +
  ylab("Income (in U.S. dollars)")



##################################################
### Correlation
##################################################

cor(city[ , c("income", "education")])



##################################################
### Regress income on edu
##################################################

lm.1 = lm(income ~ 1 + education, data = city)
lm.1



##################################################
### Using the regression equation
##################################################

# See the 12th observation's data
city[12, ]


# Compute the predicted income using the regression equation
11321 + 2651 * 14


# Compute the residual
64926 - 48435



