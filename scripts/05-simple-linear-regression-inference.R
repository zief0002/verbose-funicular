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



##################################################
### Regress income on edu
##################################################

lm.1 = lm(income ~ 1 + edu, data = city)



##################################################
### Model output with coefficients, SEs, and p-values
##################################################

summary(lm.1)



##################################################
### Interval estimates of the regression parameters
##################################################

confint(lm.1)



##################################################
### Plot regression line and model uncertainty
##################################################

ggplot(data = city, aes(x = edu, y = income)) +
	geom_smooth(method = "lm", se = TRUE) +
	xlab("Education Level") +
  ylab("Income") +
	theme_bw()



