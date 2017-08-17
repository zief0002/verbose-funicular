##################################################
### Load libraries
##################################################

library(ggplot2)
library(readr)



##################################################
### Read in data
##################################################

city = read_csv(file = "~/Dropbox/epsy-8251/data/riverside.csv") 

head(city)
tail(city)



##################################################
### Regress income on edu
##################################################

lm.1 = lm(income ~ 1 + education, data = city)



##################################################
### Model output with coefficients, SEs, and p-values
##################################################

summary(lm.1)



##################################################
### Interval estimates of the regression parameters
##################################################

confint(lm.1)



##################################################
### Plot regression line and confidence envelope
##################################################

ggplot(data = city, aes(x = education, y = income)) +
	geom_smooth(method = "lm", se = TRUE) +
	xlab("Education level") +
  ylab("Income") +
	theme_bw()



