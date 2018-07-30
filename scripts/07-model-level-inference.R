##################################################
### Load libraries
##################################################

library(broom)
library(dplyr)
library(ggplot2)
library(readr)
library(sm)



##################################################
### Read in data
##################################################

city = read_csv(file = "~/Documents/github/epsy-8251/data/riverside.csv") 

head(city)
tail(city)



##################################################
### Regress income on edu
##################################################

lm.1 = lm(income ~ 1 + education, data = city)



##################################################
### Model-level output
##################################################

glance(lm.1)



##################################################
### ANOVA decomposition
##################################################

anova(lm.1)



##################################################
### Plot of the fitted model and the model uncertainty
##################################################

ggplot(data = city, aes(x = education, y = income)) +
  geom_smooth(method = "lm", se = TRUE) +
  xlab("Education level") +
  ylab("Income") +
  theme_bw()

