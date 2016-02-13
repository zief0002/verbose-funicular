##################################################
### Read in data
##################################################

math = read.csv(file = "~/Documents/EPsy-8262/data/homework-achievement.csv")

head(math)
tail(math)



##################################################
### Load libraries
##################################################

library(ggplot2)



##################################################
### Regress achievement on homework
##################################################

lm.a = lm(achievement ~ homework, data = math)
lm.a


##################################################
### Model output with coefficients-level inferential information
##################################################

summary(lm.a)
confint(lm.a)


##################################################
### ANOVA decomposition and model-level inferential information
##################################################

anova(lm.a)



##################################################
### Plot the fitted model along with the confidence envelope
##################################################

ggplot(data = math, aes(x = homework, y = achievement)) +
	geom_smooth(method = "lm", se = TRUE) +
	xlab("Time Spent on Homework") +
  ylab("Mathematics Achievement Score") +
	theme_bw()



##################################################
### Watercolor plot
##################################################

source("~/Documents/EPsy-8251/scripts/S16-scripts/waterColorPlots.R")
vwReg(achievement ~ homework, data = math, method = lm)
