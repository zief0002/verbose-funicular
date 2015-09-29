##################################################
### Read in data
##################################################

math = read.csv(file = "~/Documents/EPsy-8262/data/homework-achievement.csv")

head(math)
tail(math)



##################################################
### Regress achievement on homework
##################################################

lm.a = lm(achievement ~ homework, data = math)



##################################################
### Model output with coefficients and SEs
##################################################

library(arm)
display(lm.a)



##################################################
### Model output with coefficients, SEs, and p-values
##################################################

summary(lm.a)



##################################################
### Examine conditional distribution of achievement given homework
##################################################

library(ggplot2)

ggplot(data = math, aes(x = homework, y = achievement)) +
	geom_smooth(method = "lm", se = TRUE) +
	xlab("Time Spent on Homework") +
    ylab("Mathematics Achievement Score") +
	theme_bw()



##################################################
### Watercolor plot
##################################################

source("/Users/andrewz/Documents/EPsy-8262/scripts/S15-scripts/waterColorPlots.R")
vwReg(achievement ~ homework, data = math, method = lm)
