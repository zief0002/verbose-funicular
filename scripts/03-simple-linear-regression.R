##################################################
### Read in data
##################################################

math = read.csv(file = "~/Documents/EPsy-8262/data/homework-achievement.csv")

head(math)
tail(math)



##################################################
### Examine outcome/response
##################################################

library(sm)
sm.density(math$achievement)

library(psych)
describe(math$achievement)



##################################################
### Examine predictor
##################################################

sm.density(math$homework)
describe(math$homework)



##################################################
### Examine conditional distribution of achievement given homework
##################################################

library(ggplot2)

ggplot(data = math, aes(x = homework, y = achievement)) +
	geom_point(size = 5) +
	xlab("Time Spent on Homework") +
    ylab("Mathematics Achievement Score") +
	theme_bw()



##################################################
### Correlation
##################################################

cor(math[ , c("homework", "achievement")])



##################################################
### Regress achievement on homework
##################################################

lm.a = lm(achievement ~ homework, data = math)

lm.a

