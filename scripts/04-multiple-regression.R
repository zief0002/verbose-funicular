##################################################
### Read in data
##################################################

multReg = read.csv(file = "~/Documents/EPsy-8262/data/homework-education-gpa.csv")

head(multReg)
tail(multReg)



##################################################
### Load libraries
##################################################

library(psych)
library(sm)
library(ggplot2)



##################################################
### Examine variables
##################################################

# outcome
sm.density(multReg$gpa)
describe(multReg$gpa)

# Homework predictor
sm.density(multReg$homework)
describe(multReg$homework)

# Parent education predictor
sm.density(multReg$parentEd)
describe(multReg$parentEd)

ggplot(data = multReg, aes(x = parentEd, y = gpa)) + geom_point(size = 3)




lm.a = lm(gpa ~ homework, data = multReg)
summary(lm.a)


lm.b = lm(gpa ~  parentEd, data = multReg)
summary(lm.b)


cor(multReg[c("gpa", "homework", "parentEd")])


##################################################
### Fit the multiple regression model
##################################################

lm.a = lm(gpa ~ homework + parentEd, data = multReg)
summary(lm.a)


lm.b = lm(gpa ~  parentEd + homework, data = multReg)
summary(lm.b)


anova(lm.a)
anova(lm.b)

##################################################
### Fit the standardized multiple regression model
##################################################

lm.b = lm(scale(gpa) ~ scale(homework) + scale(parentEd), data = multReg)
summary(lm.b)




