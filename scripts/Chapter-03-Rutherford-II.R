##################################################
### Create data
##################################################

wr = data.frame(
  words.30  = c(7, 3, 6, 6, 5, 8, 6, 7),
  words.60  = c(7, 11, 9, 11, 10, 10, 11, 11),
  words.180 = c(8, 14, 10, 11, 12, 10, 11, 12)
  )
 


##################################################
### Load libraries
##################################################

library(ggplot2)
library(psych)
library(reshape2)
library(sm)



##################################################
### Prep data
##################################################

wr = melt(wr, variable.name = "condition", value.name = "words")

levels(wr$condition) = c("30s", "60s", "180s")




##################################################
### Create dummy coded predictors
##################################################

wr$con30 = ifelse(wr$condition == "30s", 1, 0)
wr$con60 = ifelse(wr$condition == "60s", 1, 0)
wr$con180 = ifelse(wr$condition == "180s", 1, 0)



##################################################
### Fit regression model
##################################################

lm.30 = lm(words ~ 1 + con60 + con180, data = wr)
summary(lm.30)
anova(lm.30)

lm.60 = lm(words ~ 1 + con30 + con180, data = wr)
summary(lm.60)
anova(lm.60)

# This is the model fitted in the Rutherford book
lm.180 = lm(words ~ 1 + con30 + con60, data = wr)
summary(lm.180)
anova(lm.180)


##################################################
### Overparameterization
##################################################

lm.tooMany = lm(words ~ 1 + con30 + con60 + con180, data = wr)
summary(lm.tooMany)



##################################################
### Create effects coded predictors
##################################################

# In book, this is X1
wr$con30e = ifelse(wr$condition == "30s", 1, 
	ifelse(wr$condition == "60s", 0, -1)
	)

# In book, this is X2
wr$con60e = ifelse(wr$condition == "60s", 1,
	ifelse(wr$condition == "30s", 0, -1)
	)



##################################################
### Fit regression model
##################################################

lm.effects = lm(words ~ 1 + con30e + con60e, data = wr)
summary(lm.effects)
anova(lm.effects)



##################################################
### Cell mean model
##################################################

lm.cm = lm(words ~ con30 + con60 + con180 - 1, data = wr)
summary(lm.cm)



