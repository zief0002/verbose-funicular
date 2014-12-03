###################################################
### Read in Data
###################################################

Prestige = read.csv(file =  "/Users/andrewz/Documents/EPSY-8251/data/Prestige.csv")
head(Prestige)


###################################################
### Libraries
###################################################

library(ggplot2)
library(sm)
library(psych)



###################################################
### Simple models
###################################################

lm.int = lm(prestige ~ 1, data = Prestige)
anova(lm.int)

lm.educ = lm(prestige ~ education, data = Prestige)
summary(lm.educ)
anova(lm.educ)


lm.inc = lm(prestige ~ income, data = Prestige)
summary(lm.inc)
anova(lm.inc)



###################################################
### MR models
###################################################

lm.educ.inc = lm(prestige ~ education + income, data = Prestige)
anova(lm.educ.inc)

lm.inc.educ = lm(prestige ~ income + education, data = Prestige)
anova(lm.inc.educ)


summary(lm.educ.inc)
summary(lm.inc.educ)



###################################################
### Type III SS
###################################################

library(car)
Anova(lm.educ.inc, type = "III")




