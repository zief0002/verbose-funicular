###################################################
### Read in Data (Prestige2.csv)
###################################################

Prestige = read.csv(file =  "/Users/andrewz/Documents/EPSY-8251/data/Prestige2.csv")
head(Prestige)



###################################################
### Libraries
###################################################

library(ggplot2)
library(sm)
library(psych)


###################################################
### Raw model
###################################################

# Fit interaction model 
lm.1 = lm(prestige ~ income + education + education:income, data = Prestige)
summary(lm.1)



###################################################
### Scaled model
###################################################

# Fit interaction model 
lm.2 = lm(prestige ~ I(income/1000) + education + education:I(income/1000), data = Prestige)
summary(lm.2)



###################################################
### Standardized model
###################################################

# Standardize both predictors
Prestige$income.std = Prestige$income / sd(Prestige$income)
Prestige$education.std = Prestige$education / sd(Prestige$education)

# Fit interaction model 
lm.3 = lm(prestige ~ income.std + education.std + education.std:income.std, data = Prestige)
summary(lm.3)



###################################################
### Centered model
###################################################

lm.4 = lm(prestige ~ I(income - 5000) + I(education - 8) + I(education - 8):I(income - 5000), data = Prestige)
summary(lm.4)



###################################################
### Single predictor
###################################################


lm.5 = lm(prestige ~ income, data = Prestige)
lm.6 = lm(prestige ~ I(income - 5000), data = Prestige)

summary(lm.5)$coefficients
summary(lm.6)$coefficients



###################################################
### Mean-centering
###################################################

lm.7 = lm(prestige ~ I(income - mean(income)), data = Prestige)
summary(lm.7)




###################################################
### Multiple predictors: Main-effects models
###################################################


lm.8 = lm(prestige ~ income + education, data = Prestige)
lm.9 = lm(prestige ~ I(income - mean(income)) + I(education - mean(education)), data = Prestige)

summary(lm.8)$coefficients
summary(lm.9)$coefficients



###################################################
### Centered  and scaled model
###################################################

Prestige$income.z = (Prestige$income - mean(Prestige$income)) / sd(Prestige$income)
Prestige$education.z = (Prestige$education - mean(Prestige$education)) / sd(Prestige$education)

head(Prestige)

lm.10 = lm(prestige ~ income.z + education.z + income.z:education.z, data = Prestige)
summary(lm.10)



###################################################
### Collinearity
###################################################

summary(lm.1)$coefficients

cor(Prestige[c("prestige", "income", "education")])

library(car)
vif(lm.1)

sqrt(vif(lm.1))


###################################################
### Centering to alleviate collinearity
###################################################

Prestige$income.c = Prestige$income - mean(Prestige$income)
Prestige$education.c = Prestige$education - mean(Prestige$education)

lm.11 = lm(prestige ~ income.c + education.c + income.c:education.c, data = Prestige)

vif(lm.11)

summary(lm.11)$coefficients


