###################################################
### Read in the data
###################################################

beauty = read.csv(file = "/Users/andrewz/Documents/GitHub/EPsy-8251/data/beauty.csv")
head(beauty)



###################################################
### Fit one-predictor models
###################################################

lm.female = lm(avgeval ~ female, data = beauty)
summary(lm.female)

lm.tenured = lm(avgeval ~ tenured, data = beauty)
summary(lm.tenured)


anova(lm.female)
anova(lm.tenured)




###################################################
### Fit two-predictor models
###################################################

lm.both = lm(avgeval ~ female + tenured, data = beauty)
summary(lm.both)


lm.both2 = lm(avgeval ~ tenured + female, data = beauty)
summary(lm.both2)


anova(lm.both)
anova(lm.both2)



###################################################
### Three-predictor model
###################################################

lm.three = lm(avgeval ~ female + nonenglish + btystdave, data = beauty)

anova(lm.three)

summary(lm.three)



###################################################
### Delta F-test to test the change in R^2
###################################################

# Does tenured explain additional variation in course evaluation ratings 
# beyond what female explains?

lm.1 = lm(avgeval ~ female, data = beauty)
lm.2 = lm(avgeval ~ female + tenured, data = beauty)

anova(lm.1, lm.2)


# Does tenured AND beauty explain additional variation in course evaluation ratings 
# beyond what female explains?

lm.3 = lm(avgeval ~ female + tenured + btystdave, data = beauty)

anova(lm.1, lm.3)


# Does beauty explain additional variation in course evaluation ratings 
# beyond what female AND tenured explains?

anova(lm.2, lm.3)




###################################################
### Block entry testing for dummy variables
###################################################

ecls = read.csv(file = "/Users/andrewz/Documents/EPsy-8262/data/ecls.csv")
head(ecls)

lm.1 = lm(reading ~ ses, data = ecls)
summary(lm.1)


lm.2 = lm(reading ~ ethnic, data = ecls)
summary(lm.2)

lm.3 = lm(reading ~ ses + ethnic, data = ecls)

# Test all the effects in one "omnibus" test. 
# Do differences in ethnicity explain additional variation in reading scores 
# beyond what SES does?

anova(lm.1, lm.3)




