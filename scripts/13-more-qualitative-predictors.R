###################################################
### Read in the data
###################################################

ecls = read.csv(file = "/Users/andrewz/Documents/EPsy-8262/data/ecls.csv")
head(ecls)



##################################################
### Load libraries
##################################################

library(ggplot2)
library(psych)



##################################################
### Examine sample mean differences
##################################################


describeBy(ecls$reading, ecls$ethnic)

ggplot(data = ecls, aes(x = ethnic, y = reading)) +
	geom_boxplot() +
	theme_bw()



##################################################
### Examine levels of factor and create dummy variables
##################################################

levels(ecls$ethnic)

ecls$asian = ifelse(ecls$ethnic == "Asian", 1, 0)
ecls$black = ifelse(ecls$ethnic == "Black", 1, 0)
ecls$hispanic = ifelse(ecls$ethnic == "Hispanic", 1, 0)
ecls$other = ifelse(ecls$ethnic == "Other", 1, 0)
ecls$white = ifelse(ecls$ethnic == "White", 1, 0)



##################################################
### Fit regression models (ANOVA)
##################################################

lm.1.asian = lm(reading ~ black + hispanic + other + white, data = ecls)
summary(lm.1.asian)

lm.1.black = lm(reading ~ asian + hispanic + other + white, data = ecls)
summary(lm.1.black)

lm.1.hispanic = lm(reading ~ asian + black + other + white, data = ecls)
summary(lm.1.hispanic)

lm.1.other = lm(reading ~ asian + black + hispanic + white, data = ecls)
summary(lm.1.other)

lm.1.white = lm(reading ~ asian + black + hispanic + other, data = ecls)
summary(lm.1.white)



##################################################
### Adjust p-values for multiple comparisons
##################################################

p.values = c(
	0.938, # asian vs. black
	0.213, # asian vs. hispanic
	0.306, # asian vs. other
	0.326, # asian vs. white
	0.0899, # black vs. hispanic
	0.1976, # black vs. other
	0.2365, # black vs. white
	0.9674, # hispanic vs. other
	0.0000281, # hispanic vs. white
	0.0088 # other vs. white
)

p.adjust(p.values, method = "BH")
#p.adjust(p.values, method = "bonferroni")




##################################################
### Fit regression models (ANCOVA)
##################################################

lm.2.asian = lm(reading ~ black + hispanic + other + white + ses, data = ecls)
summary(lm.2.asian)

lm.2.black = lm(reading ~ asian + hispanic + other + white + ses, data = ecls)
summary(lm.2.black)

lm.2.hispanic = lm(reading ~ asian + black + other + white + ses, data = ecls)
summary(lm.2.hispanic)

lm.2.other = lm(reading ~ asian + black + hispanic + white + ses, data = ecls)
summary(lm.2.other)

lm.2.white = lm(reading ~ asian + black + hispanic + other + ses, data = ecls)
summary(lm.2.white)



##################################################
### Adjust p-values for multiple comparisons
##################################################

p.values.2 = c(
	0.479, # asian vs. black
	0.719, # asian vs. hispanic
	0.814, # asian vs. other
	0.279, # asian vs. white
	0.140, # black vs. hispanic
	0.285, # black vs. other
	0.753, # black vs. white
	0.9239, # hispanic vs. other
	0.0103, # hispanic vs. white
	0.117 # other vs. white
)

p.adjust(p.values.2, method = "BH")


