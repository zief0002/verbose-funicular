###################################################
### Read in data 
###################################################

osteo = read.csv("/Users/andrewz/Documents/EPSY-8261/data/Osteo.csv")



###################################################
### Examine effect of employment
###################################################

library(psych)
describeBy(osteo$sun, osteo$employed)


###################################################
### Make a customized plot showing the standard errors 
### and variation in the average sunlight by employment 
### status
###################################################

## Create data frame
osteo.summary = data.frame(
	employed = c("Full time", "No", "Part time"),
	M = c(0.8, 0.99, 1.27),
	SD = c(0.32, 0.68, 0.69),
	N = c(3, 20, 10),
	Min = c(0.51, 0.18, 0.11),
	Max = c(1.14, 2.96, 2.41)
	)

## Create plot
library(ggplot2)
ggplot(data = osteo.summary, aes(x = employed, y = M)) +
	geom_segment(aes(x = employed, xend = employed, y = Min, yend = Max), color = "#7a0019", alpha = 0.4) +
	geom_segment(aes(x = employed, xend = employed, y = (M + SD/sqrt(N)), yend = (M - SD/sqrt(N))), color = "#7a0019", lwd = 2, alpha = 0.4) +
	geom_point(size = 3, color = "#7a0019") +
	xlab("Employment status") +
	ylab("Exposure to direct sunlight (per week)") +
	ylim(0, 2) +
	theme_bw()





###################################################
### Fit ANOVA model
###################################################

lm.1 = lm(sun ~ employed, data = osteo)
anova(lm.1)

# Get R-squared
summary(lm.1)

# CI for R-squared
library(MBESS)
ci.R2(R2 = 0.05371, df.1 = 2, df.2 = 30)



###################################################
### Power
###################################################

# To find the critical value
qf(p = 0.95, df1 = 2, df2 = 30) 

# Compute lambda-hat
33 * 0.05371 / (1 - 0.05371)

# To find the cumulative density
pf(q = 3.316, df1 = 2, df2 = 30, ncp = 1.87303) 

# To find the cumulative density
1 - 0.8033059

# Power analysis using R
library(pwr)
pwr.anova.test(
	k = 3, 
	f = 0.2382404, 
	sig.level = 0.05, 
	power = 0.8
	)

