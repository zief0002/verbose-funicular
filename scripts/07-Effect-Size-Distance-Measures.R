###################################################
### Read in data
###################################################

mn = read.csv(file = "/Users/andrewz/Documents/EPSY-8251/data/MN-Colleges-2012.csv")
head(mn)



###################################################
### Prep the data
###################################################

# Remove all private-for-profit schools
mn = subset(mn, sector != "Private for-profit")
head(mn)




###################################################
### Descriptive statistics
###################################################

library(psych)
describeBy(mn$gradRate, mn$sector)

library(ggplot2)
ggplot(data = mn, aes(x = sector, y = gradRate, fill = sector)) +
	geom_boxplot() +
	scale_x_discrete(name = "Sector", labels = c("Private", "Public")) +
	ylab("Six-Year Graduation Rate") +
	scale_fill_brewer(palette = "Set3") +
	theme_bw() +
	guides(fill = FALSE)



###################################################
### Fit dummy coded regression model
###################################################

# Dummy code the sector variable
mn$public = ifelse(mn$sector == "Public", 1, 0)
head(mn)

# Fit the regression model
lm.1 = lm(gradRate ~ public, data = mn)
summary(lm.1)
anova(lm.1)



###################################################
### Cohen's d (standardized effect)
###################################################

# Load MBESS library (you may need to install it first)
library(MBESS)

# Point estimate for the standardized effect
smd(
    Mean.1 = 63.1, 
    s.1 = 19.1,
    n.1 = 24,
    Mean.2 = 50.4,
    s.2 = 10.2,
    n.2 = 11
    )



###################################################
### Confidence interval for raw effect
###################################################

confint(lm.1)



###################################################
### Confidence interval for Cohen's d (standardized effect)
###################################################

ci.smd(smd = 0.7512442, n.1 = 24, n.2 = 11)









