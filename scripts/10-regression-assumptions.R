##################################################
### Read in data
##################################################

math = read.csv(file = "~/Documents/EPsy-8262/data/homework-achievement.csv")

head(math)
tail(math)


lm.a = lm(achievement ~ homework, data = math)



##################################################
### Examine linearity assumption
##################################################

library(ggplot2)

ggplot(data = math, aes(x = homework, y = achievement)) +
	geom_point(size = 5) +
	xlab("Time Spent on Homework") +
    ylab("Mathematics Achievement Score") +
	theme_bw() +
	geom_smooth(method = "lm", se = FALSE)



##################################################
### Fortify the model
##################################################

out_a = fortify(lm.a)
head(out_a)



##################################################
### Residual plot
##################################################

library(ggplot)

ggplot(data = out_a, aes(x = homework, y = .stdresid)) +
    geom_point(size = 5) +
    theme_bw() +
    geom_hline(yintercept = 0)



##################################################
### Plot the marginal distribution of the errors (normality)
##################################################

library(sm)
sm.density(out_a$.resid)

# With error bands
sm.density(out_a$.resid, model = "normal")



##################################################
### Multiple regression assumptions
##################################################

# Read in the data
multReg = read.csv(file = "~/Documents/EPsy-8262/data/homework-education-gpa.csv")

# Fit the model
lm.b = lm(gpa ~ homework + parentEd, data = multReg)
summary(lm.b)

# Fortify the data
out_b = fortify(lm.b)
head(out_b)

# Residual plot
ggplot(data = out_a, aes(x = .fitted, y = .stdresid)) +
    geom_point(size = 4) +
    theme_bw() +
    geom_hline(yintercept = 0)

# Marginal plot of the residuals with error bands
sm.density(out_b$.resid, model = "normal")



##################################################
### Plots using the standardized residuals
##################################################

ggplot(data = out_b, aes(x = .fitted, y = .stdresid)) +
    geom_point(size = 4) +
    theme_bw() +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = c(-2, 2), lty = "dashed")


sm.density(out_b$.stdresid, model = "normal")



##################################################
### Residual showing observation number
##################################################

ggplot(data = out_b, aes(x = .fitted, y = .stdresid)) +
    geom_text(aes(label = id), size = 4, hjust = 0, vjust = 0) +
    theme_bw() +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = c(-2, 2), lty = "dashed")



##################################################
### Find these observations with large residuals
##################################################

library(dplyr)

# Get observations with standardized residual >= 2
out_b %>% filter(.stdresid >= 2)

# Get observations with standardized residual <= -2
out_b %>% filter(.stdresid <= -2)

# Arrange all of the observations according to their standardized residuals
arrange(out_b, .stdresid)


##################################################
### Add an ID variable to the fortified data
##################################################

out_b$id = 1:100
head(out_b)

out_b %>% filter(.stdresid >= 2)
out_b %>% filter(.stdresid <= -2)




##################################################
### Add an ID variable to the original data, remove case 34, and re-fit model
##################################################

multReg$id = 1:100
head(multReg)

# Check that row 34 is the problematic observation
multReg[34, ]

# Create a new data frame that removes row 34
multReg2 = multReg[-c(34), ]
multReg2

# Fit the model
lm.c = lm(gpa ~ homework + parentEd, data = multReg2)
summary(lm.c)

# Fortify the data
out_c = fortify(lm.c)
head(out_c)

# Add ID variable into fortiied data
out_c$id = multReg2$id
head(out_c)

# Density plot of the marginal standardized residuals
sm.density(out_c$.stdresid, model = "normal")

# Residual plot
ggplot(data = out_c, aes(x = .fitted, y = .stdresid)) +
	#geom_point() +
    geom_text(aes(label = id), size = 4, hjust = 0, vjust = 0) +
    theme_bw() +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = c(-2, 2), lty = "dashed")



##################################################
### Correlation between the fitted values and the residuals
##################################################

cor(out_a$.fitted, out_a$.resid)
cor(out_b$.fitted, out_b$.resid)



##################################################
### Correlation between the fitted values and the outcome
##################################################

cor(out_a$.fitted, out_a$achievement)
cor(math$homework, out_a$achievement)


cor(out_b$.fitted, out_b$gpa)




