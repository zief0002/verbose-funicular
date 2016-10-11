##################################################
### Read in data
##################################################

city = read.csv(file = "~/Documents/data/Applied-Regression-Lewis-Beck/riverside_final.csv") 

head(city)
tail(city)



###################################################
### Load all needed libraries 
###################################################

library(dplyr)
library(ggplot2)
library(sm)



##################################################
### Fit the simple regression model
##################################################

lm.1 = lm(income ~ 1 + edu, data = city)
summary(lm.1)



##################################################
### Examine linearity assumption
##################################################

ggplot(data = city, aes(x = edu, y = income)) +
	geom_point(size = 5) +
	xlab("Education level") +
  ylab("Income") +
	theme_bw() +
	geom_smooth(method = "lm", se = FALSE)



##################################################
### Fortify the model
##################################################

out_1 = fortify(lm.1)
head(out_1)



##################################################
### Raw residual plots
##################################################

# Residuals vs. predictor
ggplot(data = out_1, aes(x = edu, y = .resid)) +
    geom_point(size = 5)  +
    geom_hline(yintercept = 0) +
    theme_bw() +
    xlab("Education level") +
    ylab("Residuals")


# Density plot of the residuals
sm.density(out_1$.resid)


# Density plot of the residuals with error bands
sm.density(out_1$.resid, model = "normal")



##################################################
### Residual plots with standardized residuals
##################################################

# Standardized residuals vs. predictor
ggplot(data = out_1, aes(x = edu, y = .stdresid)) +
  geom_point(size = 5)  +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Education level") +
  ylab("Standardized residuals")


# Density plot of the residuals with error bands
sm.density(out_1$.stdresid, model = "normal", xlab = "Standardized residuals")



##################################################
### Correlations
##################################################

# Correlation between predictor and outcome
cor(city[ , c("edu", "income")])

# Correlation between fitted values and outcome
cor(out_1[ , c(".fitted", "income")])

# Correlation between fitted values and residuals
cor(out_1[ , c(".fitted", ".resid")])



##################################################
### Multiple regression assumptions
##################################################

# Fit the model
lm.2 = lm(income ~ 1 + edu + senior, data = city)
summary(lm.2)


# Fortify the data
out_2 = fortify(lm.2)
head(out_2)


# Standardized residuals vs fitted values (y-hats) 
ggplot(data = out_2, aes(x = .fitted, y = .stdresid)) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Standardized residuals")
    


### If the plot of residuals vs. fitted values does not suggest that the assumptions are met,
### Look at the residuals vs. each of the predictors to find the issue.


# Standardized residuals vs education level
ggplot(data = out_a, aes(x = edu, y = .stdresid)) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Education level") +
  ylab("Standardized residuals")


# Standardized residuals vs seniority
ggplot(data = out_2, aes(x = senior, y = .stdresid)) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Seniority") +
  ylab("Standardized residuals")


# Marginal plot of the standardized residuals with error bands
sm.density(out_2$.stdresid, model = "normal", xlab = "Standardized residuals")



##################################################
### Standardized residuals vs. fitted values 
### Indicate 'extreme' values by showing observation number
##################################################

# Add an ID number to the fortified data
out_2 = out_2 %>% mutate( id2 = 1:nrow(out_2) )
head(out_2)


# Plot
ggplot(data = out_2, aes(x = .fitted, y = .stdresid)) +
  geom_text(aes(label = id), size = 4, hjust = 0, vjust = 0) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), lty = "dashed") +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Standardized residuals")



##################################################
### Find these observations with large residuals
##################################################

# Get observations with standardized residual >= 2
out_2 %>% filter(.stdresid >= 2)

# Get observations with standardized residual <= -2
out_2 %>% filter(.stdresid <= -2)

# Arrange all of the observations according to their standardized residuals
arrange(out_2, .stdresid)



##################################################
### Correlation between the fitted values and the residuals
##################################################

cor(out_1$.fitted, out_1$.resid)
cor(out_2$.fitted, out_2$.resid)



##################################################
### Correlations
##################################################

# Correlation between fitted values and outcome
cor(out_2[ , c(".fitted", "income")])

# Multiple R^2
cor(out_2[ , c(".fitted", "income")]) ^ 2
summary(lm.2)


# Correlation between fitted values and residuals
cor(out_2[ , c(".fitted", ".resid")])





