###################################################
### Load all needed libraries 
###################################################

library(broom)
library(dplyr)
library(ggplot2)
library(readr)
library(sm)



##################################################
### Read in data
##################################################

city = read_csv(file = "~/Dropbox/epsy-8251/data/riverside.csv") 

head(city)
tail(city)



##################################################
### Fit the simple regression model
##################################################

lm.1 = lm(income ~ 1 + education, data = city)
summary(lm.1)



##################################################
### Augment the model
##################################################

out_1 = augment(lm.1)
head(out_1)



##################################################
### Raw residual plots
##################################################

# Residuals vs. predictor
ggplot(data = out_1, aes(x = education, y = .resid)) +
  geom_point(size = 5)  +
  theme_bw() +
  geom_hline(yintercept = 0) +
  xlab("Education level") +
  ylab("Residuals")


# Density plot of the residuals
sm.density(out_1$.resid, xlab = "Residuals", model = "normal")


# Density plot of the residuals with error bands
sm.density(out_1$.resid, model = "normal", xlab = "Residuals")



##################################################
### Residual plots with studentized residuals
##################################################

# Studentized residuals vs. predictor
ggplot(data = out_1, aes(x = education, y = .std.resid)) +
  geom_point(size = 5) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  xlab("Education level") +
  ylab("Studentized residuals")


# Density plot of the residuals with error bands
sm.density(out_1$.std.resid, xlab = "Studentized residuals", model = "normal")



##################################################
### Find these observations with large residuals
##################################################

# Get observations with standardized residual >= 2
out_1 %>% filter(.std.resid <= -2 | .std.resid >= 2)


# Arrange all of the observations according to their standardized residuals
out_1 %>% arrange(.std.resid)



##################################################
### Multiple regression assumptions
##################################################

# Fit the model
lm.2 = lm(income ~ 1 + education + seniority, data = city)


# Augment the model
out_2 = augment(lm.2)
head(out_2)


# Studentized residuals vs fitted values
ggplot(data = out_2, aes(x = education, y = .std.resid)) + 
  geom_point() +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = -2, linetype = "dotted") + 
  geom_hline(yintercept = 2, linetype = "dotted") + 
  xlab("Education level") +
  ylab("Studentized residuals")
    

# Density plot of the studentized residuals with error bands
sm.density(out_2$.std.resid, xlab = "Studentized residuals", model = "normal")



##################################################
### Advanced plotting: Loess smooth to help evaluate linearity
##################################################

ggplot(data = out_2, aes(x = .fitted, y = .std.resid)) + 
  geom_point() +
  geom_smooth(method = "loess") +
  theme_bw() +
  geom_hline(yintercept = 0) + 
  xlab("Fitted values") + 
  ylab("Studentized residuals")



##################################################
### Advanced plotting: Identify observations with extreme residuals
##################################################

# Add an ID number to the augmented data for lm.1
out_1 = out_1 %>% mutate( id = 1:nrow(out_1) ) 
head(out_1)


# Plot the id variable as text rather than points in the scatterplot
ggplot(data = out_1, aes(x = .fitted, y = .std.resid)) + 
  geom_text(aes(label = id), size = 4) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = -2, linetype = "dotted") + 
  geom_hline(yintercept = 2, linetype = "dotted") + 
  xlab("Fitted values") + 
  ylab("Studentized residuals")



##################################################
### Advanced plotting: Identify observations with extreme residuals
### Different plotting for extreme and non-extreme observations
##################################################

# Put extreme and non-extreme variables in different data sets
extreme = out_1 %>% filter(.std.resid <= -2 | .std.resid >= 2) 
nonextreme = out_1 %>% filter(.std.resid > -2 & .std.resid < 2)


ggplot(data = extreme, aes(x = .fitted, y = .std.resid)) + 
  geom_text(aes(label = id), size = 4, color = "red") +
  geom_point(data = nonextreme) +
  theme_bw() +
  geom_hline(yintercept = 0) + 
  geom_hline(yintercept = -2, linetype = "dotted") + 
  geom_hline(yintercept = 2, linetype = "dotted") + 
  xlab("Fitted values") + 
  ylab("Studentized residuals")


