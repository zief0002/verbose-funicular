###################################################
### Load all needed libraries 
###################################################

library(broom)
library(corrr)
library(dplyr)
library(educate)
library(ggplot2)
library(readr)
library(tidyr)



##################################################
### Set options so up to 6 significant digits print
##################################################

options(pillar.sigfig = 6)



##################################################
### Read in data
##################################################

keith = read_csv(file = "~/Documents/github/epsy-8251/data/keith-gpa.csv")

head(keith)



##################################################
### Fit the simple regression model
##################################################

lm.1 = lm(gpa ~ 1 + homework, data = keith)

glance(lm.1)
tidy(lm.1)



##################################################
### Augment the model
##################################################

# Augment the model to get residuals
out_1 = augment(lm.1)

# View augmented data
head(out_1)



##################################################
### Raw residual plots
##################################################

# Residuals vs. predictor (Linearity, homoskedasticity)
ggplot(data = out_1, aes(x = homework, y = .resid)) +
  geom_point() +
  theme_bw() +
  geom_hline(yintercept = 0) +
  xlab("Time spent on homework (in hours)") +
  ylab("Residuals")


# Density plot of the residuals (normality)
ggplot(data = out_1, aes(x = .resid)) +
  stat_watercolor_density(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Residuals") +
  ylab("Probability density")



##################################################
### Residual plots with studentized residuals
##################################################

# Plot the studentized residuals versus time spent on homework
ggplot(data = out_1, aes(x = homework, y = .std.resid)) +
  geom_point() +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed") +
  xlab("Time spent on homework (in hours)") +
  ylab("Studentized residuals")


# Density plot of the studentized residuals
ggplot(data = out_1, aes(x = .std.resid)) +
  stat_watercolor_density(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Studentized residuals") +
  ylab("Probability density")



##################################################
### Find these observations with large residuals
##################################################

# Get observations with standardized residual >= 2
out_1 %>% 
  filter(.std.resid <= -2 | .std.resid >= 2)


# Arrange all of the observations according to their standardized residuals
out_1 %>% 
  arrange(.std.resid)



##################################################
### Multiple regression assumptions
##################################################

# Fit the model
lm.2 = lm(gpa ~ 1 + homework + parent_ed, data = keith)


# Augment the model
out_2 = augment(lm.2)
head(out_2)


# Plot the studentized residuals versus the fitted values
ggplot(data = out_2, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Studentized residuals")


# Density plot of the studentized residuals
ggplot(data = out_2, aes(x = .std.resid)) +
  stat_watercolor_density(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Studentized residuals") +
  ylab("Probability density")



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

# Create ID variable in the augmented data
out_2 = out_2 %>% 
  mutate(id = row.names(keith))

head(out_2)


# Plot the id variable as text rather than points in the scatterplot
ggplot(data = out_2, aes(x = .fitted, y = .std.resid)) +
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

# Create different data sets for the extreme and non-extreme observations
extreme = out_2 %>% 
  filter(.std.resid <= -2 | .std.resid >= 2)

nonextreme = out_2 %>% 
  filter(.std.resid > -2 & .std.resid < 2)


# Plot using text for the extreme observations and points for the non-extreme
ggplot(data = extreme, aes(x = .fitted, y = .std.resid)) +
  geom_text(aes(label = id), size = 4, color = "red") +
  geom_point(data = nonextreme) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = -2, linetype = "dotted") +
  geom_hline(yintercept = 2, linetype = "dotted") +
  xlab("Fitted values") +
  ylab("Studentized residuals")


