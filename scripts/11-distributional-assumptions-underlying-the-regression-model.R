###################################################
### Load all needed libraries 
###################################################

library(broom)
library(corrr)
library(dplyr)
library(educate)
library(ggplot2)
library(patchwork)
library(readr)



##################################################
### Set options so up to 6 significant digits print
##################################################

options(pillar.sigfig = 6)



##################################################
### Read in data
##################################################

keith = read_csv(file = "https://raw.githubusercontent.com/zief0002/modeling/master/data/keith-gpa.csv")
head(keith)



##################################################
### Fit the simple regression model
##################################################

lm.a = lm(gpa ~ 1 + homework, data = keith)



##################################################
### Augment the model
##################################################

# Augment the model to get residuals
aug_a = augment(lm.a)


# View augmented data
head(aug_a)



##################################################
### Raw residual plots
##################################################

# Density plot of the residuals
ggplot(data = aug_a, aes(x = .resid)) +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Residual") +
  ylab("Probability density")


# Scatterplot of the residuals versus X
ggplot(data = aug_a, aes(x = homework, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Time spent on homework (in hours)") +
  ylab("Residual")



##################################################
### Standardized residual plots
##################################################

# Density plot of the residuals
ggplot(data = aug_a, aes(x = .std.resid)) +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Standardized residual") +
  ylab("Probability density")


# Scatterplot of the residuals versus X
ggplot(data = aug_a, aes(x = homework, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Time spent on homework (in hours)") +
  ylab("Standardized residual")



##################################################
### Find these observations with large residuals
##################################################

# Get observations with standardized residual >= 2
aug_a %>% 
  filter(.std.resid <= -2 | .std.resid >= 2)


# Arrange all of the observations according to their standardized residuals
out_1 %>% 
  arrange(.std.resid)



##################################################
### Multiple regression assumptions
##################################################

# Fit the multiple regression model
lm.b = lm(gpa ~ 1 + homework + parent_ed, data = keith)


# Augment the model to obtain the fitted values and residuals
aug_b = augment(lm.b)
head(aug_b)


# Density plot of the standardized residuals
ggplot(data = aug_b, aes(x = .std.resid)) +
  stat_density(geom = "line", color = "#c62f4b") +
  theme_bw() +
  xlab("Standardized residual") +
  ylab("Probability density")


# Plot the standardized residuals versus the fitted values
ggplot(data = aug_b, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Standardized residual")



##################################################
### Advanced plotting: Accounting for sampling uncertainty in the density plot
##################################################

# Density plot of the standardized residuals (compare with normal distribution)
ggplot(data = aug_b, aes(x = .std.resid)) +
  stat_density(geom = "line", color = "#c62f4b") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), color = "black", linetype = "dashed") +
  theme_bw() +
  xlab("Standardized residual") +
  ylab("Probability density")


# Density plot of the standardized residuals (uncertainty envelope on normal dist.)
ggplot(data = aug_b, aes(x = .std.resid)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line", color = "#c62f4b") +
  theme_bw() +
  xlab("Standardized residual") +
  ylab("Probability density")



##################################################
### Advanced Plotting: Loess smooth to help evaluate linearity
##################################################

ggplot(data = aug_b, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_bw() +
  geom_hline(yintercept = 0) +
  xlab("Fitted values") +
  ylab("Standardized residual")



##################################################
### Advanced plotting: Identify observations with extreme residuals
##################################################

# Create ID variable in the augmented data
aug_b = aug_b %>% 
  mutate(id = row.names(keith))


# View new data
head(aug_b)


# Plot the id variable as text rather than points in the scatterplot
ggplot(data = aug_b, aes(x = .fitted, y = .std.resid)) +
  geom_text(aes(label = id), size = 4) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = -2, linetype = "dotted") +
  geom_hline(yintercept = 2, linetype = "dotted") +
  xlab("Fitted values") +
  ylab("Standardized residuals")



##################################################
### Advanced plotting: Identify observations with extreme residuals
### Only call out extreme residuals
##################################################

# Create different data sets for the extreme and non-extreme observations
extreme = aug_b %>% 
  filter(.std.resid <= -2 | .std.resid >= 2)

nonextreme = aug_b %>% 
  filter(.std.resid > -2 & .std.resid < 2)


# Plot using text for the extreme observations and points for the non-extreme
ggplot(data = extreme, aes(x = .fitted, y = .std.resid)) +
  geom_text(aes(label = id), size = 4, color = "red") +
  geom_point(data = nonextreme) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  xlab("Fitted values") +
  ylab("Standardized residual")



