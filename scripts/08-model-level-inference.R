##################################################
### Load libraries
##################################################

library(broom)
library(dplyr)
library(ggplot2)
library(readr)



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
### Fit regression model
##################################################

lm.1 = lm(gpa ~ 1 + homework, data = keith)



##################################################
### Model-level output
##################################################

glance(lm.1)



##################################################
### ANOVA decomposition
##################################################

anova(lm.1)



##################################################
### Plot of the fitted model and the model uncertainty
##################################################

# Install educate package (only need to do this once)
# library(devtools)
# install_github("zief0002/educate")

# Load educate package
library(educate)

# Create plot with regression line and confidence envelope
ggplot(data = keith, aes(x = homework, y = gpa)) +
  stat_watercolor_smooth(method = "lm") +
  geom_abline(intercept = 74.3, slope = 1.21) +
  xlab("Time spent on homework") +
  ylab("GPA (on a 100-pt. scale)") +
  theme_bw()

