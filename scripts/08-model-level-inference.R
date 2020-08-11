##################################################
### Load libraries
##################################################

library(broom) #v0.7.0.9 or higher
library(corrr)
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

keith = read_csv(file = "https://raw.githubusercontent.com/zief0002/modeling/master/data/keith-gpa.csv")
head(keith)



##################################################
### Fit regression model
##################################################

lm.a = lm(gpa ~ 1 + homework, data = keith)



##################################################
### Model-level output
##################################################

glance(lm.a)



##################################################
### ANOVA decomposition
##################################################

anova(lm.a)



##################################################
### Understanding the ANOVA decomposition
##################################################

# Variance of outcome variable
keith %>%
  summarize(V_gpa = var(gpa))


# Fit intercept-only model
lm.0 = lm(gpa ~ 1, data = keith)


# ANOVA decomposition
anova(lm.0)



##################################################
### Plot of the fitted model and the model uncertainty
##################################################

ggplot(data = keith, aes(x = homework, y = gpa)) +
  geom_smooth(
    method = "lm", 
    color = "#c62f4b", 
    fill = "#696969"
    ) +
  xlab("Time spent on homework") +
  ylab("GPA (on a 100-pt. scale)") +
  theme_bw()

