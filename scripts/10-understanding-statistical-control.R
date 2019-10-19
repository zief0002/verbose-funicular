##################################################
### Load libraries
##################################################

library(broom)
library(corrr)
library(dplyr)
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
### Fit the multiple regression model
##################################################

# Fit regression models
lm.1 = lm(gpa ~ 1 + homework, data = keith)
lm.2 = lm(gpa ~ 1 + homework + parent_ed, data = keith)


# Model-level information
glance(lm.1)
glance(lm.2)


# Coefficient-level information
tidy(lm.1)
tidy(lm.2)



##################################################
### Plot GPA versus time spent on homework for three parent education levels
##################################################

ggplot(data = keith, aes(x = homework, y = gpa)) +
  geom_point() +
  theme_bw() +
  xlab("Time spent on homework") +
  ylab("Model Ppredicted GPA") +
  geom_abline(intercept = 70.18, slope = 0.99) +
  geom_abline(intercept = 73.66, slope = 0.99) +
  geom_abline(intercept = 77.14, slope = 0.99)



##################################################
### Plot GPA versus time spent on homework for three parent education levels;
### Differentiate by linetype
##################################################

ggplot(data = keith, aes(x = homework, y = gpa)) +
  geom_point(alpha = 0) +
  theme_bw() +
  xlab("Time spent on homework") +
  ylab("Model predicted GPA") +
  geom_abline(intercept = 70.18, slope = 0.99, color = "#46ACC8", linetype = "dotdash") +
  geom_abline(intercept = 73.66, slope = 0.99, color = "#E58601", linetype = "solid") +
  geom_abline(intercept = 77.14, slope = 0.99, color = "#B40F20", linetype = "dashed") 



##################################################
### Triptych plot
##################################################

# Load package
library(gridExtra)

# Create plot 1
p1 = ggplot(data = keith, aes(x = homework, y = gpa)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = 70.18, slope = 0.99) +
  theme_bw() +
  xlab("Time spent on homework") +
  ylab("Model predicted GPA") +
  ggtitle("Parent Education = 8 Years")

# Create plot 2
p2 = ggplot(data = keith, aes(x = homework, y = gpa)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = 73.66, slope = 0.99) +
  theme_bw() +
  xlab("Time spent on homework") +
  ylab("Model predicted GPA") +
  ggtitle("Parent Education = 12 Years")

# Create plot 3
p3 = ggplot(data = keith, aes(x = homework, y = gpa)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = 77.14, slope = 0.99) +
  theme_bw() +
  xlab("Time spent on homework") +
  ylab("Model predicted GPA") +
  ggtitle("Parent Education = 16 Years")

# Put plots side-by-side
grid.arrange(p1, p2, p3, nrow = 1)



##################################################
### Triptych plot: Emphasis on effect of parent education level
##################################################

# Create plot 1
p4 = ggplot(data = keith, aes(x = parent_ed, y = gpa)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = 65.18, slope = 0.87) +
  theme_bw() +
  xlab("Parent education (in years)") +
  ylab("Model predicted GPA") +
  ggtitle("Time Spent on Homework = 2 Hours")

# Create plot 2
p5 = ggplot(data = keith, aes(x = parent_ed, y = gpa)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = 68.14, slope = 0.87) +
  theme_bw() +
  xlab("Parent education (in years)") +
  ylab("Model predicted GPA") +
  ggtitle("Time Spent on Homework = 5 Hours")

# Create plot 3
p6 = ggplot(data = keith, aes(x = parent_ed, y = gpa)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = 73.08, slope = 0.87) +
  theme_bw() +
  xlab("Parent education (in years)") +
  ylab("Model predicted GPA") +
  ggtitle("Time Spent on Homework = 10 Hours")

# Put plots side-by-side
grid.arrange(p4, p5, p6, nrow = 1)



##################################################
### Plot displaying a single effect (controlling for parent education level)
##################################################

ggplot(data = keith, aes(x = homework, y = gpa)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = 75.43, slope = 0.99) +
  theme_bw() +
  xlab("Time spent on homework") +
  ylab("Model predicted GPA")

