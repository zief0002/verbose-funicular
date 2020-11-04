##################################################
### Load libraries
##################################################

library(broom)
library(corrr)
library(dplyr)
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
### Fit the multiple regression model
##################################################

# Fit regression models
lm.a = lm(gpa ~ 1 + homework, data = keith)
lm.b = lm(gpa ~ 1 + homework + parent_ed, data = keith)


# Model-level information
glance(lm.a)
glance(lm.b)


# Coefficient-level information
tidy(lm.a)
tidy(lm.b)



##################################################
### Plot GPA versus time spent on homework for three parent education levels
##################################################

ggplot(data = keith, aes(x = homework, y = gpa)) +
  geom_point(alpha = 0.1) +
  theme_bw() +
  xlab("Time spent on homework") +
  ylab("Model predicted GPA") +
  geom_abline(intercept = 70.18, slope = 0.99, color = "#003f5c", linetype = "dotdash") +
  geom_abline(intercept = 73.66, slope = 0.99, color = "#f26419", linetype = "solid") +
  geom_abline(intercept = 77.14, slope = 0.99, color = "#b40f20", linetype = "dashed") 



##################################################
### Show variation in model predicted GPA for fixed HW = 6
##################################################

ggplot(data = keith, aes(x = homework, y = gpa)) +
  geom_point(alpha = 0) +
  theme_bw() +
  xlab("Time spent on homework") +
  ylab("Model predicted GPA") +
  geom_abline(intercept = 70.18, slope = 0.99, color = "#003f5c", linetype = "dotdash") +
  geom_abline(intercept = 73.66, slope = 0.99, color = "#f26419", linetype = "solid") +
  geom_abline(intercept = 77.14, slope = 0.99, color = "#b40f20", linetype = "dashed") +
  geom_point(x = 6, y = 76.11908, color = "#003f5c", size = 2) +
  geom_point(x = 6, y = 79.60157, color = "#f26419", size = 2) +
  geom_point(x = 6, y = 83.08407, color = "#b40f20", size = 2)



##################################################
### Triptych plot
##################################################

# Load package
library(patchwork)


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
p1 | p2 | p3



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
p4 | p5 |p6



##################################################
### Plot displaying a single effect (mean parent education level)
##################################################

ggplot(data = keith, aes(x = homework, y = gpa)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = 75.43, slope = 0.99) +
  theme_bw() +
  xlab("Time spent on homework") +
  ylab("Model predicted GPA")

