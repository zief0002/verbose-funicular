##################################################
### Read in data
##################################################

math = read.csv(file = "~/Documents/EPsy-8262/data/homework-achievement.csv")

head(math)
tail(math)



##################################################
### Load libraries
##################################################

library(ggplot2)
library(psych)
library(sm)



##################################################
### Examine outcome/response
##################################################

sm.density(math$achievement)
describe(math$achievement)



##################################################
### Examine predictor
##################################################

sm.density(math$homework)
describe(math$homework)



##################################################
### Examine conditional distribution of achievement given homework
##################################################

ggplot(data = math, aes(x = homework, y = achievement)) +
	geom_point(size = 5) +
	xlab("Time Spent on Homework") +
  ylab("Mathematics Achievement Score") +
	theme_bw()



##################################################
### Correlation
##################################################

cor(math[ , c("homework", "achievement")])



##################################################
### Regress achievement on homework
##################################################

lm.1 = lm(achievement ~ homework, data = math)
lm.1



##################################################
### Compute unexplained variation from simple regression model
##################################################

yhat = 47.03 + 1.99 * math$homework
res.1 = math$achievement - yhat
res.1

# Compute the SSE
sse.1 = sum(res.1 ^ 2)
sse.1



##################################################
### Compute unexplained variation from the intercept-only model
##################################################

# Fit intercept-only model
lm.0 = lm(achievement ~ 1, data = math)
lm.0

# Plot of the intercept-only model
ggplot(data = math, aes(x = homework, y = achievement)) +
  geom_point(size = 5) +
  geom_hline(yintercept = 51.41, color = "blue") +
  xlab("Time Spent on Homework") +
  ylab("Mathematics Achievement Score") +
  theme_bw()


# Compute SSresiduals/SSerror for intercept-only model
res.0 = math$achievement - 51.41
res.0

# Compute SSE for intercept-only (baseline) model
sse.0 = sum(res.0 ^ 2)
sse.0



##################################################
### Compute R^2
##################################################

sse.0 - sse.1

R2 = 1291.231 / sse.0
R2

# Single computation
(sse.0 - sse.1) / sse.0


