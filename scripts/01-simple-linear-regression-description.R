##################################################
### Read in data
##################################################

math = read.csv(file = "~/Documents/EPsy-8262/data/homework-achievement.csv")

head(math)
tail(math)



##################################################
### Examine outcome/response
##################################################

library(sm)
sm.density(math$achievement)

library(psych)
describe(math$achievement)



##################################################
### Examine predictor
##################################################

sm.density(math$homework)
describe(math$homework)



##################################################
### Examine conditional distribution of achievement given homework
##################################################

library(ggplot2)

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

lm.a = lm(achievement ~ homework, data = math)

lm.a



##################################################
### Compute variation to be explained in the data
##################################################

# Fit intercept-only model
lm.0 = lm(achievement ~ 1, data = math)
lm.0

# Compute SSresiduals/SSerror for intercept-only model
res.0 = math$achievement - 51.41
res.0

# Compute SSE for intercept-only (baseline) model
SSE.0 = sum(res.0 ^ 2)
SSE.0



##################################################
### Partition variation for fitted model
##################################################

# Compute SSresiduals/SSerror for fitted model
pred.a = 47.03 + 1.99 * math$homework
res.a = math$achievement - pred.a
res.a

# Compute SSE for fitted model
SSE.a = sum(res.a ^ 2)
SSE.a



##################################################
### Partitioning variation
##################################################

# Compute reduction in error from baseline to fitted model
12610.19 - 11318.96

# Partition variation
12610.19 # Total variation in data
1291.23  # Variation explained by the model
11318.96 # Variation unexplained by the model



##################################################
### Compute R^2
##################################################

(12610.19 - 11318.96) / 12610.19




