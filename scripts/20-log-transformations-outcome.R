###################################################
### Read in the data
###################################################

movies = read.csv(file = "~/Documents/epsy-8251/data/movies.csv")
head(movies)


# Re-scale budget
movies$budget_millions = movies$budget / 1000000
head(movies)



###################################################
### Load libraries
###################################################

library(dplyr)
library(ggplot2)
library(sm)



###################################################
### Relationship between age and budget
###################################################

ggplot(data = movies, aes(x = age, y = budget_millions)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("Movie age") +
  ylab("Movie Budget (in millions of dollars)")



###################################################
### Log-transform Y
###################################################

movies$Lbudget_millions = log(movies$budget_millions)
head(movies)


# e
exp(1)

#e^2.77258872
exp(2.77258872)



###################################################
### Relationship between age and ln(budget)
###################################################

ggplot(data = movies, aes(x = age, y = Lbudget_millions)) +
  geom_point() +
  theme_bw() +
  xlab("Movie age") +
  ylab("ln(movie budget)")



###################################################
### Fit log-linear model
###################################################

lm.1 = lm(Lbudget_millions ~ 1 + age, data = movies)
summary(lm.1)


# Intercept
exp(3.189963)

#Slope
exp(-0.044084)


###################################################
### Understanding the fitted coefficients
###################################################

my_movies = data.frame(
  age = c(1, 2, 3)
)

# Predict log-budget
my_movies$Lbudget = predict(lm.1, newdata = my_movies)

# Displey
my_movies

# Back-transform
my_movies$budget = exp(my_movies$Lbudget)

# Display
my_movies

23.24009 * 0.95

# Using the coefficent without rounding
# 23.24009 * 0.9568736



###################################################
### Interpret using percentage difference
###################################################

exp(-0.044084) - 1



###################################################
### Plot results from earnings model
###################################################

# Summary to find values for age
summary(movies)

# Set up data
plotData = expand.grid(
  age = seq(from = 11, to = 78, by = 1)
)

# Predict
plotData$Lbudget = predict(lm.1, newdata = plotData)

# Examine data
head(plotData)

# Back-transform any log terms
plotData$budget = exp(plotData$Lbudget)

# Re-examine data
head(plotData)

# PLot
ggplot(data = plotData, aes(x = age, y = budget)) +
  geom_line() +
  theme_bw() +
  xlab("Age (in years") +
  ylab("Predicted budget")



###################################################
### Examine residuals of earning model
###################################################

out_1 = fortify(lm.1)

# Normality
sm.density(out_1$.stdresid, model = "normal")

# Normality and homogeneity of variance
ggplot(data = out_1, aes(x = .fitted, y = .stdresid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Studentized residuals")




###################################################
### Back to the MN schools data
###################################################

mn = read.csv(file = "~/Documents/EPsy-8251/data/mnSchools.csv")
mn$Lsat = log(mn$sat)
head(mn)


# Fit model
lm.2 = lm(gradRate ~ Lsat, data = mn)
summary(lm.2)


# Set up data for Lsat
my_schools = data.frame(
  Lsat = log(c(1000, 1010, 1020.1))
)

# Predict graduation rates
my_schools$gradRate = predict(lm.2, newdata = my_schools)

# Display
my_schools

# Constant difference
48.40581 - 46.87784
49.93378 - 48.40581

# Compute constant difference from slope directly
153.6 * log(1.01)

# Shortcut
153.6 / 100

