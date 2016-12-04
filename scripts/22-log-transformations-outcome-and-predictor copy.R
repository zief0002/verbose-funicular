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
### Exploration
###################################################

sm.density(movies$budget_millions)
sm.density(movies$age)



###################################################
### Log-transform X and Y
###################################################

movies$Lbudget_millions = log(movies$budget_millions)
movies$Lage = log(movies$age)
head(movies)



###################################################
### Relationship between ln(age) and ln(budget)
###################################################

ggplot(data = movies, aes(x = Lage, y = Lbudget_millions)) +
  geom_point() +
  theme_bw() +
  xlab("ln(age") +
  ylab("ln(budget)")



###################################################
### Fit log-linear model
###################################################

lm.1 = lm(Lbudget_millions ~ 1 + Lage, data = movies)
summary(lm.1)


# Intercept
exp(0)  # Back-transform Lage = 0
exp(4.8395)  # Back-transform intercept coefficient

#Slope
-0.8584 / 100

exp(-0.008584) - 1



###################################################
### Understanding the fitted coefficients
###################################################

my_movies = data.frame(
  age = c(10, 10.1, 10.201)
) %>%
  mutate(Lage = log(age)) 

# Predict
my_movies$Lbudget = predict(lm.1, newdata = my_movies)

# Back-transform to get budget
my_movies$budget = exp(my_movies$Lbudget)

# Display
my_movies

17.36604 / 17.51500
17.21835/ 17.36604



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
  geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Studentized residuals")



###################################################
### Include covariates: Main-Effect
###################################################

lm.2 = lm(Lbudget_millions ~ 1 + Lage + mpaa, data = movies)
summary(lm.2)


# Intercept
exp(4.9362)

# Age
exp(-0.7338/100) - 1

# PG-13
exp(0.2312)

# R
exp(-0.8688)



###################################################
### Plot the model
###################################################

# Examine ranges
summary(movies)

# Set up data
plotData = expand.grid(
  Lage = seq(from = 2.398, to = 4.357, by = 0.001),
  mpaa = c("PG", "PG-13", "R")
)

# Predict
plotData$Lbudget = predict(lm.2, newdata = plotData)

# Back-transform any log terms
plotData$age = exp(plotData$Lage)
plotData$budget = exp(plotData$Lbudget)

# Examine data
head(plotData)

# Plot
ggplot(data = plotData, aes(x = age, y = budget, group = mpaa, color = mpaa)) +
  geom_line() +
  theme_bw() +
  xlab("Age (in years)") +
  ylab("Predicted budget") +
  scale_color_brewer(name = "", palette = "Set1")



###################################################
### Include covariates: Interaction
###################################################

lm.3 = lm(Lbudget_millions ~ 1 + Lage + mpaa + Lage:mpaa, data = movies)

# Test the three interaction effects simultaneously
anova(lm.2, lm.3)



###################################################
### Plot the model
###################################################

# Set up data
plotData = expand.grid(
  Lage = seq(from = 2.398, to = 4.357, by = 0.001),
  mpaa = c("PG", "PG-13", "R")
)

# Predict
plotData$Lbudget = predict(lm.3, newdata = plotData)

# Back-transform any log terms
plotData$age = exp(plotData$Lage)
plotData$budget = exp(plotData$Lbudget)

# Plot
ggplot(data = plotData, aes(x = age, y = budget, group = mpaa, color = mpaa)) +
  geom_line() +
  theme_bw() +
  xlab("Age (in years)") +
  ylab("Predicted budget") +
  scale_color_brewer(name = "", palette = "Set1")
