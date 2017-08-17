###################################################
### Load libraries
###################################################

library(broom)
library(dplyr)
library(ggplot2)
library(readr)
library(sm)



###################################################
### Read in the data
###################################################

movies = read_csv(file = "~/Dropbox/epsy-8251/data/movies.csv")
head(movies)




###################################################
### Relationship between age and budget
###################################################

ggplot(data = movies, aes(x = age, y = budget)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("Movie age") +
  ylab("Movie Budget (in millions of dollars)")



###################################################
### Relationship between mpaa and budget
###################################################

ggplot(data = movies, aes(x = mpaa, y = budget)) +
  geom_boxplot() +
  theme_bw() +
  xlab("MPAA rating") +
  ylab("Movie Budget (in millions of dollars)")




###################################################
### Fit log-linear model
###################################################

lm.1 = lm(log(budget) ~ 1 + age, data = movies)
summary(lm.1)



###################################################
### Examine residuals
###################################################

# Obtain residuals
out = augment(lm.1)

# Plot std. residuals vs. fitted values
ggplot(data = out, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw()



###################################################
### Back-transform the fitted coefficients
###################################################

exp(coef(lm.1))



###################################################
### Plot the fitted model
###################################################

# Set up data
plotData = expand.grid(
  age = 12:79
)

# Predict log-budget
plotData = plotData %>% mutate(Lbudget = predict(lm.1, newdata = plotData))

# Back-transform log-budget to budget
plotData = plotData %>% mutate(budget = exp(Lbudget))
head(plotData)

# Plot
ggplot(data = plotData, aes(x = age, y = budget)) +
  geom_line() +
  theme_bw() +
  xlab("Movie age") +
  ylab("Budget (in millions of U.S. dollars)")



###################################################
### mpaa as a predictor of log(budget)
###################################################

# Create dummy variables
movies = movies %>% mutate(pg   = ifelse(mpaa == "PG", 1, 0))
movies = movies %>% mutate(pg13 = ifelse(mpaa == "PG-13", 1, 0))
movies = movies %>% mutate(r    = ifelse(mpaa == "R", 1, 0))

# Examine data
head(movies)

# Fit model (PG is reference group)
lm.2 = lm(log(budget) ~ 1 + pg13 + r, data = movies)
summary(lm.2)

# Back-transform fitted coefficients
exp(coef(lm.2))



###################################################
### Adjust p-values for pairwise comparisons
###################################################

# Input the p-values into a vector
p = c(0.0545, 0.00000000000849, 0.0000000000000002)

# Adjust using the BH method
p.adjust(p, method = "BH")



###################################################
### Use both mpaa rating and age to predict log(budget)
###################################################

# Main-effects model
lm.3 = lm(log(budget) ~ 1 + age + pg13 + r, data = movies)
summary(lm.3)

# Fit interaction model
lm.4 = lm(log(budget) ~ 1 + age + pg13 + r + age:pg13 + age:r, data = movies)

# Test whethe interaction terms are warranted
anova(lm.3, lm.4)



###################################################
### Plot fitted main-effects model
###################################################

# Re-fit main-effects model using original mpaa predictor (categorical)
lm.4 = lm(log(budget) ~ 1 + age + mpaa, data = movies)
summary(lm.4)

# Set up data
plotData = expand.grid(
  age = 12:79,
  mpaa = c("PG", "PG-13", "R")
)

# Predict log-budget
plotData = plotData %>% mutate(Lbudget = predict(lm.4, newdata = plotData))

# Back-transform log-budget to budget
plotData = plotData %>% mutate(budget = exp(Lbudget))
head(plotData)

# Plot
ggplot(data = plotData, aes(x = age, y = budget, color = mpaa)) +
  geom_line() +
  theme_bw() +
  xlab("Movie age") +
  ylab("Budget (in millions of U.S. dollars)") +
  scale_color_brewer(name = "MPAA rating", palette = "Set1")
