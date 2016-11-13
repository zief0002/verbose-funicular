###################################################
### Read in the data
###################################################

movies = read.csv(file = "~/Documents/epsy-8251/data/movies.csv")
head(movies)





##################################################
### Load libraries
##################################################

library(dplyr)
library(ggplot2)
library(sm)



##################################################
### Examine data
##################################################

summary(movies)


# Re-scale budget
movies$budget_millions = movies$budget / 1000000
head(movies)



##################################################
### Examine effect of age
##################################################

ggplot(data = movies, aes(x = age, y = budget_millions)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  xlab("Movie age") +
  ylab("Movie Budget (in millions of dollars)")



##################################################
### Fit main-effects model (age only)
##################################################

lm.1 = lm(budget_millions ~ 1 + age, data = movies)
summary(lm.1)



##################################################
### Fit main-effects model (age and MPAA rating)
##################################################

# Create dummy variables for MPAA effect
movies$pg = ifelse(movies$mpaa == "PG", 1, 0)
movies$pg13 = ifelse(movies$mpaa == "PG-13", 1, 0)
movies$r = ifelse(movies$mpaa == "R", 1, 0)

head(movies)


# Fit model
lm.2 = lm(budget_millions ~ 1 + age + pg + pg13, data = movies)
summary(lm.2)



##################################################
### Testing all three MPAA effects simultaneously
##################################################

# Fit nested models
lm.1 = lm(budget_millions ~ 1 + age, data = movies)
lm.2 = lm(budget_millions ~ 1 + age + pg + pg13, data = movies)


# Test effects
anova(lm.1, lm.2)



##################################################
### Explore data for differential effects between age and MPAA rating
##################################################

ggplot(data = movies, aes(x = age, y = budget_millions, color = mpaa)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  xlab("Movie age") +
  ylab("Movie budget (in millions of dollars)") +
  scale_color_brewer(name = "MPAA Rating", palette = "Set1") +
  facet_wrap(~mpaa)



##################################################
### Fit interaction model
##################################################

# Create interaction terms
movies$pg_age   = movies$pg   * movies$age
movies$pg13_age = movies$pg13 * movies$age
movies$r_age    = movies$r    * movies$age

head(movies)


# Fit interaction model
lm.3 = lm(budget_millions ~ 1 + age + pg + pg13 + pg_age + pg13_age, data = movies)

# Test whether interaction terms are necessary
anova(lm.2, lm.3)

# Examine model output
summary(lm.2)



##################################################
### Alternative model language in lm()
##################################################

# Fit main-effects model
lm.2 = lm(budget_millions ~ 1 + age + mpaa, data = movies)
summary(lm.2)


# Make R rated movies the reference group
lm.2_r = lm(budget_millions ~ 1 + age + relevel(mpaa, ref = "R"), data = movies)
summary(lm.2_r)


# Fit interaction model
lm.3 = lm(budget_millions ~ 1 + age + mpaa + age:mpaa, data = movies)
summary(lm.3)


# Make R rated movies the reference group
lm.3_r = lm(budget_millions ~ 1 + age + relevel(mpaa, ref = "R") + age: relevel(mpaa, ref = "R"), data = movies)
summary(lm.3_r)



##################################################
### Plot interaction model results
##################################################

# Fit interaction model using alternative modeling lnguage
lm.3 = lm(budget_millions ~ 1 + age + mpaa + age:mpaa, data = movies)


# Create new data set with main effects
plotData = expand.grid(
  age = seq(from = 11, to = 78, by = 1),
  mpaa = c("PG", "PG-13", "R")
)

head(plotData)


# Use fitted model to compute fitted values for the data
plotData$yhat = predict(lm.3, newdata = plotData)
head(plotData)


# Plot the fitted model
ggplot(data = plotData, aes(x = age, y = yhat, color = mpaa)) +
  geom_line() +
  theme_bw() +
  xlab("Movie age") +
  ylab("Predicted Budget") +
  scale_color_brewer(name = "MPAA rating", palette = "Set2")



##################################################
### Examine model assumptions
##################################################

# Create fortified data
fort_lm3 = fortify(lm.3)
head(fort_lm3)


# Examine normality assumption
sm.density(fort_lm3$.stdresid, model = "normal", xlab = "Studentized Residuals")


# Examine other assumptions
ggplot(data = fort_lm3, aes(x = .fitted, y = .stdresid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-3, 3), linetype = "dotted") +
  geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("Fitted Values") +
  ylab("Studentized Residuals")
