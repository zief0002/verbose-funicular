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
### Relationship between MPAA rating and budget
###################################################

ggplot(data = movies, aes(x = mpaa, y = budget)) +
  geom_boxplot() +
  theme_bw() +
  xlab("MPAA rating") +
  ylab("Movie Budget (in millions of dollars)")


# Compute summary statistics
movies %>%
  group_by(mpaa) %>%
  summarize(M = mean(budget), SD = sd(budget))



###################################################
### Relationship between mpaa and budget
###################################################

ggplot(data = movies, aes(x = mpaa, y = budget)) +
  geom_boxplot() +
  theme_bw() +
  xlab("MPAA rating") +
  ylab("Movie Budget (in millions of dollars)")




###################################################
### Fit linear model
###################################################

# Fit linear model
lm.1 = lm(budget ~ 1 + mpaa, data = movies)

# Obtain residuals
out = augment(lm.1)

# Plot std. residuals vs. fitted values
ggplot(data = out, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw()

# Condition on rating
PG = out %>% filter(mpaa == "PG")
PG13 = out %>% filter(mpaa == "PG-13")
R = out %>% filter(mpaa == "R")

# Density plots
par(mfrow = c(1, 3))
sm.density(PG$.std.resid, model = "Normal", main = "PG")
sm.density(PG13$.std.resid, model = "Normal", main = "PG")
sm.density(R$.std.resid, model = "Normal", main = "PG")
par(mfrow = c(1, 1))



###################################################
### Log-transform budget
###################################################

movies = movies %>% mutate(Lbudget = log(budget))
head(movies)


# 'Til There Was You budget --> log(budget)
log(23)
exp(3.13549422)



###################################################
### Summary of relationship between MPAA rating and log-budget
###################################################

# Plot the conditional distributions
ggplot(data = movies, aes(x = mpaa, y = Lbudget)) +
  geom_point() +
  theme_bw() +
  xlab("Movie age") +
  ylab("ln(Movie Budget)")


# Compute summary statistics
movies %>%
  group_by(mpaa) %>%
  summarize(M = mean(Lbudget), SD = sd(Lbudget))



###################################################
### Fit log-linear model
###################################################

# Fit linear model
lm.2 = lm(Lbudget ~ 1 + mpaa, data = movies)

# Obtain residuals
out2 = augment(lm.2)

# Plot std. residuals vs. fitted values
ggplot(data = out2, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw()

# Obtain each mpaa ratings residuals
PG2 = out2 %>% filter(mpaa == "PG")
PG132 = out2 %>% filter(mpaa == "PG-13")
R2 = out2 %>% filter(mpaa == "R")

# Density plots
par(mfrow = c(1, 3))
sm.density(PG2$.std.resid, model = "Normal", main = "PG")
sm.density(PG132$.std.resid, model = "Normal", main = "PG")
sm.density(R2$.std.resid, model = "Normal", main = "PG")
par(mfrow = c(1, 1))



###################################################
### Model summary
###################################################

summary(lm.2)

# Back-transform fitted coefficients
exp(coef(lm.2))



###################################################
### Relationship with age
###################################################

# Relationship with raw-budget
p1 = ggplot(data = movies, aes(x = age, y = budget)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("Movie age") +
  ylab("Movie Budget (in millions of dollars)") +
  ggtitle("Raw-Budget")

# Relationship with log-budget
p2 = ggplot(data = movies, aes(x = age, y = Lbudget)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("Movie age") +
  ylab("Ln(Movie Budget)") +
  ggtitle("Log-Budget")

gridExtra::grid.arrange(p1, p2, nrow = 1)



###################################################
### Model with age
###################################################

lm.3 = lm(Lbudget ~ 1 + age, data = movies)
summary(lm.3)

exp(coef(lm.3))



###################################################
### Main-effects model
###################################################

lm.4 = lm(Lbudget ~ 1 + age + mpaa, data = movies)

anova(lm.3, lm.4)

summary(lm.4)

exp(coef(lm.4))



###################################################
### Main-effects model (PG-13 as reference group)
###################################################

# Create dummy variables
movies = movies %>%
  mutate(
    PG = if_else(mpaa == "PG", 1, 0),
    R  = if_else(mpaa == "R",  1, 0)
  )

# Fit model (PG-13 is reference group)
lm.5 = lm(Lbudget ~ 1 + age + PG + R, data = movies)
summary(lm.5)

# Exponentiate the coefficients
exp(coef(lm.5))



###################################################
### p-value adjustment
###################################################

# Vector of unadjusted p-values
p.values = c(0.123, 0.000000000000679, 0.0000000000000002)

# Adjust the p-values
p.adjust(p.values, method = "BH")



###################################################
### Interaction model
###################################################

lm.4 = lm(Lbudget ~ 1 + age + mpaa, data = movies)
lm.6 = lm(Lbudget ~ 1 + age + mpaa + age:mpaa, data = movies)

anova(lm.4, lm.6)



###################################################
### Plotting main-effects model
###################################################

# Set up data
plotData = expand.grid(
  age = 12:79,
  mpaa = c("PG", "PG-13", "R")
)

# Predict log-budget
plotData$Lbudget = predict(lm.4, newdata = plotData)

# Back-transform log-budget to budget
plotData$budget = exp(plotData$Lbudget)
head(plotData)

# Plot
ggplot(data = plotData, aes(x = age, y = budget, color = mpaa)) +
  geom_line() +
  theme_bw() +
  xlab("Movie age") +
  ylab("Budget (in millions of U.S. dollars)") +
  scale_color_brewer(name = "MPAA rating", palette = "Set1")
