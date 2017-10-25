##################################################
### Load libraries
##################################################

library(broom)
library(dplyr)
library(ggplot2)
library(readr)
library(sm)



##################################################
### Read in data
##################################################

mn = read_csv(file = "~/Dropbox/epsy-8251/data/mnSchools.csv")

head(mn)
tail(mn)




##################################################
### Exploration
##################################################

ggplot(data = mn, aes(x = as.factor(public), y = gradRate)) +
  geom_point(size = 4) +
  theme_bw() +
  scale_x_discrete(name = "Sector", labels = c("Private", "Public"))



mn %>%
  group_by(public) %>%
  summarize(
    M = mean(gradRate),
    SD = sd(gradRate),
    N = length(gradRate)
  )



##################################################
### Fit regression model
##################################################

lm.public = lm(gradRate ~ 1 + public, data = mn)
summary(lm.public)





##################################################
### Examine assumptions
##################################################

# Augment the model
out = augment(lm.public)
head(out)


# Density plot of the marginal standardized residuals
sm.density(out$.std.resid, model = "normal")


# Normality by sector
out_private = out %>% filter(public == 0)
sm.density(out_private$.std.resid, model = "normal")

out_public = out %>% filter(public == 1)
sm.density(out_public$.std.resid, model = "normal")


# Scatterplot of the standardized residuals versus the fitted values
ggplot(data = out, aes(x = .fitted, y = .std.resid)) +
  geom_point(size = 4) +
  theme_bw() +
  geom_hline(yintercept = 0)



##################################################
### Including other predictors (ANCOVA model)
##################################################

lm.2 = lm(gradRate ~ 1 + public + sat, data = mn)
summary(lm.2)



##################################################
### Compute adjusted means
##################################################

# Compute mean SAT
m_sat = mean(mn$sat)

# Compute adjusted means
d = expand.grid(
  public = c(0, 1),
  sat = m_sat
)

predict(lm.2, newdata = d)

# Compute adjusted mean difference
63.5 - 55.1



##################################################
### Fit ANCOVA model 2
##################################################

lm.3 = lm(gradRate ~ 1 + public + sat + tuition, data = mn)
summary(lm.3)


# Augment the model
out3 = augment(lm.3)
head(out3)


# Density plot of the marginal standardized residuals
sm.density(out3$.std.resid, model = "normal")


# Scatterplot of the standardized residuals versus the fitted values
ggplot(data = out3, aes(x = .fitted, y = .std.resid)) +
  geom_point(size = 4) +
  theme_bw() +
  geom_hline(yintercept = 0)





