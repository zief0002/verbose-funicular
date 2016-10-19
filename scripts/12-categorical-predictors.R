##################################################
### Read in data
##################################################

mn = read.csv(file = "~/Documents/epsy-8251/data/mnSchools.csv")

head(mn)
tail(mn)




##################################################
### Load libraries
##################################################

library(dplyr)
library(ggplot2)
library(sm)



##################################################
### Exploration
##################################################

ggplot(data = mn, aes(x = as.factor(public), y = gradRate)) +
  geom_point() +
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
### Recoding the predictor
##################################################

mn$private = ifelse(mn$public == 0, 1, 0)
head(mn)


# Fit regression model using private predictor
lm.private = lm(gradRate ~ 1 + private, data = mn)
summary(lm.private)





##################################################
### Examine assumptions
##################################################

# Use fortify() to obtain the fitted values and residuals
out = fortify(lm.public)
head(out)


# Density plot of the marginal standardized residuals
sm.density(out$.stdresid, model = "normal")


# Normality by sector
out_private = out %>% filter(public == 0)
sm.density(out_private$.stdresid, model = "normal")

out_public = out %>% filter(public == 1)
sm.density(out_public$.stdresid, model = "normal")


# Scatterplot of the standardized residuals versus the fitted values
ggplot(data = out, aes(x = .fitted, y = .stdresid)) +
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


# Use fortify() to obtain the fitted values and residuals
out3 = fortify(lm.3)
head(out3)


# Density plot of the marginal standardized residuals
sm.density(out3$.stdresid, model = "normal")


# Scatterplot of the standardized residuals versus the fitted values
ggplot(data = out3, aes(x = .fitted, y = .stdresid)) +
  geom_point(size = 4) +
  theme_bw() +
  geom_hline(yintercept = 0)





