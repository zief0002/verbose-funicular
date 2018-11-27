##################################################
### Load libraries
##################################################

library(broom)
library(corrr)
library(dotwhisker)
library(dplyr)
library(ggplot2)
library(readr)
library(sm)
library(tidyr)



###################################################
### Read in the data
###################################################

evals = read_csv(file = "~/Documents/github/epsy-8251/data/evaluations.csv")
head(evals)





##################################################
### Fit main-effects model
##################################################

lm.1 = lm(avg_eval ~ 1 + beauty + tenured, data = evals)

glance(lm.1)
tidy(lm.1)



##################################################
### Plot main-effects model results
##################################################

# Set up dataframe
profs = crossing(
  beauty = seq(from = -1.6, to = 1.9, by = 0.1), 
  tenured = c(0, 1)
)


# Add y-hat values and plot
profs %>% mutate(
  yhat = predict(lm.1, newdata = profs), # Get y-hat values
  tenured = factor( # Make tenured a factor for better plotting
    tenured, levels = c(0, 1), 
    labels = c("Non-Tenured", "Tenured")
    ) 
  ) %>%
  ggplot(aes(x = beauty, y = yhat, color = tenured)) +
    geom_line() +
    theme_bw() +
    xlab("Beauty rating") +
    ylab("Predicted average course evaluation score") + 
    scale_color_brewer(name = "", palette = "Set1")




##################################################
### Explore data for differential effects between beauty and tenure
##################################################

ggplot(data = evals, aes(x = beauty, y = avg_eval, color = factor(tenured))) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  xlab("Beauty rating") +
  ylab("Predicted average course evaluation score") +
  scale_color_brewer(name = "", palette = "Set1", labels = c("Non-Tenured", "Tenured")) + 
  facet_wrap(~tenured)



##################################################
### Fit interaction model
##################################################

# Create interaction term
evals = evals %>% 
  mutate(
    beauty_tenured = beauty * tenured 
    )

head(evals)


# Fit interaction model
lm.2 = lm(avg_eval ~ 1 + beauty + tenured + beauty_tenured, data = evals) 

tidy(lm.2)



##################################################
### Explore data for differential effects between beauty and female
##################################################

ggplot(data = evals, aes(x = beauty, y = avg_eval, color = factor(female))) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  xlab("Beauty score") +
  ylab("Predicted average course evaluation score") +
  scale_color_brewer(name = "", palette = "Set1", labels = c("Male", "Female")) + 
  facet_wrap(~female)



##################################################
### Fit interaction model
##################################################

# Create interaction predictor
evals = evals %>% 
  mutate(
    beauty_female = beauty * female 
    )

head(evals)


# Fit interaction model
lm.3 = lm(avg_eval ~ 1 + beauty + female + beauty_female, data = evals) 

tidy(lm.3)



##################################################
### Plot interaction model results
##################################################

# Create new data set with main effects
profs = crossing(
  beauty = seq(from = -1.6, to = 1.9, by = 0.1), female = c(0, 1)
  ) %>%
  mutate(
    beauty_female = beauty * female 
    )

# Compute fitted values for the data, and plot
profs %>% mutate(
  yhat = predict(lm.3, newdata = profs),
  female = factor(female, levels = c(0, 1), labels = c("Male", "Female")) 
  ) %>%
  ggplot(aes(x = beauty, y = yhat, color = female)) + 
    geom_line() +
    theme_bw() +
    xlab("Beauty rating") +
    ylab("Predicted average course evaluation score") + 
    scale_color_brewer(name = "", palette = "Set1") + 
    ylim(0, 5)



##################################################
### Examine model assumptions
##################################################

# Create fortified data
out.3 = augment(lm.3) 
head(out.3)


# Examine normality assumption
sm.density(out.3$.std.resid, model = "normal", xlab = "Studentized Residuals")


# Examine other assumptions
ggplot(data = out.3, aes(x = .fitted, y = .std.resid)) + 
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), linetype = "dotted") + 
  #geom_smooth() +
  theme_bw() +
  xlab("Fitted Values") + 
  ylab("Studentized Residuals")



##################################################
### Fit one last model
##################################################

lm.4 = lm(avg_eval ~ 1 + beauty + female + tenured + beauty_tenured, data = evals)
tidy(lm.4)


# Create new data set with main effects
profs = crossing(
  beauty = seq(from = -1.6, to = 1.9, by = 0.1), 
  female = c(0, 1),
  tenured = c(0, 1)
  ) %>%
  mutate(
    beauty_tenured = beauty * tenured 
  )

# Compute fitted values for the data, and plot
profs %>% mutate(
  yhat = predict(lm.4, newdata = profs),
  female = factor(female, levels = c(0, 1), labels = c("Male", "Female")), 
  tenured = factor(tenured, levels = c(0, 1), labels = c("Non-tenured", "Tenured"))
  ) %>%
  ggplot(aes(x = beauty, y = yhat, color = tenured)) + 
    geom_line() +
    theme_bw() +
    xlab("Beauty rating") +
    ylab("Predicted average course evaluation score") + 
    scale_color_brewer(name = "", palette = "Set1") + 
    ylim(0, 5) +
    facet_wrap(~female)
