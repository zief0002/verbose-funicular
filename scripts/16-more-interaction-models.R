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
### Examine data
##################################################

summary(evals)


# Discretize the age variable into 3 categories
evals = evals %>%
  mutate(
    age_discrete = cut(age, breaks = 3)
  )

summary(evals)




##################################################
### Examine effect of age
##################################################

ggplot(data = evals, aes(x = beauty, y = avg_eval, color = age_discrete)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  xlab("Beauty rating") +
  ylab("Average course evaluation score") +
  scale_color_brewer(palette = "Set2") +
  facet_wrap(~age_discrete)



##################################################
### Fit interaction model
##################################################

# Fit model
lm.1 = lm(avg_eval ~ 1 + beauty + age + beauty:age, data = evals)

# Model-level output
glance(lm.1)

# Coefficient-level output
tidy(lm.1)




##################################################
### Plot interaction model results
##################################################

# Create new data set with main effects
plot_data = crossing(
  beauty = seq(from = -1.6, to = 1.9, by = 0.1),
  age = c(40, 50, 60)
)

# Use fitted model to compute fitted values for the data
plot_data = plot_data %>% 
  mutate(
    yhat = predict(lm.1, newdata = plot_data)
  )

head(plot_data)

# Plot the fitted model
ggplot(data = plot_data, aes(x = beauty, y = yhat, color = factor(age))) +
  geom_line() +
  theme_bw() +
  xlab("Beauty score") +
  ylab("Predicted average course evaluation score") +
  scale_color_brewer(
    name = "Age", 
    palette = "Set1", 
    labels = c("40 years old", "50 years old", "60 years old")
  ) +
  ylim(0, 5)
  
  

##################################################
### Interpret interaction model effects
##################################################

tidy(lm.1)



##################################################
### Include covariates in the model
##################################################

# Fit model
lm.2 = lm(avg_eval ~ 1 + beauty + age + female + beauty:age, data = evals)

# Model-level output
glance(lm.2)

# Coefficient-level output
tidy(lm.2)



##################################################
### Plot the model results
##################################################

# Create new data set with main effects
plot_data = crossing(
  beauty = seq(from = -1.6, to = 1.9, by = 0.1),
  age = c(40, 50, 60),
  female = c(0, 1)
)

# Use fitted model to compute fitted values for the data
plot_data = plot_data %>% 
  mutate(
    yhat = predict(lm.2, newdata = plot_data),
    age = factor(age),
    female = factor(female, levels = c(0, 1), labels = c("Male", "Female"))
  )

head(plot_data)

# Plot the fitted model
ggplot(data = plot_data, aes(x = beauty, y = yhat, color = age)) +
  geom_line() +
  theme_bw() +
  xlab("Beauty score") +
  ylab("Predicted average course evaluation score") +
  scale_color_brewer(
    name = "Age", 
    palette = "Set1", 
    labels = c("40 years old", "50 years old", "60 years old")
  ) +
  ylim(0, 5) +
  facet_wrap(~female)                                                                                                                                    



##################################################
### Higher order interactions
##################################################

# Fit model
lm.3 = lm(avg_eval ~ 1 + beauty + age + female + 
            beauty:age + beauty:female + female:age + 
            beauty:age:female, data = evals)

# Model-level output
glance(lm.3)

# Coefficient-level output
tidy(lm.3)
            
            

##################################################
### Plot the model results
##################################################

# Create new data set with main effects
plot_data = crossing(
  beauty = seq(from = -1.6, to = 1.9, by = 0.1),
  age = c(40, 60),
  female = c(0, 1)
)

# Use fitted model to compute fitted values for the data
plot_data = plot_data %>% 
  mutate(
    yhat = predict(lm.3, newdata = plot_data)
  )

# Convert female and age into factors for better plotting
plot_data = plot_data %>%
  mutate(
    Sex = factor(female, levels = c(0, 1), labels = c("Males", "Females")),
    Age = factor(age, levels = c(40, 60), labels = c("40 year olds", "60 year olds"))
  )

head(plot_data)

# Plot the fitted model
ggplot(data = plot_data, aes(x = beauty, y = yhat, color = Age)) +
  geom_line() +
  theme_bw() +
  xlab("Beauty score") +
  ylab("Predicted average course evaluation score") +
  scale_color_brewer(name = "Age", palette = "Set1") +
  facet_wrap(~Sex)

# Plot 2
ggplot(data = plot_data, aes(x = beauty, y = yhat, color = Sex)) +
  geom_line() +
  theme_bw() +
  xlab("Beauty score") +
  ylab("Predicted average course evaluation score") +
  scale_color_brewer(name = "Age", palette = "Set1") +
  facet_wrap(~Age)

