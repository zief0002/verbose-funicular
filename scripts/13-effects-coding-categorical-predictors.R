##################################################
### Load libraries
##################################################

library(broom)
library(corrr)
library(tidyverse)
library(educate)
library(ggridges)
library(ungeviz)



##################################################
### Set options so up to 6 significant digits print
##################################################

options(pillar.sigfig = 6)



##################################################
### Read in data
##################################################

activity = read_csv(file = "~/Documents/github/epsy-8251/data/activity-based-courses.csv")
head(activity)




##################################################
### Exploration
##################################################

# Scatterplot
ggplot(data = activity, aes(x = grade_points, y = condition)) +
  geom_density_ridges() +
  theme_bw() +
  ylab("Experimental Condition") +
  xlab("Grade points")


# Descriptive statistics
activity %>% 
  group_by(condition) %>%
  summarize(
    M = mean(grade_points),
    SD = sd(grade_points),
    N = n()
  )



##################################################
### ACT score conditioned on condition --- Did the randomization equalize the groups?
##################################################

ggplot(data = activity, aes(x = act, y = condition)) +
  geom_density_ridges() +
  theme_bw() +
  ylab("Experimental Condition") +
  xlab("ACT score")


activity %>% 
  group_by(condition) %>%
  summarize(
    M = mean(act),
    SD = sd(act),
    N = n()
  )


##################################################
### Dummy code condition
##################################################

# Create new column that dummy-codes condition
activity = activity %>%
  mutate(
    treatment = if_else(condition == "Control", 0, 1)
  )

# View data
head(activity)

# Regress ACT on condition
lm.act = lm(act ~ 1 + treatment, data = activity)
tidy(lm.act)



##################################################
### Fit model to explain variation in grade-points
##################################################

# Fit model
lm.2 = lm(grade_points ~ 1 + treatment, data = activity)

# Model-level output
glance(lm.2)

# Coefficient-level output
tidy(lm.2)



##################################################
### Include ACT as a covariate
##################################################

# Fit model
lm.3 = lm(grade_points ~ 1 + treatment + act, data = activity)

# Model-level output
glance(lm.3)

# Coefficient-level output
tidy(lm.3)



##################################################
### Effect-code condition
##################################################

# Create new column that effect-codes condition
activity = activity %>%
  mutate(
    effect = if_else(condition == "Control", -1, 1)
  )

# View data
head(activity)



##################################################
### Fit effects-coded model
##################################################

# Fit model using effects-coded predictor
lm.4 = lm(grade_points ~ 1 + effect, data = activity)

# Model-level output
glance(lm.4)

# Coefficient-level output
tidy(lm.4)

# Marginal mean grade point
activity %>%
  summarize(
    M = mean(grade_points)
  )



##################################################
### Include ACT as a covariate
##################################################

# Fit model using effects-coded predictor
lm.5 = lm(grade_points ~ 1 + effect + act, data = activity)

# Model-level output
glance(lm.5)

# Coefficient-level output
tidy(lm.5)



##################################################
### Plot of the fitted model (lm.3)
##################################################

# Set up the data to plot
plot_data = crossing(
  sat = seq(from = 890, to = 1400, by = 10), 
  public = c(0, 1),
  tuition = mean(mn$tuition)
) 

plot_data %>%
  mutate(
    # Get predicted values
    yhat = predict(lm.3, newdata = plot_data),
    # Change public into a factor
    sector = factor(public, 
                    levels = c(0, 1), 
                    labels = c("Public institution", "Private institution")
    )
  ) %>%
  # Create plot
  ggplot(aes(x = sat, y = yhat, group = sector, color = sector)) +
  geom_line() +
  theme_bw() + 
  xlab("Median SAT score") + 
  ylab("Predicted graduation rate") +
  scale_color_viridis_d(name = "")

