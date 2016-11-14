###################################################
### Read in the data
###################################################

anemia = read.csv(file = "~/Documents/epsy-8251/data/iron-deficiency.csv")
head(anemia)





##################################################
### Load libraries
##################################################

library(dplyr)
library(ggplot2)
library(sm)



##################################################
### Examine main-effect of pot type
##################################################

pot_anemia = anemia %>%
  group_by(pot) %>%
  summarize( M = mean(iron), SD = sd(iron), N = n() )

pot_anemia


# Create plot to examine race differences
ggplot(data = pot_anemia, aes(x = pot, y = M)) +
  geom_line(aes(group = 1), color = "black", linetype = "dotted") +
  geom_point(size = 5, pch = 17) +
  theme_bw() +
  xlab("Pot type") +
  ylab("Mean iron content")



##################################################
### Examine whether main-effect of pot type is due to chance
##################################################

lm.0 = lm(iron ~ 1, data = anemia)
lm.1 = lm(iron ~ 1 + pot, data = anemia)

anova(lm.0, lm.1)



##################################################
### Examine controlled effect of pot type
##################################################

lm.2 = lm(iron ~ 1 + food, data = anemia)
lm.3 = lm(iron ~ 1 + food + pot, data = anemia)

anova(lm.2, lm.3)



##################################################
### Plot the controlled main-effect of pot type
##################################################

# Create new data set with main effects
plotData = expand.grid(
  pot = c("Aluminum", "Clay", "Iron"),
  food = c("Legumes", "Meat", "Vegetables")
)

# Use fitted model to compute fitted values for the data
plotData$yhat = predict(lm.3, newdata = plotData)
head(plotData)

# Plot the fitted model
ggplot(data = plotData, aes(x = pot, y = yhat, color = food)) +
  geom_line(aes(group = food), linetype = "dotted") +
  geom_point() +
  theme_bw() +
  xlab("Pot type") +
  ylab("Predicted average iron content") +
  scale_color_brewer(name = "", palette = "Set1")



##################################################
### Examine potential interaction effect between pot type and food type
##################################################

## Examine numerical evidence for interaction
pot_food_anemia = anemia %>%
  group_by(pot, food) %>%
  summarize( M = mean(iron), SD = sd(iron), N = n() )

pot_food_anemia


# Create plot to examine the potential interaction
ggplot(data = pot_food_anemia, aes(x = pot, y = M, color = food)) +
  geom_line(aes(group = food), color = "black", linetype = "dotted") +
  geom_point(size = 5, pch = 17) +
  theme_bw() +
  xlab("Pot type") +
  ylab("Mean iron content") +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Set2")



##################################################
### Is interaction effect due to chance?
##################################################

# Fit interaction model
lm.4 = lm(iron ~ 1 + food + pot + food:pot, data = anemia)

# Nested F-test
anova(lm.3, lm.4)



##################################################
### Plot the results of the interaction model
##################################################

# Create new data set with main effects
plotData = expand.grid(
  pot = c("Aluminum", "Clay", "Iron"),
  food = c("Legumes", "Meat", "Vegetables")
)


# Use fitted model to compute fitted values for the data
plotData$yhat = predict(lm.4, newdata = plotData)
head(plotData)


# Plot the fitted model
ggplot(data = plotData, aes(x = pot, y = yhat, color = food)) +
  geom_line(aes(group = food), linetype = "dotted") +
  geom_point() +
  theme_bw() +
  xlab("Pot type") +
  ylab("Predicted average iron content") +
  scale_color_brewer(name = "", palette = "Set1")



##################################################
### Examine model output
##################################################

summary(lm.4)



##################################################
### Pairwise differences in pot/food type combinations
##################################################

pairwise.t.test(x = anemia$iron, g = anemia$food:anemia$pot, p.adjust.method = "BH")



##################################################
### Examine model assumptions
##################################################

# Create fortified data
fort_lm3 = fortify(lm.4)
head(fort_lm4)


# Examine normality assumption
sm.density(fort_lm4$.stdresid, model = "normal", xlab = "Studentized Residuals")


# Examine other assumptions
ggplot(data = fort_lm4, aes(x = .fitted, y = .stdresid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-3, 3), linetype = "dotted") +
  geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("Fitted Values") +
  ylab("Studentized Residuals")
