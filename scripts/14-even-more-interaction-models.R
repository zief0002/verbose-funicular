##################################################
### Load libraries
##################################################

library(broom)
library(dplyr)
library(ggplot2)
library(readr)
library(sm)



###################################################
### Read in the data
###################################################

anemia = read_csv(file = "~/Dropbox/epsy-8251/data/iron-deficiency.csv")
head(anemia)



##################################################
### Main effect of pot type
##################################################

## Examine numeric evidence for race differences
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
### Test main effect of pot type
##################################################

# Create pot type dummy variables
anemia = anemia %>%
  mutate(
    al_pot = if_else(pot == "Aluminum", 1, 0),
    cl_pot = if_else(pot == "Clay", 1, 0),
    ir_pot = if_else(pot == "Iron", 1, 0) #Do not name this iron..it is the DV
  )

# Fit intercept-only model
lm.0 = lm(iron ~ 1, data = anemia)

# Fit model with pot type effect
lm.1 = lm(iron ~ 1 + cl_pot + ir_pot, data = anemia)

# Nested F-test
anova(lm.0, lm.1)


##################################################
### Examine Potential Main Effect of Food Type
##################################################

## Examine numeric evidence for race differences
food_anemia = anemia %>%
  group_by(food) %>%
  summarize( M = mean(iron), SD = sd(iron), N = n() )

food_anemia

# Create plot to examine race differences
ggplot(data = food_anemia, aes(x = food, y = M)) +
  geom_line(aes(group = 1), color = "black", linetype = "dotted") +
  geom_point(size = 5, pch = 17) +
  theme_bw() +
  xlab("Food type") +
  ylab("Mean iron content")



##################################################
### Test main effect of food type
##################################################

# Create pot type dummy variables
anemia = anemia %>%
  mutate(
    legume = if_else(food == "Legumes", 1, 0),
    meat = if_else(food == "Meat", 1, 0),
    vegetable = if_else(food == "Vegetables", 1, 0) #Do not name this iron..it is the DV
  )

# Fit model with pot type effect
lm.2 = lm(iron ~ 1 + meat + vegetable, data = anemia)

# Nested F-test
anova(lm.0, lm.2)



##################################################
### Controlled effect of pot type
##################################################

lm.2 = lm(iron ~ 1 + meat + vegetable, data = anemia)

lm.3 = lm(iron ~ 1 + meat + vegetable + cl_pot + ir_pot, data = anemia)

anova(lm.2, lm.3)



##################################################
### Plot main effects model
##################################################

lm.4 = lm(iron ~ 1 + pot + food, data = anemia)

# Create new data set
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
  ggsci::scale_color_d3(name = "")



##################################################
### Examining the Potential Interaction Between Pot Type and Food Type
##################################################

## Examine numerical evidence for interaction
pot_food_anemia = anemia %>%
  group_by(pot, food) %>%
  summarize( M = mean(iron), SD = sd(iron), N = n() )

pot_food_anemia

# Create plot to examine the potential interaction
ggplot(data = pot_food_anemia, aes(x = pot, y = M, color = food)) +
  geom_line(aes(group = food), linetype = "dotted") +
  geom_point(size = 5, pch = 17) +
  theme_bw() +
  xlab("Pot type") +
  ylab("Mean iron content") +
  ggsci::scale_color_d3()



##################################################
### Test interaction effect
##################################################

# Fit interaction model
lm.5 = lm(iron ~ 1 + food + pot + food:pot, data = anemia)

# Nested F-test
anova(lm.4, lm.5)                                                                                                                                    



##################################################
### Test interaction effect using dummy variables
##################################################

lm.6 = lm(iron ~ 1 + cl_pot + ir_pot + meat + vegetable + 
            cl_pot:meat + cl_pot:vegetable + 
            ir_pot:meat + ir_pot:vegetable, data = anemia)

# Check that results are the same (compare to main effects using dummies)
anova(lm.3, lm.6)
            
            

##################################################
### Interpreting interaction effects
##################################################

summary(lm.5)


# Create new data set with main effects
plotData = expand.grid(
  pot = c("Aluminum", "Clay", "Iron"),
  food = c("Legumes", "Meat", "Vegetables")
)

# Use fitted model to compute fitted values for the data
plotData$yhat = predict(lm.5, newdata = plotData)
head(plotData)

# Plot the fitted model
ggplot(data = plotData, aes(x = pot, y = yhat, color = food)) +
  geom_line(aes(group = food), linetype = "dotted") +
  geom_point() +
  theme_bw() +
  xlab("Pot type") +
  ylab("Predicted average iron content") +
  ggsci::scale_color_d3(name = "")



##################################################
### Assumptions
##################################################

out5 = augment(lm.5)
head(out5)

# Examine normality assumption
sm.density(out5$.std.resid, model = "normal", xlab = "Studentized Residuals")

# Examine other assumptions
ggplot(data = out5, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-3, 3), linetype = "dotted") +
  geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("Fitted Values") +
  ylab("Studentized Residuals")



##################################################
### Pairwise differences
##################################################

## Create pot x food combination
anemia = anemia %>%
  mutate(food_pot = paste0(food, "_", pot))
head(anemia)

pairwise.t.test(x = anemia$iron, g = anemia$food_pot, p.adjust.method = "BH")