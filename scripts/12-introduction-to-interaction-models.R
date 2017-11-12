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

beauty = read_csv(file = "~/Dropbox/epsy-8251/data/beauty.csv")
head(beauty)





##################################################
### Fit main-effects model
##################################################

lm.1 = lm(avgeval ~ 1 + btystdave + female, data = beauty)
summary(lm.1)



##################################################
### Plot main-effects model results
##################################################

# Set up dataframe
myData = expand.grid(
  btystdave = seq(from = -1.6, to = 1.9, by = 0.1),
  female = c(0,1)
)


# Add y-hat values
myData = myData %>% mutate(yhat = predict(lm.1, newdata = myData))


# Coerce female into a factor
myData = myData %>% 
  mutate(gender = factor(myData$female, 
                         levels = c(0, 1), 
                         labels = c("Male", "Female")
                         )
  )


# Plot
ggplot(data = myData, aes(x = btystdave, y = yhat, color = gender)) +
  geom_line() +
  theme_bw() +
  xlab("Beauty score") +
  ylab("Predicted average course evaluation score") +
  scale_color_brewer(name = "", palette = "Set1")



##################################################
### Explore data for differential effects between beauty and sex
##################################################

ggplot(data = beauty, aes(x = btystdave, y = avgeval, color = factor(female))) +
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

# Create interaction term
beauty = beauty %>% mutate(bty_female = btystdave * female)
head(beauty)


# Fit interaction model
lm.2 = lm(avgeval ~ 1 + btystdave + female + bty_female, data = beauty)
summary(lm.2)



##################################################
### Explore data for differential effects between beauty and tenure
##################################################

ggplot(data = beauty, aes(x = btystdave, y = avgeval, color = factor(tenured))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  xlab("Beauty score") +
  ylab("Predicted average course evaluation score") +
  scale_color_brewer(name = "", palette = "Set1", labels = c("Non-tenured", "Tenured")) +
  facet_wrap(~tenured)



##################################################
### Fit interaction model
##################################################

# Create interaction predictor
beauty = beauty %>% mutate(bty_tenured = btystdave * tenured)
head(beauty)


# Fit interaction model
lm.3 = lm(avgeval ~ 1 + btystdave + tenured + bty_tenured, data = beauty)
summary(lm.3)



##################################################
### Plot interaction model results
##################################################

# Create new data set with main effects
myData = expand.grid(
  btystdave = seq(from = -1.6, to = 1.9, by = 0.1),
  tenured = c(0, 1)
)


# Compute interaction effect
myData = myData %>% mutate( bty_tenured = btystdave * tenured )


# Note this can be done in one computation as well
myData = expand.grid(
  btystdave = seq(from = -1.6, to = 1.9, by = 0.1),
  tenured = c(0, 1)
) %>%
  mutate( bty_tenured = btystdave * tenured )


# Use fitted model to compute fitted values for the data
myData = myData %>% mutate( yhat = predict(lm.3, newdata = myData) )
head(myData)


# Plot the fitted model
ggplot(data = myData, aes(x = btystdave, y = yhat, color = factor(tenured))) +
  geom_line() +
  theme_bw() +
  xlab("Beauty score") +
  ylab("Predicted average course evaluation score") +
  scale_color_brewer(name = "", palette = "Set1", labels = c("Non-tenured", "Tenured")) +
  ylim(0, 5)



##################################################
### Examine model assumptions
##################################################

# Create fortified data
fort_lm3 = augment(lm.3)
head(fort_lm3)


# Examine normality assumption
sm.density(fort_lm3$.std.resid, model = "normal", xlab = "Studentized Residuals")


# Examine other assumptions
ggplot(data = fort_lm3, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), linetype = "dotted") +
  geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("Fitted Values") +
  ylab("Studentized Residuals")
