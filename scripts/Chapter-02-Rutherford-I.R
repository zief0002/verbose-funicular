##################################################
### Create data
##################################################

wr = data.frame(
  words.30  = c(7, 3, 6, 6, 5, 8, 6, 7),
  words.60  = c(7, 11, 9, 11, 10, 10, 11, 11),
  words.180 = c(8, 14, 10, 11, 12, 10, 11, 12)
  )
 


##################################################
### Load libraries
##################################################

library(ggplot2)
library(psych)
library(reshape2)
library(sm)



##################################################
### Prep data
##################################################

wr = melt(wr, variable.name = "condition", value.name = "words")

levels(wr$condition) = c("30s", "60s", "180s")




##################################################
### Marginal distribution of outcome
##################################################

sm.density(wr$words)
describe(wr$words)



##################################################
### Conditional distribution of outcome
##################################################

ggplot(data = wr, aes(x = condition, y = words)) +
  geom_boxplot(fill = "steelblue") +
  theme_bw() +
  scale_x_discrete(name = "Experimental conditions") +
  scale_y_continuous(name = "Number of words recalled")

describeBy(wr$words, wr$condition)



##################################################
### Fit the models
##################################################

# marginal mean model
lm.m = lm(words ~ 1, data = wr)
anova(lm.m)

#var(wr$words) * 23

# conditional mean model
lm.c = lm(words ~ 1 + condition, data = wr)
anova(lm.c)

# Nested F-test
anova(lm.m, lm.c)





