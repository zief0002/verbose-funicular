##################################################
### Load libraries
##################################################

library(dplyr)
library(ggplot2)
library(readr)



###################################################
### Read in the data
###################################################

ecls = read_csv(file = "~/Dropbox/epsy-8251/data/ecls.csv")
head(ecls)





##################################################
### Examine sample mean differences
##################################################

# Obtain sample means and standard deviations for each ethnicity
ecls %>% 
  group_by(race) %>% 
  summarize( M = mean(read), SD = sd(read) )


# Plot the reading scores by ethnicity
ggplot(data = ecls, aes(x = race, y = read, color = race)) +
  geom_boxplot() +
  geom_point() +
	theme_bw() +
	xlab("Race/ethnicity") +
	ylab("Reading Score") +
  guides(color = FALSE)



##################################################
### Examine levels of factor and create dummy variables
##################################################

# Examine factor levels
levels(ecls$race)


# Create dummy variables
ecls = ecls %>% mutate(asian    = ifelse(race == "Asian",    1, 0))
ecls = ecls %>% mutate(black    = ifelse(race == "Black",    1, 0))
ecls = ecls %>% mutate(hispanic = ifelse(race == "Hispanic", 1, 0))
ecls = ecls %>% mutate(other    = ifelse(race == "Other",    1, 0))
ecls = ecls %>% mutate(white    = ifelse(race == "White",    1, 0))



##################################################
### Fit regression models (ANOVA)
##################################################

# Asian is reference group
lm.asian = lm(read ~ 1 + black + hispanic + other + white, data = ecls)
summary(lm.asian)


# Black is reference group
lm.black = lm(read ~ 1 + asian + hispanic + other + white, data = ecls)
summary(lm.black)


# Hispanic is reference group
lm.hispanic = lm(read ~ 1 + asian + black + other + white, data = ecls)
summary(lm.hispanic)


# Other is reference group
lm.other = lm(read ~ 1 + asian + black + hispanic + white, data = ecls)
summary(lm.other)


# White is reference group
lm.white = lm(read ~ 1 + asian + black + hispanic + other, data = ecls)
summary(lm.white)



##################################################
### Adjust p-values for multiple comparisons
##################################################

p.values = c(
  0.938,     # asian vs. black
  0.213,     # asian vs. hispanic
  0.306,     # asian vs. other
  0.326,     # asian vs. white
  0.0899,    # black vs. hispanic
  0.1976,    # black vs. other
  0.2365,    # black vs. white
  0.9674,    # hispanic vs. other
  0.0000281, # hispanic vs. white
  0.0088     # other vs. white
)


# Benjamani-Hochburg adjustment to the p-values
p.adjust(p.values, method = "BH")


# Bonferroni adjustment to the p-values
#p.adjust(p.values, method = "bonferroni")




##################################################
### Fit regression models (ANCOVA)
##################################################

# Asian is reference group
lm.asian = lm(read ~ 1 + black + hispanic + other + white + ses + momEd, data = ecls)
summary(lm.asian)


# Black is reference group
lm.black = lm(read ~ 1 + asian + hispanic + other + white + ses + momEd, data = ecls)
summary(lm.black)


# Hispanic is reference group
lm.hispanic = lm(read ~ 1 + asian + black + other + white + ses + momEd, data = ecls)
summary(lm.hispanic)


# Other is reference group
lm.other = lm(read ~ 1 + asian + black + hispanic + white + ses + momEd, data = ecls)
summary(lm.other)


# White is reference group
lm.white = lm(read ~ 1 + asian + black + hispanic + other + ses + momEd, data = ecls)
summary(lm.white)



##################################################
### Adjust p-values for multiple comparisons
##################################################

p.values = c(
  0.573, # asian vs. black
  0.655, # asian vs. hispanic
  0.794, # asian vs. other
  0.353, # asian vs. white
  0.165, # black vs. hispanic
  0.350, # black vs. other
  0.731, # black vs. white
  0.869, # hispanic vs. other
  0.013, # hispanic vs. white
  0.154  # other vs. white
)


# Benjamani-Hochburg adjustment to the p-values
p.adjust(p.values, method = "BH")




