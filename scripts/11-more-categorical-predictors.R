##################################################
### Load libraries
##################################################

library(broom)
library(corrr)
library(dplyr)
library(ggplot2)
library(readr)
library(sm)



##################################################
### Read in data
##################################################

stem = read_csv(file = "~/Dropbox/epsy-8251/data/stem.csv")
head(stem)



##################################################
### Exploration
##################################################

# Density plot of the median incomes
sm.density(stem$income, xlab = "Median income")
sm.density(stem$women, xlab = "Percentage of women")

# Compute summary statistics for income
stem %>% 
  summarize(
    M  = mean(income),
    SD = sd(income),
    Min = min(income),
    Max = max(income)
  )



##################################################
### Are Women Attracted to Lower-Earning STEM Majors?
##################################################

# Scatterplot
ggplot(data = stem, aes(x = women, y = income)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  xlab("Percentage of recent graduates who are women") +
  ylab("Median income (in thousands of dollars)")


# Compute correlation coefficient
stem %>%
  select(income, women) %>%
  correlate() %>%
  shave() %>%
  fashion(decimals = 3)


# Fit regression model
lm.1 = lm(income ~ 1 + women, data = stem)
summary(lm.1)



##################################################
### Are there Income Differences by the Type of STEM Major?
##################################################

# Plot the median incomes by STEM type
ggplot(data = stem, aes(x = stem_type, y = income, fill = stem_type)) +
  geom_point(shape = 21, color = "black", size = 4) +
  theme_bw() +
  xlab("Type of STEM Major") +
  ylab("Median income (in thousands of dollars)") +
  guides(fill = FALSE)


# Summary statistics
stem %>%
  group_by(stem_type) %>%
  summarize( M = mean(income), SD = sd(income), N = n() )

# Ridge plots
library(ggridges)

ggplot(data = stem, aes(x = income, y = stem_type)) + 
  geom_density_ridges() +
  theme_bw() +
  xlab("Median income") +
  ylab("Type of STEM major")



##################################################
### Create all four dummy variables
##################################################

stem = stem %>%
  mutate( 
    science = if_else(stem_type == "Science", 1, 0),
    tech = if_else(stem_type == "Technology", 1, 0),
    engineer = if_else(stem_type == "Engineering", 1, 0),
    math = if_else(stem_type == "Mathematics", 1, 0)
  )

head(stem)



##################################################
### Fit regression models
##################################################

# Science is reference group
lm.science = lm(income ~ 1 + tech + engineer + math, data = stem)
summary(lm.science)


# tech is reference group
lm.tech = lm(income ~ 1 + science + engineer + math, data = stem)
summary(lm.tech)


# math is reference group
lm.math = lm(income ~ 1 + science + tech + engineer, data = stem)
summary(lm.math)


# engineer is reference group
lm.engineer = lm(income ~ 1 + science + tech + math, data = stem)
summary(lm.engineer)



##################################################
### p-value adjustments
##################################################

# Input unadjusted p-values
p_values = c(
  0.349,           # Science vs. Technology
  0.0000000000228, # Science vs. Engineering
  0.235,           # Science vs. Mathematics
  0.000342,        # Technology vs. Engineering
  0.701,           # Technology vs. Mathematics
  0.014            # Engineering vs. Mathematics
)


# Bonferroni adjustment to the p-values
p.adjust(p_values, method = "bonferroni")


# Benjamini-Hochberg adjustment to the p-values
p.adjust(p_values, method = "BH")



##################################################
### Does STEM Type Mediate the Relationship Between Proportion of Women and Income?
##################################################

lm.mediator = lm(income ~ 1 + women + tech + engineer + math, data = stem)
summary(lm.mediator)




##################################################
### ANCOVA analysis
##################################################

# Fit ANCOVA models
lm.science.control  = lm(income ~ 1 + women +           tech + engineer + math, data = stem)
lm.tech.control     = lm(income ~ 1 + women + science +        engineer + math, data = stem)
lm.engineer.control = lm(income ~ 1 + women + science + tech +            math, data = stem)
lm.math.control     = lm(income ~ 1 + women + science + tech + engineer,        data = stem)

# Adjust p-values
p_values = c(0.814, 0.001, 0.591, 0.0004, 0.501, 0.043)
p.adjust(p_values, method = "BH")

