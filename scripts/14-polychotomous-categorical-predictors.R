##################################################
### Load libraries
##################################################

library(broom)
library(corrr)
library(ggridges)
library(tidyverse)



##################################################
### Read in data
##################################################

family = read_csv(file = "~/Documents/github/epsy-8251/data/substance-family.csv")
head(family)



##################################################
### Exploration
##################################################

# Marginal distribution of substance use
ggplot(data = family, aes(x = substance_use, y = ..density..)) +
  geom_histogram(color = "black", fill = "skyblue") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Substance use") +
  ylab("Probability density")

# Substance use conditioned on family structure
ggplot(data = family, aes(x = substance_use, y = family_structure)) +
  geom_density_ridges() +
  theme_bw() +
  ylab("Family structure") +
  xlab("Substance use")

# Compute summary statistics
family %>%
  group_by(family_structure) %>%
  summarize(
    M  = mean(substance_use),
    SD = sd(substance_use)
  )



##################################################
### Create dummy-coded variables
##################################################

# Create all three dummy variables
family = family %>%
  mutate(
    two_parent = if_else(family_structure == "Two-parent family", 1, 0),
    parent_guardian = if_else(family_structure == "One-parent, one guardian", 1, 0),
    one_parent = if_else(family_structure == "Single-parent family", 1, 0),
  )

# Examine data
head(family)

# Get category names
unique(family$family_structure)



##################################################
### Fit model
##################################################

# Two-parent households is reference group
lm.1 = lm(substance_use ~ 1 + parent_guardian + one_parent, data = family)

# Model-level info
glance(lm.1)

# Coefficient-level info
tidy(lm.1)


##################################################
### Fit model to get third difference
##################################################

# Single-parent households is reference group
lm.2 = lm(substance_use ~ 1 + parent_guardian + two_parent, data = family)

# Model-level info
glance(lm.2)

# Coefficient-level info
tidy(lm.2)



##################################################
### Adjusted p-values
##################################################

# Create vector of unadjusted p-values
p_values = c(0.079, 0.005, 0.609)

# Bonferroni adjustment to the p-values
p.adjust(p_values, method = "bonferroni")

# Benjamini-Hochberg adjusted p-values
p.adjust(p_values, method = "BH")



##################################################
### Control for other variables
##################################################

# Two-parent households is reference group
lm.3 = lm(substance_use ~ 1 + female + gpa + parent_guardian + one_parent, data = family)

# Model-level output
glance(lm.3)

# Coefficient-level output
tidy(lm.3)

# One-parent households is reference group
lm.4 = lm(substance_use ~ 1 + female + gpa + parent_guardian + two_parent, data = family)

# Coefficient-level inference
tidy(lm.4)



##################################################
### Adjusted p-values
##################################################

# Create vector of unadjusted p-values
p_values = c(0.092, 0.007, 0.092)

# Bonferroni adjustment to the p-values
p.adjust(p_values, method = "BH")


