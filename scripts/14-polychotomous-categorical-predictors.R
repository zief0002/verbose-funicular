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

family = read_csv(file = "https://raw.githubusercontent.com/zief0002/modeling/master/data/substance-family.csv")
head(family)



##################################################
### Exploration
##################################################

# Density plot of substance use
ggplot(data = family, aes(x = substance_use)) +
  stat_density(geom = "line", color = "#c62f4b") +
  theme_bw() +
  xlab("Standardized substance use") +
  ylab("Probability density")


# Bar plot of family structure
ggplot(data = family, aes(x = family_structure)) +
  geom_bar(fill = "#c62f4b") +
  theme_bw() +
  xlab("Family structure") +
  ylab("Frequency")


# Scatterplot
ggplot(data = family, aes(x = family_structure, y = substance_use)) +
  geom_point() +
  theme_bw() +
  xlab("Family structure") +
  ylab("Standardized substance use")


# Substance use conditioned on family structure
ggplot(data = family, aes(x = substance_use, y = family_structure)) +
  geom_density_ridges() +
  theme_bw() +
  ylab("Family structure") +
  xlab("Standardized substance use")


# Compute summary statistics
family %>%
  group_by(family_structure) %>%
  summarize(
    M  = mean(substance_use),
    SD = sd(substance_use),
    N = n()
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
print(head(family), width = Inf)


# Get category names
family %>% 
  select(family_structure) %>% 
  unique()



##################################################
### Fit model
##################################################

# Two-parent households is reference group
lm.a = lm(substance_use ~ 1 + parent_guardian + one_parent, data = family)

print(glance(lm.a), width = Inf)  # Model-level
tidy(lm.a)                        # Coefficient-level info



##################################################
### Fit model to get third difference
##################################################

# Single-parent households is reference group
lm.b = lm(substance_use ~ 1 + parent_guardian + two_parent, data = family)

print(glance(lm.b), width = Inf)  # Model-level
tidy(lm.b)                        # Coefficient-level



##################################################
### Adjusted p-values
##################################################

# Dunn-Bonferonni adjustment to p-values (naive)
c(0.079, 0.005, 0.609) * 3


# Dunn-Bonferroni adjustment to p-values
data.frame(
  comparison = c("Two-parent vs. Parent/guardian", "Two-parent vs. Single-parent", "Parent/guardian vs. Single-parent"),
  unadjusted_p = c(0.079, 0.005, 0.609)
) %>%
  mutate(
    bonferroni_p = p.adjust(unadjusted_p, method = "bonferroni")
  )


# Benjamini-Hochberg adjusted p-values
data.frame(
  comparison = c("Two-parent vs. Parent/guardian", "Two-parent vs. Single-parent", "Parent/guardian vs. Single-parent"),
  unadjusted_p = c(0.079, 0.005, 0.609)
) %>%
  mutate(
    bh_p = p.adjust(unadjusted_p, method = "BH")
  )



##################################################
### Control for other variables
##################################################

# Two-parent households is reference group
lm.c = lm(substance_use ~ 1 + female + gpa + parent_guardian + one_parent, data = family)

print(glance(lm.c), width = Inf)  # Model-level
tidy(lm.c)                        # Coefficient-level


# One-parent households is reference group
lm.d = lm(substance_use ~ 1 + female + gpa + parent_guardian + two_parent, data = family)

tidy(lm.d) # Coefficient-level



##################################################
### Adjusted p-values
##################################################

data.frame(
  comparison = c("Two-parent vs. Parent/guardian", "Two-parent vs. Single-parent", "Parent/guardian vs. Single-parent"),
  unadjusted_p = c(0.092, 0.007, 0.092)
) %>%
  mutate(
    bh_p = p.adjust(unadjusted_p, method = "BH")
  )



