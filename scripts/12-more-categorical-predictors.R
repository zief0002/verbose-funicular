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



##################################################
### Read in data
##################################################

stem = read_csv(file = "~/Documents/github/epsy-8251/data/stem.csv")
head(stem)



##################################################
### Exploration
##################################################

# Density plot of the median incomes
sm.density(stem$income, xlab = "Median income")


# Compute summary statistics for income
stem %>% 
  summarize(
    M  = mean(income),
    SD = sd(income),
    Min = min(income),
    Max = max(income)
  )



##################################################
### Explore H1
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
  fashion(decimals = 3)


# Fit regression model
lm.1 = lm(income ~ 1 + women, data = stem)

glance(lm.1) #Model-level info
tidy(lm.1) # Coefficient-level info



##################################################
### Explore H2
##################################################

# Plot the median incomes by STEM type
ggplot(data = stem, aes(x = factor(stem_major), y = income, fill = factor(stem_major))) +
  geom_point(shape = 21, color = "black", size = 4) +
  theme_bw() +
  scale_x_discrete(name = "", labels = c("Non-STEM major", "STEM major")) +
  ylab("Median income (in thousands of dollars)") +
  scale_fill_viridis_d() +
  guides(fill = FALSE)


# Summary statistics
stem %>%
  group_by(stem_major) %>%
  summarize(
    M = mean(income), 
    SD = sd(income), 
    N = n()
  )


stem %>%
  select(income, stem_major) %>%
  correlate() %>%
  fashion(decimals = 3)



##################################################
### Ridge plots
##################################################

library(ggridges)

ggplot(data = stem, aes(x = income, y = factor(stem_major))) + 
  geom_density_ridges() +
  theme_bw() +
  xlab("Median income") +
  ylab("Dummy coded STEM major") +
  coord_flip()


##################################################
### Fit regression model
##################################################

lm.2 = lm(income ~ 1 + stem_major, data = stem)

glance(lm.2) #Model-level info
tidy(lm.2) # Coefficient-level info



##################################################
### Explore H3
##################################################

# Ridge plot
ggplot(data = stem, aes(x = income, y = s_tem)) + 
  geom_density_ridges() +
  theme_bw() +
  xlab("Median income") +
  ylab("Major categorization") +
  coord_flip()


# Summary statistics
stem %>%
  group_by(s_tem) %>%
  summarize(
    M = mean(income), 
    SD = sd(income), 
    N = n()
  )



##################################################
### Create all four dummy variables
##################################################

stem = stem %>%
  mutate( 
    science       = if_else(s_tem == "Science",               1, 0),
    tech_eng_math = if_else(s_tem == "Tech-Engineering-Math", 1, 0),
    non_stem      = if_else(s_tem == "Non-STEM",              1, 0)
  )

# Examine data
head(stem)



##################################################
### Fit regression models
##################################################

# non-STEM is reference group
lm.3 = lm(income ~ 1 + science + tech_eng_math, data = stem)

glance(lm.3)
tidy(lm.3)


# Science majors is reference group
lm.science = lm(income ~ 1 + non_stem + tech_eng_math, data = stem)

glance(lm.science)
tidy(lm.science)


# tech/eng/math is reference group
lm.tech = lm(income ~ 1 + non_stem + science, data = stem)

glance(lm.tech)
tidy(lm.tech)



##################################################
### p-value adjustments
##################################################

# Input unadjusted p-values
p_values = c(
  0.00000000002373643,          # Tech/Eng./Mathematics vs. Science
  0.00000000000000000002477524, # Tech/Eng./Mathematics vs. non-STEM
  0.1074411                     # Science vs. non-STEM
)


# Bonferroni adjustment to the p-values
p.adjust(p_values, method = "bonferroni")


# Benjamini-Hochberg adjustment to the p-values
p.adjust(p_values, method = "BH")



##################################################
### Mediation model
##################################################

lm.mediator = lm(income ~ 1 + women + science + tech_eng_math, data = stem)

glance(lm.mediator)
tidy(lm.mediator)



##################################################
### ANCOVA analysis
##################################################

# Fit ANCOVA models
# Fit ANCOVA models
lm.7 = lm(income ~ 1 + women + science + tech_eng_math, data = stem)
lm.8 = lm(income ~ 1 + women + non_stem + tech_eng_math, data = stem)


# Enter unadjusted p-values
p_values = c(
  0.02544385,       #non-STEM vs. science
  0.00000003928573, #non-STEM vs. Tech/Eng/Math
  0.0003641043      #science vs. Tech/Eng/Math
)


# Adjust p-values
p.adjust(p_values, method = "BH")

