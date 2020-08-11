##################################################
### Load libraries
##################################################

library(broom)
library(corrr)
library(dotwhisker)
library(dplyr)
library(ggplot2)
library(readr)



##################################################
### Set options so up to 6 significant digits print
##################################################

options(pillar.sigfig = 6)



##################################################
### Read in data
##################################################

city = read_csv(file = "https://raw.githubusercontent.com/zief0002/modeling/master/data/riverview.csv")
head(city)



##################################################
### Fit regression model
##################################################

lm.a = lm(income ~ 1 + education, data = city)

glance(lm.a) # Model-level results
tidy(lm.a)   # Coefficient-level results




##################################################
### Examine seniority predictor
##################################################

# Examine the marginal distribution
ggplot(data = city, aes(x = seniority)) +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Seniority level (in years)") +
  ylab("Probability density")


# Compute mean and standard deviation
city %>% 
  summarize(
    M = mean(seniority), 
    SD = sd(seniority)
    )



##################################################
### Examine relationship between seniority and income
##################################################

# Scatterplot
ggplot(data = city, aes(x = seniority, y = income)) +
  geom_point() +
  theme_bw() +
  xlab("Seniority (in years)") +
  ylab("Income (in thousands of dollars)")


# Correlation matrix
city %>%
  select(income, education, seniority) %>%
  correlate()



##################################################
### Regress income on seniority
##################################################

lm.b = lm(income ~ 1 + seniority, data = city) 

glance(lm.b) # Model-level results
tidy(lm.b)   # Coefficient-level results




##################################################
### Fit the multiple regression model
##################################################

lm.c = lm(income ~ 1 + education + seniority, data = city) 

glance(lm.c) # Model-level results
tidy(lm.c)   # Coefficient-level results


# Partitioning the sums of squares
anova(lm.c)



##################################################
### Changing the order of the predictors
##################################################

# Fit model with different predictor order
lm.d = lm(income ~ 1 + seniority + education, data = city)


anova(lm.d)  # ANOVA decomposition
glance(lm.d) # Model-level output
tidy(lm.d)   # Coefficient-level output



##################################################
### Coefficient plot 1
##################################################

# Create tidy() data frames with model names
mod_1 = tidy(lm.a) %>%
  mutate(model = "Model A")

mod_2 = tidy(lm.b) %>%
  mutate(model = "Model B")

mod_3 = tidy(lm.c) %>%
  mutate(model = "Model C")


# Combine into single data frame
all_models = rbind(mod_1, mod_2, mod_3)


# Create plot
dwplot(all_models, show_intercept = FALSE) +
  theme_bw() +
  scale_color_manual(name = "Model", values = c("#c62f4b", "#c62f4b", "#c62f4b")) +
  scale_x_continuous(name = "Estimate") +
  scale_y_discrete(name = "Coefficients", labels = c("Seniority", "Education")) +
  facet_wrap(~model) +
  guides(color = FALSE)


