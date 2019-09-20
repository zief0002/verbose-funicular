##################################################
### Load libraries
##################################################

library(broom)
library(corrr)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(ungeviz)



##################################################
### Set options so up to 6 significant digits print
##################################################

options(pillar.sigfig = 6)



##################################################
### Read in data
##################################################

city = read_csv(file = "~/Documents/github/epsy-8251/data/riverview.csv")
head(city)



##################################################
### Fit regression model
##################################################

lm.1 = lm(income ~ 1 + education, data = city)

glance(lm.1) # Model-level results
tidy(lm.1)   # Coefficient-level results




##################################################
### Examine seniority predictor
##################################################

# Examine the marginal distribution
ggplot(data = city, aes(x = seniority)) +
  geom_histogram(aes(y = ..density..), fill = "yellow", color = "black") +
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

lm.2 = lm(income ~ 1 + seniority, data = city) 

glance(lm.2) # Model-level results
tidy(lm.2)   # Coefficient-level results




##################################################
### Fit the multiple regression model
##################################################

lm.3 = lm(income ~ 1 + education + seniority, data = city) 

glance(lm.3) # Model-level results
tidy(lm.3)   # Coefficient-level results


# Partitioning the sums of squares
anova(lm.3)



##################################################
### Coefficient plot 1
##################################################

# Obtain tidy() output and filter out the intercept
coef_output = tidy(lm.3) %>%
  filter(term != "(Intercept)") 


# Plot
ggplot(data = coef_output, aes(x = estimate, y = term)) +
  stat_confidence_density(aes(moe = std.error, confidence = 0.68, 
                              fill = stat(ndensity)), height = 0.15) +
  geom_point(aes(x = estimate), size = 2) +
  scale_fill_gradient(low = "#eff3ff", high = "#6baed6") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(name = "Estimate", limits = c(-1, 4)) +
  scale_y_discrete(name = "Coefficients", labels = c("Education level", "Seniority"))



##################################################
### Coefficient plots from multiple models
##################################################

# Create tidy() objects and identify each with a model column
m1 = tidy(lm.1) %>% mutate(model = "Model 1")
m2 = tidy(lm.2) %>% mutate(model = "Model 2")
m3 = tidy(lm.3) %>% mutate(model = "Model 3")


# Combine all three tidy() outputs, filter out intercepts, and drop missing values
all_models = rbind(m1, m2, m3) %>%
  filter(term != "(Intercept)") %>%
  drop_na()


# Create coefficient plots
ggplot(data = all_models, aes(x = estimate, y = term)) +
  stat_confidence_density(aes(moe = std.error, confidence = 0.68, 
                              fill = stat(ndensity)), height = 0.15) +
  geom_point(aes(x = estimate), size = 2) +
  scale_fill_gradient(low = "#eff3ff", high = "#6baed6") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(name = "Estimate", limits = c(-1, 4)) +
  scale_y_discrete(name = "Coefficients", labels = c("Education level", "Seniority")) +
  facet_wrap(~model)


