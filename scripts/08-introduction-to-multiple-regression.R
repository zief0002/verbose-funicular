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



##################################################
### Read in data
##################################################

city = read_csv(file = "~/Documents/github/epsy-8251/data/riverside.csv") 
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
sm.density(city$seniority, xlab = "Seniority (in years)")


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
  ylab("Income (in dollars)")


# Correlation matrix
city %>%
  select(income, education, seniority) %>%
  correlate() %>%
  shave() %>%
  fashion(decimals = 2)



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
### Coefficient plot with dw_plot(): Simple regression
##################################################

m1 = tidy(lm.1) %>% mutate(model = "Model 1") # Create tidy model object
dw_plot(m1, show_intercept = TRUE) # Create coefficient plot


# Customize plot
dw_plot(m1, show_intercept = TRUE, order_vars = c("education", "(Intercept)")) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.4) +
  scale_y_discrete(name = "", labels = c("Intercept", "Education level")) +
  scale_color_manual(
    name = "", 
    labels = c("Model 1"),
    values = c("#999999")
  )



##################################################
### Coefficient plot with dw_plot(): Multiple regression
##################################################

# Create tidy model objects
m1 = tidy(lm.1) %>% mutate(model = "Model 1")
m2 = tidy(lm.2) %>% mutate(model = "Model 2")
m3 = tidy(lm.3) %>% mutate(model = "Model 3")

# Bind into a single object
all_models = rbind(m1, m2, m3)

# Coefficient plot
dw_plot(all_models, show_intercept = FALSE) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.4) +
  scale_y_discrete(
    name = "", 
    labels = c("Seniority level", "Education level")
  ) +
  scale_color_manual(
    name = "", 
    labels = c("Model 1", "Model 2", "Model 3"),
    values = c("#999999", "#e69f00", "#56b4e9")
  )
