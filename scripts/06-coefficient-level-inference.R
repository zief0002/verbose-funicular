##################################################
### Load libraries
##################################################

library(broom)
library(dplyr)
library(ggplot2)
library(readr)
library(sm)



##################################################
### Read in data
##################################################

city = read_csv(file = "~/Documents/github/epsy-8251/data/riverside.csv") 

head(city)
tail(city)



##################################################
### Regress income on edu
##################################################

lm.1 = lm(income ~ 1 + education, data = city)
lm.1


##################################################
### Coefficient-level output
##################################################

tidy(lm.1)



##################################################
### Interval estimates of the regression parameters
##################################################

confint(lm.1, level = 0.95)



##################################################
### Coefficient plot
##################################################

# Add the lower- and upper-limits for the CIs
my_reg = tidy(lm.1) %>%
  mutate(
    lwr_limit = estimate - 2 * std.error,
    upr_limit = estimate + 2 * std.error
  )

# View the output
my_reg

# Create the coefficient plot
ggplot(data = my_reg, aes(x = estimate, y = term)) +
  geom_segment(
    aes(x = lwr_limit, xend = upr_limit, y = term, yend = term), 
    color = "red") +
  geom_point() +
  geom_vline(
    xintercept = 0, 
    linetype = "dashed"
  ) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_y_discrete(name = "Coefficients", labels = c("Intercept", "Education")) +
  xlab("Estimate")







