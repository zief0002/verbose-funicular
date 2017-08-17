##################################################
### Load libraries
##################################################

library(corrr)
library(ggplot2)
library(readr)



##################################################
### Read in data
##################################################

city = read_csv(file = "~/Dropbox/epsy-8251/data/riverside.csv") 

head(city)
tail(city)



##################################################
### Regress income on edu
##################################################

lm.1 = lm(income ~ 1 + education, data = city)
lm.1



##################################################
### Compute the SSE
##################################################

# Step 1: Compute the predicted values of Y
y_hat = 11321 + 2651 * city$education
y_hat

# Step 2: Compute the residuals
errors = city$income - y_hat
errors

# Step 3: Compute the squared residuals
sq_errors = errors^2
sq_errors

# Step 4: Compute the sum of the squared residuals
sum(sq_errors)

# Chain all the computations together
sse.1 = sum((city$income - (11321 + 2651 * city$education))^2)
sse.1



##################################################
### Intercept-Only Model: A Baseline for Comparison
##################################################

# Fit intercept-only model
lm.0 = lm(income ~ 1, data = city)
lm.0

# Plot of the intercept-only model
ggplot(data = city, aes(x = education, y = income)) +
  geom_point() +
  geom_hline(yintercept = 53742, color = "blue") +
  xlab("Education (in years)") +
  ylab("Income") +
  theme_bw()


# Compute SSE for intercept-only (baseline) model
sse.0 = sum((city$income - 53742) ^ 2)
sse.0


##################################################
### Compute proportion of the reduction in error (PRE)
##################################################

pre = (sse.0 - sse.1) / sse.0
pre

# Correlation ^ 2
city %>% 
  select(income, education) %>%
  correlate() 

0.7947847 ^ 2


# Unexplained variation
1 - pre



