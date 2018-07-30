##################################################
### Load libraries
##################################################

library(corrr)
library(dplyr)
library(ggplot2)
library(readr)



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
### Compute the SSE
##################################################

# Step 1: Compute the predicted values of Y
city %>%
  mutate(y_hat = 11321 + 2651 * education)


# Step 2: Compute the residuals
city %>%
  mutate(
    y_hat = 11321 + 2651 * education,
    errors = income - y_hat
    )


# Step 3: Compute the squared residuals
city %>%
  mutate(
    y_hat = 11321 + 2651 * education,
    errors = income - y_hat,
    sq_errors = errors ^ 2
  )


# Step 4: Compute the sum of the squared residuals
city %>%
  mutate(
    y_hat = 11321 + 2651 * education,
    errors = income - y_hat,
    sq_errors = errors ^ 2
  ) %>%
  summarize(
    SSE = sum(sq_errors)
  )



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
city %>%
  mutate(
    y_hat = 53742,
    errors = income - y_hat,
    sq_errors = errors ^ 2
  ) %>%
  summarize(
    SSE = sum(sq_errors)
  )



##################################################
### Compute proportion of the reduction in error (PRE)
##################################################

# pre = (sse.0 - sse.1) / sse.0
(6565527426 - 2418197826) / 6565527426


# Correlation ^ 2
city %>% 
  select(income, education) %>%
  correlate() 

0.7947847 ^ 2


# Unexplained variation
1 - 0.63168271935409



