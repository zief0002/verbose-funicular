##################################################
### Load libraries
##################################################

library(corrr)
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
tail(city)



##################################################
### Regress income on education level
##################################################

lm.a = lm(income ~ 1 + education, data = city)
lm.a



##################################################
### Compute the SSE
##################################################

# Step 1: Compute the predicted values of Y
city %>%
  mutate(
    y_hat = 11.321 + 2.651 * education
    )


# Step 2: Compute the residuals
city %>%
  mutate(
    y_hat = 11.321 + 2.651 * education,
    errors = income - y_hat
    )


# Step 3: Compute the squared residuals
city %>%
  mutate(
    y_hat = 11.321 + 2.651 * education,
    errors = income - y_hat,
    sq_errors = errors ^ 2
  )


# Step 4: Compute the sum of the squared residuals
city %>%
  mutate(
    y_hat = 11.321 + 2.651 * education,
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
  geom_point(size = 5) +
  geom_hline(yintercept = 53.742, color = "blue") +
  xlab("Education (in years)") +
  ylab("Income") +
  theme_bw()


# Compute SSE for intercept-only (baseline) model
city %>%
  mutate(
    y_hat = 53.742,
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
(6565.53 - 2418.20) / 6565.53



# Unexplained variation
1 - 0.632



##################################################
### R-squared
##################################################

# Compute correlation between fitted values and residuals
city %>%
  mutate(
    y_hat = 11.321 + 2.651*education
  ) %>%
  select(y_hat, income) %>%
  correlate()


# Compute R^2
0.795 ^ 2



