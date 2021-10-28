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

keith = read_csv(file = "https://raw.githubusercontent.com/zief0002/modeling/master/data/keith-gpa.csv")
head(keith)



##################################################
### Examine the marginal and conditional distributions
##################################################

# Density plot of the marginal distribution of GPA
ggplot(data = keith, aes(x = gpa)) +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("GPA (on a 100-pt. scale)") +
  ylab("Probability density") +
  ggtitle("Outcome: GPA")


# Density plot of the marginal distribution of homework
ggplot(data = keith, aes(x = homework)) +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Time spent on homework per week (in hours)") +
  ylab("Probability density")  +
  ggtitle("Predictor: Homework")


# Summary statistics
keith %>%
  summarize(
    M_gpa  = mean(gpa),
    SD_gpa = sd(gpa),
    M_hw   = mean(homework),
    SD_hw  = sd(homework)
    )


# Scatterplot
ggplot( data = keith, aes(x = homework, y = gpa) ) +
  geom_point(size = 5) +
  theme_bw() +
  xlab("Time spent on homework per week (in hours)") +
  ylab("GPA (on a 100-pt. scale)")



##################################################
### Correlation
##################################################

keith %>%
  select(gpa, homework) %>%
  correlate()



##################################################
### Standardized regression
##################################################

# Standardize the outcome and predictor
keith = keith %>%
  mutate(
    z_gpa = (gpa - mean(gpa)) / sd(gpa),
    z_homework = (homework - mean(homework)) / sd(homework)
  )

head(keith)


# Fit standardized regression
lm.z = lm(z_gpa ~ 1 + z_homework, data = keith)
lm.z



##################################################
### Scatterplot of standardized variables
##################################################

ggplot(data = keith, aes(x = z_homework, y = z_gpa)) +
  geom_point(size = 5) +
  theme_bw() +
  xlab("Time spent on homework (standardized)") +
  ylab("GPA (standardized)") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_abline(intercept = 0, slope = 0.327)



##################################################
### Variance accounted for (R^2) in standardized regression
##################################################

keith %>%
  select(z_gpa, z_homework) %>%
  correlate()


# Compute the SSE for the standardized intercept-only model
keith %>%
  mutate(
    y_hat = 0,
    errors = z_gpa - y_hat,
    sq_errors = errors ^ 2
  ) %>%
  summarize(
    SSE = sum(sq_errors)
  )


# Compute the SSE for the standardized slope-intercept model
keith %>%
  mutate(
    y_hat = 0 + 0.327 * z_homework,
    errors = z_gpa - y_hat,
    sq_errors = errors ^ 2
  ) %>%
  summarize(
    SSE = sum(sq_errors)
  )


# Compute R^2
(99 - 88.39) / 99



##################################################
### Correlation Between Observed Values, Fitted Values, and Residuals
##################################################

keith %>%
  mutate(
    y_hat = 74.290 + 1.214 * homework,
    errors = gpa - y_hat
  ) %>%
  select(gpa, y_hat, errors) %>%
  correlate()


