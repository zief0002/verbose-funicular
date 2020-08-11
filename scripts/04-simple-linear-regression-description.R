##################################################
### Load libraries
##################################################

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
### Examine outcome/response -- Income
##################################################

ggplot(data = city, aes(x = income)) +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Income (in thousands of dollars)") +
  ylab("Probability density")


city %>%
  summarize(
    M = mean(income),
    SD = sd(income)
  )



##################################################
### Examine predictor -- Education level
##################################################

ggplot(data = city, aes(x = education)) +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Education level") +
  ylab("Probability density")


city %>% 
  summarize(
    M = mean(education), 
    SD = sd(education)
    )



##################################################
### Relationship between vartiable
##################################################

ggplot(data = city, aes(x = education, y = income)) + 
  geom_point(size = 5) +
  theme_bw() +
  xlab("Education (in years)") +
  ylab("Income (in U.S. dollars)")



##################################################
### Regress income on education level
##################################################

# Fit model
lm.a = lm(income ~ 1 + education, data = city)


# View coefficients
lm.a



##################################################
### Using the regression equation
##################################################

# See the 25th observation's data
city %>%
  filter(row_number() == 25)


# Compute the predicted income using the regression equation
11.321 + 2.651 * 20


# Compute the residual
54.672 - 64.341








