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

city = read_csv(file = "~/Documents/github/epsy-8251/data/riverview.csv") 

head(city)
tail(city)



##################################################
### Examine outcome/response
##################################################

ggplot(data = city, aes(x = income)) +
  geom_histogram(color = "black", fill = "yellow", bins = 20)


ggplot(data = city, aes(x = income)) +
  #geom_density()
  stat_density(geom = "line")

ggplot(data = city, aes(x = income)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "yellow") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Income (in thousands of dollars)") +
  ylab("Probability density")


city %>% 
  summarize(
    M = mean(income), 
    SD = sd(income),
    Med = median(income)
    )



##################################################
### Examine predictor
##################################################

ggplot(data = city, aes(x = education)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "yellow") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Education level") +
  ylab("Probability density")


city %>% 
  summarize(
    M = mean(education), 
    SD = sd(education),
    Med = median(education)
    )



##################################################
### Examine conditional distribution of income given education
##################################################

ggplot(data = city, aes(x = education, y = income)) + 
  geom_point(size = 5) +
  theme_bw() +
  xlab("Education (in years)") +
  ylab("Income (in U.S. dollars)")



##################################################
### Regress income on edu
##################################################

lm.1 = lm(income ~ 1 + education, data = city)
lm.1



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








