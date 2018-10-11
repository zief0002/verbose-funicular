##################################################
### Load libraries
##################################################

library(corrr)
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


options(scipen = 9999)


##################################################
### Examine outcome/response
##################################################

sm.density(city$income)

city %>% 
  summarize(
    M = mean(income), 
    SD = sd(income),
    Med = median(income)
    )



##################################################
### Examine predictor
##################################################

sm.density(city$education, xlab = "Education")

city %>% 
  summarize(
    M = mean(education), 
    SD = sd(education)
    )



##################################################
### Examine conditional distribution of income given edu
##################################################

ggplot(data = city, aes(x = education, y = income)) + 
  geom_point(size = 5) +
  theme_bw() +
  xlab("Education (in years)") +
  ylab("Income (in U.S. dollars)")



##################################################
### Correlation
##################################################

city %>%
  select(income, education) %>%
  correlate()



##################################################
### Regress income on edu
##################################################

lm.1 = lm(income ~ 1 + education, data = city)
lm.1



##################################################
### Using the regression equation
##################################################

# See the 12th observation's data
city %>%
  filter(row_number() == 12)


# Compute the predicted income using the regression equation
11321 + 2651 * 14


# Compute the residual
64926 - 48435



