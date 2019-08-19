##################################################
### Load libraries
##################################################

library(broom)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)



##################################################
### Read in data
##################################################

city = read_csv(file = "~/Documents/github/epsy-8251/data/riverside.csv") 
head(city)



##################################################
### Fit the multiple regression model
##################################################

# Fit mutiple regression model
lm.1 = lm(income ~ 1 + education + seniority, data = city) 

# Coefficient-level information
tidy(lm.1)



##################################################
### Use R to compute predicted values (y-hats)
##################################################

# Create data frame
employees = data.frame( 
  education = c(10, 11, 12), 
  seniority = c(10, 10, 10) 
  )


# View data frame
employees


# Predict
predict(lm.1, newdata = employees)


# Add predicted values as a column in the data frame
employees %>% 
  mutate(
    yhat = predict(lm.1, newdata = employees)
    )



##################################################
### Plot the fitted values from the multiple regression model
### Education on x-axis, three levels of seniority
##################################################

# Find range of education values
summary(city$education)


# Find values for seniority
summary(city$seniority)


# Set up plotting data
plot_data = crossing(
  education = seq(from = 1, to = 24, by = 1), 
  seniority = c(10, 15, 20)
  ) 

# Obtain predicted values and turn seniority into a factor for better plotting
plot_data_2 = plot_data %>%
  mutate(
    yhat = predict(lm.1, newdata = plot_data),
    seniority2 = 
      factor(seniority, 
             levels = c(10, 15, 20), 
             labels = c("Low seniority", "Moderate seniority", "High seniority") 
      )
  ) 


# View data
plot_data_2


# Create the plot
ggplot(data = plot_data_2, aes(x = education, y = yhat, group = seniority2, 
                            color = seniority2)) +
  geom_line() +
  theme_bw() +
  xlab("Education level") +
  ylab("Predicted income") + 
  scale_color_brewer(name = "", palette = "Set1")



##################################################
### Plot the fitted values from the multiple regression model
### Seniority on x-axis, two levels of education
##################################################

crossing(
  seniority = seq(from = 1, to = 27, by = 1), 
  education = c(12, 16)
) %>% 
  mutate(
    yhat = predict(lm.1, newdata = .)
  ) %>%
  mutate( 
    education2 = factor(education, 
                        levels = c(12, 16), 
                        labels = c("High school", "College")
    ) 
  ) %>%
  ggplot(aes(x = seniority, y = yhat, group = education2, color = education2)) +
    geom_line() +
    theme_bw() +
    xlab("Seniority level") +
    ylab("Predicted income") +
    scale_color_brewer(name = "", palette = "Set1")



##################################################
### Plot the fitted values from the multiple regression model
### Seniority on x-axis, one level of education (control)
##################################################

crossing(
  seniority = seq(from = 1, to = 27, by = 1), 
  education = mean(city$education)
  ) %>% 
  mutate( 
    yhat = predict(lm.1, newdata = .) 
  ) %>%
  ggplot(aes(x = seniority, y = yhat)) +
    geom_line() +
    theme_bw() +
    xlab("Seniority level") +
    ylab("Predicted income")
