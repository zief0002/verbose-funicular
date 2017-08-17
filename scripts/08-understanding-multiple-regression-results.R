##################################################
### Load libraries
##################################################

library(dplyr)
library(ggplot2)
library(readr)



##################################################
### Read in data
##################################################

city = read_csv(file = "~/Dropbox/epsy-8251/data/riverside.csv") 
head(city)



##################################################
### Fit the multiple regression model
##################################################

lm.1 = lm(income ~ 1 + education + seniority, data = city) 
summary(lm.1)



##################################################
### Use R to compute predicted values (y-hats)
##################################################

# Create data frame
myData = data.frame( 
  education = c(10, 11, 12), 
  seniority = c(10, 10, 10) 
  )


# View data frame
myData


# Predict
predict(lm.1, newdata = myData)


# Add predicted values as a column in the data frame
myData = myData %>% 
  mutate(yhat = predict(lm.1, newdata = myData))



##################################################
### Plot the fitted values from the multiple regression model
### Education on x-axis, three levels of seniority
##################################################

# Find range of education values
summary(city$education)


# Find values for seniority
summary(city$seniority)


# Set up plotting data
plotData = expand.grid(
  education = seq(from = 1, to = 24, by = 1), 
  seniority = c(10, 15, 20)
)


# View data
plotData


# Get predicted values and append them to data frame
plotData = plotData %>% mutate( yhat = predict(lm.1, newdata = plotData) ) 
head(plotData)


# Turn seniority into a factor for better plotting
plotData = plotData %>% 
  mutate(seniority2 = 
    factor(seniority, 
      levels = c(10, 15, 20), 
      labels = c("Low seniority", "Moderate seniority", "High seniority" ) 
      ) 
    )
  
head(plotData)


# Create the plot
ggplot(data = plotData, aes(x = education, y = yhat, group = seniority2, 
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

expand.grid(
  seniority = seq(from = 1, to = 27, by = 1), 
  education = c(12, 16)
  ) %>%
  mutate( yhat = predict(lm.1, newdata = .) ) %>% 
  mutate( education2 = factor(education, 
      levels = c(12, 16), labels = c("High school", "College" ))) %>%
  ggplot(data = ., aes(x = seniority, y = yhat, group = education2, color = education2)) +
    geom_line() +
    theme_bw() +
    xlab("Seniority level") +
    ylab("Predicted income") + 
    scale_color_brewer(name = "", palette = "Set1")



##################################################
### Plot the fitted values from the multiple regression model
### Seniority on x-axis, one level of education (control)
##################################################

expand.grid(
  seniority = seq(from = 1, to = 27, by = 1), 
  education = mean(city$education)
  ) %>%
  mutate( yhat = predict(lm.1, newdata = .) ) %>% 
  ggplot(data = ., aes(x = seniority, y = yhat)) +
    geom_line() +
    theme_bw() + 
    xlab("Seniority level") + 
    ylab("Predicted income")

