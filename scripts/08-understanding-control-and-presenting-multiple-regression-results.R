##################################################
### Read in data
##################################################

city = read.csv(file = "~/Documents/data/Applied-Regression-Lewis-Beck/riverside_final.csv") 

head(city)
tail(city)



##################################################
### Load libraries
##################################################

library(ggplot2)



##################################################
### Fit the multiple regression model
##################################################

lm.1 = lm(income ~ 1 + edu + senior, data = city)
summary(lm.1)




##################################################
### Create data frame to predict from
##################################################

myData = data.frame(
	edu =  c(10, 11, 12),
	senior =  c(10, 10, 10)
	)

myData



##################################################
### Predict income
##################################################

predict(lm.1, newdata = myData)

# Save predictions into a vector
myPreds = predict(lm.1, newdata = myData)

# Append the predictions (as a column) to myData
cbind(myData, myPreds)



##################################################
### Predict income (Youe turn)
##################################################

# set up predictor data frame
myData2 = data.frame(
  edu =  c(16, 16, 16),
  senior =  c(6, 7, 8)
)

myData2

# predict
predict(lm.1, newdata = myData2)

# Save predictions into a vector
myPreds = predict(lm.1, newdata = myData2)

# Append the predictions (as a column) to myData
cbind(myData2, myPreds)



##################################################
### Create plotting data
##################################################

# Examine range of values for the predictors
summary(city)

# Set up data frame of predictors
plotData = expand.grid(
	edu =  seq(from = 1, to = 24, by = 1), # continuous range (on x-axis)
	senior =  c(1, 10, 25)                 # discrete values (different lines)
	)

plotData



##################################################
### Predict from newly created data
##################################################

yhat =  predict(lm.1, newdata = plotData)
plotData = cbind(plotData, yhat)

head(plotData)



##################################################
### Coerce variable with discrete values into a factor for better plotting
##################################################

plotData$senior2 =  factor(plotData$senior, 
    levels = c(1, 10, 25),
    labels = c("Low seniority", "Moderate seniority", "High seniority")
    )

head(plotData)



##################################################
### Create plot
##################################################

ggplot(data = plotData, aes(x = edu, y = yhat, group = senior2)) +
     geom_line(aes(color = senior2)) +
     theme_bw()



##################################################
### Spruce up plot
##################################################

ggplot(data = plotData, aes(x = edu, y = yhat, group = senior2)) +
  geom_line(aes(color = senior2), lwd = 1.5) +
  theme_bw() +
  xlab("Education level") +
  ylab("Predicted income") +
  scale_color_brewer(name = "Seniority level", palette = "Set2")




##################################################
### Plot predicted income as a function of education level controlling for seniority
##################################################

# Compute mean seniority level
mean(city$senior)


# Create plotting data
plotData = expand.grid(
	edu =  seq(from = 8, to = 24, by = 1),  # range of values (on x-axis)
	senior =  c(14.81)
	)

head(plotData)


# Predict GPAs
yhat =  predict(lm.1, newdata = plotData)
plotData = cbind(plotData, yhat)

head(plotData)


# Actual plot
ggplot(data = plotData, aes(x = edu, y = yhat)) +
    geom_line(lwd = 1.5) +
    theme_bw() +
    xlab("Education level") +
    ylab("Predicted income")



