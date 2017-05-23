##################################################
### Read in data
##################################################

city = read.csv(file = "~/Documents/data/Applied-Regression-Lewis-Beck/riverside_final.csv") 

head(city)
tail(city)



###################################################
### Load all needed libraries 
###################################################

library(ggplot2)



##################################################
### Fit the multiple regression model
##################################################

lm.1 = lm(income ~ 1 + edu + senior, data = city)
summary(lm.1)




###################################################
### Create dataset to predict from
###################################################

myData = data.frame(
	edu =  c(8, 12, 16),
	senior =  c(10, 10, 10)
	)

myData




###################################################
### Predict GPA for INDIVIDUAL students with HW =1, 2, 3 and parentEd = 12
###################################################

# Point estimate for GPA
predict(lm.1, newdata = myData)


# Interval estimate for GPA
predict(lm.1, newdata = myData, interval = "prediction")



###################################################
### Plotting prediction envelope for the model
###################################################

# Create plotting data
plotData = expand.grid(
	edu =  seq(from = 8, to = 24, by = 1),
	senior =  14.81
	)

plotData


# Predict from newly created data; also obtain PIs
yhat =  predict(lm.1, newdata = plotData, interval = "prediction")
yhat


# Add the y-hat values, the lower limits, and the upper limits to the plotData
plotData = cbind(plotData, yhat)
plotData


# Plot
ggplot(data = plotData, aes(x = edu, y = fit)) +
  	geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey80") +
    geom_line(color = "blue", lwd = 1.5) +
    theme_bw() +
    xlab("Education Level") +
    ylab("Predicted Income")
    
# Note. With only one predictor (and certain situations with multiple predictors),
# you can use geom_smooth(se = TRUE). But many times this will not work.




###################################################
### Plotting confidence envelope AND prediction envelope for the model
###################################################

# Create plotting data
plotData = expand.grid(
  edu =  seq(from = 8, to = 24, by = 1),
  senior =  14.81
)

plotData


# Predict from newly created data; also obtain PIs
yhat =  predict(lm.1, newdata = plotData, interval = "prediction")
yhat

# Predict from newly created data; also obtain CIs
yhat2 =  predict(lm.1, newdata = plotData, interval = "confidence")
yhat2

# Add the y-hat values, the lower limits, and the upper limits to the plotData
plotData_pi = cbind(plotData, yhat)
plotData_pi

plotData_ci = cbind(plotData, yhat2)
plotData_ci


# Plot
ggplot(data = plotData_pi, aes(x = edu, y = fit)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey80") +
    geom_ribbon(data = plotData_ci, aes(ymin = lwr, ymax = upr), fill = "grey60") +
    geom_line(color = "blue", lwd = 1.5) +
    theme_bw() +
    xlab("Education Level") +
    ylab("Predicted Income")

    