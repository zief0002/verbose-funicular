##################################################
### Read in data
##################################################

multReg = read.csv(file = "~/Documents/EPsy-8262/data/homework-education-gpa.csv")

head(multReg)
tail(multReg)



###################################################
### Load all needed libraries 
###################################################

library(ggplot2)



##################################################
### Fit the multiple regression model
##################################################

lm.a = lm(gpa ~ homework + parentEd, data = multReg)
summary(lm.a)




###################################################
### Create dataset to predict from
###################################################

myData = data.frame(
	homework =  c(1, 2, 3),
	parentEd =  c(12, 12, 12)
	)

myData




###################################################
### Predict GPA for INDIVIDUAL students with HW =1, 2, 3 and parentEd = 12
###################################################

# Point estimate for GPA
predict(lm.a, newdata = myData)


# Interval estimate for GPA
predict(lm.a, newdata = myData, interval = "prediction")



###################################################
### Plotting prediction envelope for the model
###################################################

# Create plotting data
plotData = expand.grid(
	homework =  seq(from = 1, to = 11, by = 1),
	parentEd =  14.03
	)

plotData


# Predict from newly created data; also obtain PIs
yhat =  predict(lm.a, newdata = plotData, interval = "prediction")
yhat


# Add the y-hat values, the lower limits, and the upper limits to the plotData
plotData = cbind(plotData, yhat)
plotData


# Plot
ggplot(data = plotData, aes(x = homework, y = fit)) +
	geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey80") +
    geom_line(color = "blue", lwd = 1.5) +
    theme_bw() +
    xlab("Time Spent on Homework (Hours per Week)") +
    ylab("Predicted GPA (100-pt Scale)")
    
# Note. With only one predictor (and certain situations with multiple predictors),
# you can use geom_smooth(se = TRUE). But many times this will not work.




###################################################
### Plotting confidence envelope AND prediction envelope for the model
###################################################

# Create plotting data
plotData = expand.grid(
    homework =  seq(from = 1, to = 11, by = 1),
    parentEd =  14.03
    )

plotData


# Predict from newly created data; also obtain PIs
yhat =  predict(lm.a, newdata = plotData, interval = "prediction")
yhat

# Predict from newly created data; also obtain CIs
yhat2 =  predict(lm.a, newdata = plotData, interval = "confidence")
yhat2

# Add the y-hat values, the lower limits, and the upper limits to the plotData
plotData_pi = cbind(plotData, yhat)
plotData_pi

plotData_ci = cbind(plotData, yhat2)
plotData_ci


# Plot
ggplot(data = plotData_pi, aes(x = homework, y = fit)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey80") +
    geom_ribbon(data = plotData_ci, aes(ymin = lwr, ymax = upr), fill = "grey60") +
    geom_line(color = "blue", lwd = 1.5) +
    theme_bw() +
    xlab("Time Spent on Homework (Hours per Week)") +
    ylab("Predicted GPA (100-pt Scale)")

    