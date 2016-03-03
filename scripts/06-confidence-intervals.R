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
### Predict mean GPA for ALL students with HW =1, 2, 3 and parentEd = 12
###################################################

# Point estimate for mean
predict(lm.a, newdata = myData)


# Interval estimate for mean
predict(lm.a, newdata = myData, interval = "confidence")



###################################################
### Plotting confidence envelope for the model
###################################################

# Create plotting data
plotData = expand.grid(
	homework =  seq(from = 1, to = 11, by = 1),
	parentEd =  14.03
	)

plotData


# Predict from newly created data; also obtain CIs
yhat =  predict(lm.a, newdata = plotData, interval = "confidence")
yhat


# Add the y-hat values, the lower limits, and the upper limits to the plotData
plotData = cbind(plotData, yhat)
plotData


# Plot (Take 1)
ggplot(data = plotData, aes(x = homework, y = fit)) +
    geom_line(color = "blue", lwd = 1.5) +
    theme_bw() +
    xlab("Time Spent on Homework (Hours per Week)") +
    ylab("Predicted GPA (100-pt Scale)") +
    geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey80")


# Plot (Take 2)
ggplot(data = plotData, aes(x = homework, y = fit)) +
	geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey80") +
    geom_line(color = "blue", lwd = 1.5) +
    theme_bw() +
    xlab("Time Spent on Homework (Hours per Week)") +
    ylab("Predicted GPA (100-pt Scale)")
    
# Note. With only one predictor (and certain situations with multiple predictors),
# you can use geom_smooth(se = TRUE). But many times this will not work.




###################################################
### Plotting confidence envelope for the model, multiple parentEd values
###################################################

# Create plotting data
plotData = expand.grid(
	homework =  seq(from = 1, to = 11, by = 1),
	parentEd =  c(10, 12, 16)
	)

plotData


# Predict from newly created data; also obtain CIs
yhat =  predict(lm.a, newdata = plotData, interval = "confidence")
yhat


# Add the y-hat values, the lower limits, and the upper limits to the plotData
plotData = cbind(plotData, yhat)
plotData


# Turn parentEd into a factor for better ggplotting
plotData$parentEd2 =  factor(plotData$parentEd, 
    levels = c(10, 12, 16),
    labels = c("Some High School", "High School", "College")
    )

head(plotData)


# Plot
ggplot(data = plotData, aes(x = homework, y = fit, group = parentEd2)) +
	geom_ribbon(aes(ymin = lwr, ymax = upr, fill = parentEd2), alpha = 0.6) +
    geom_line(aes(color = parentEd2), lwd = 1.5) +
    theme_bw() +
    xlab("Time Spent on Homework (Hours per Week)") +
    ylab("Predicted GPA (100-pt Scale)") +
    scale_color_brewer(name = "Parent Level of Education", palette = "Set2") +
    scale_fill_brewer(name = "Parent Level of Education", palette = "Set2") +
    facet_wrap(~parentEd2)

# Note. Since we colored the "color" and the "fill", we need a scale_color_brewer() 
# and a scale_fill_brewer() command. Giving these the same name= argument will result 
# in a single legend that includes color and fill (rather than two legends). 

