##################################################
### Read in data
##################################################

multReg = read.csv(file = "~/Documents/EPsy-8262/data/homework-education-gpa.csv")

head(multReg)
tail(multReg)



##################################################
### Load libraries
##################################################

library(psych)
library(sm)
library(ggplot2)



##################################################
### Fit the multiple regression model
##################################################

lm.a = lm(gpa ~ homework + parentEd, data = multReg)
summary(lm.a)




##################################################
### Create data frame to predict from
##################################################

myData = data.frame(
	homework =  c(1, 2, 3),
	parentEd =  c(12, 12, 12)
	)

myData

myData = data.frame(
    homework =  c(5, 5, 5),
    parentEd =  c(12, 13, 14)
    )

myData


##################################################
### Predict GPAs
##################################################

predict(lm.a, newdata = myData)

# Save predictions into a vector
myPreds = predict(lm.a, newdata = myData)

# Append the predictions (as a column) to myData
cbind(myData, myPreds)



##################################################
### Create plotting data
##################################################

plotData = expand.grid(
	homework =  seq(from = 1, to = 11, by = 1),
	parentEd =  c(10, 12, 16)
	)

plotData



##################################################
### Predict from newly created data
##################################################

yhat =  predict(lm.a, newdata = plotData)
plotData = cbind(plotData, yhat)

head(plotData)



##################################################
### Coerce variable with discrete values into a factor for better plotting
##################################################

plotData$parentEd2 =  factor(plotData$parentEd, 
    levels = c(10, 12, 16),
    labels = c("Some High School", "High School", "College")
    )

head(plotData)



##################################################
### Create plot
##################################################

ggplot(data = plotData, aes(x = homework, y = yhat, group = parentEd2)) +
     geom_line(aes(color = parentEd2)) +
     theme_bw()



##################################################
### Spruce up plot
##################################################

ggplot(data = plotData, aes(x = homework, y = yhat, group = parentEd2)) +
    geom_line(aes(color = parentEd2), lwd = 1.5) +
    theme_bw() +
    xlab("Time Spent on Homework (Hours per Week)") +
    ylab("Predicted GPA (100-pt Scale)") +
    scale_color_brewer(name = "Parent Level of Education", palette = "Set2")




##################################################
### Plot predicted GPA as a function of parent level of education for two values of homework
##################################################

# Create plotting data
plotData = expand.grid(
	parentEd =  seq(from = 10, to = 20, by = 1),
	homework =  c(1, 10)
	)

head(plotData)


# Predict GPAs
yhat =  predict(lm.a, newdata = plotData)
plotData = cbind(plotData, yhat)

head(plotData)


# Coerce variable with discrete values into a factor for better plotting
plotData$homework2 =  factor(plotData$homework, 
    levels = c(1, 10),
    labels = c("Spends one hour a week on homework", "Spends ten hours a week on homework")
    )

head(plotData)

# Actual plot
ggplot(data = plotData, aes(x = parentEd, y = yhat, group = homework2)) +
    geom_line(aes(color = homework2), lwd = 1.5) +
    theme_bw() +
    xlab("Parent Level of Education (in Years)") +
    ylab("Predicted GPA (100-pt Scale)") +
    scale_color_brewer(name = "", palette = "Set2")



##################################################
### Plot predicted GPA as a function of parent level of education controlling for homework
##################################################

# Create plotting data, choose only one value for the predictor you want to control out. Generally the mean.
plotData = expand.grid(
	parentEd =  seq(from = 10, to = 20, by = 1),
	homework =  5.09
	)

head(plotData)


# Predict GPAs
yhat =  predict(lm.a, newdata = plotData)
plotData = cbind(plotData, yhat)

head(plotData)


# Actual plot
ggplot(data = plotData, aes(x = parentEd, y = yhat)) +
    geom_line(lwd = 1.5) +
    theme_bw() +
    xlab("Parent Level of Education (in Years)") +
    ylab("Predicted GPA (100-pt Scale)")
