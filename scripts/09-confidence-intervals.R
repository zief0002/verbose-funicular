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
	edu =  c(10, 11, 12),
	senior =  c(10, 10, 10)
	)

myData



###################################################
### Predict mean GPA for ALL students with HW =1, 2, 3 and parentEd = 12
###################################################

# Point estimate for mean
predict(lm.1, newdata = myData)


# Interval estimate for mean
predict(lm.1, newdata = myData, interval = "confidence")



###################################################
### Plotting confidence envelope for the model
###################################################

# Examine summaries
summary(city)

# Create plotting data
plotData = expand.grid(
	edu =  seq(from = 8, to = 24, by = 1),
	senior =  14.81
	)

plotData


# Predict from newly created data; also obtain CIs
yhat =  predict(lm.1, newdata = plotData, interval = "confidence")
yhat


# Add the y-hat values, the lower limits, and the upper limits to the plotData
plotData = cbind(plotData, yhat)
plotData


# Plot (Take 1)
ggplot(data = plotData, aes(x = edu, y = fit)) +
    geom_line(color = "blue", lwd = 1.5) +
    theme_bw() +
    xlab("Education Level") +
    ylab("Predicted Income") +
    geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey80")


# Plot (Take 2)
ggplot(data = plotData, aes(x = edu, y = fit)) +
  	geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey80") +
    geom_line(color = "blue", lwd = 1.5) +
    theme_bw() +
    xlab("Education Level") +
    ylab("Predicted Income")
    
# Note. With only one predictor (and certain situations with multiple predictors),
# you can use geom_smooth(se = TRUE). But many times this will not work.




###################################################
### Plotting confidence envelope for the model, multiple seniority values
###################################################

# Create plotting data
plotData = expand.grid(
  edu =  seq(from = 8, to = 24, by = 1),
  senior =  c(10, 15, 20)
)

plotData


# Predict from newly created data; also obtain CIs
yhat =  predict(lm.1, newdata = plotData, interval = "confidence")
yhat


# Add the y-hat values, the lower limits, and the upper limits to the plotData
plotData = cbind(plotData, yhat)
plotData


# Turn seniority into a factor for better ggplotting
plotData$senior2 =  factor(plotData$senior, 
    levels = c(10, 15, 20),
    labels = c("Low seniority", "Moderate seniority", "High seniority")
    )

head(plotData)


# Plot
ggplot(data = plotData, aes(x = edu, y = fit, group = senior2)) +
	  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = senior2), alpha = 0.6) +
    geom_line(aes(color = senior2), lwd = 1.5) +
    theme_bw() +
    xlab("Education Level") +
    ylab("Predicted Income") +
    scale_color_brewer(name = "Seniority level", palette = "Set2") +
    scale_fill_brewer(name = "Seniority level", palette = "Set2") +
    facet_wrap(~senior2)

# Note. Since we colored the "color" and the "fill", we need a scale_color_brewer() 
# and a scale_fill_brewer() command. Giving these the same name= argument will result 
# in a single legend that includes color and fill (rather than two legends). 

