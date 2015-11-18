###################################################
### Read in the data
###################################################

earnings = read.csv(file = "/Users/andrewz/Documents/EPsy-8262/data/earnings.csv")

head(earnings)
tail(earnings)




###################################################
### Load libraries
###################################################

library(ggplot2)
library(sm)


###################################################
### Relationship between earn and height
###################################################

ggplot(data = earnings, aes(x = height, y = earn)) +
	geom_point(size = 4) +
	#geom_smooth(se = FALSE) +
	theme_bw()


###################################################
### Fit linear model
###################################################

lm.1 = lm(earn ~ height, data = earnings)

# Check residuals
out1 = fortify(lm.1)

sm.density(out1$.stdresid, model = "normal")

ggplot(data = out1, aes(x = .fitted, y = .stdresid)) +
	geom_point(size = 4) +
	geom_hline(yintercept = 0) +
	#geom_smooth(se = FALSE) +
	theme_bw()



###################################################
### Log-transform Y
###################################################

earnings$Learn = log(earnings$earn)
head(earnings)



###################################################
### Relationship between log(earn) and height
###################################################

ggplot(data = earnings, aes(x = height, y = Learn)) +
	geom_point(size = 4) +
	#geom_smooth(se = FALSE) +
	theme_bw()



###################################################
### Fit log-linear model
###################################################

lm.2 = lm(Learn ~ height, data = earnings)

# Check residuals
out2 = fortify(lm.2)

sm.density(out2$.stdresid, model = "normal")

ggplot(data = out2, aes(x = .fitted, y = .stdresid)) +
	geom_point(size = 4) +
	geom_hline(yintercept = 0) +
	#geom_smooth(se = FALSE) +
	theme_bw()



###################################################
### Examine model
###################################################

summary(lm.2)




###################################################
### Plot of the fitted model
###################################################

plotData = expand.grid(
	height = seq(from = 58, to = 77, by = 1)
	)

# Compute the predicted values
plotData$yhat = predict(lm.2, newdata = plotData)
head(plotData)


# Back-transform logs
plotData$earn = exp(plotData$yhat)
head(plotData)


# Plot
ggplot(data = plotData, aes(x = height, y = earn)) +
	geom_line() +
	theme_bw() +
	xlab("Height (in inches)") +
	ylab("Predicted earnings")










