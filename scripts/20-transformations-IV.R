
###################################################
### Read in the data
###################################################

earnings = read.csv(file = "/Users/andrewz/Documents/GitHub/EPsy-8262/data/earnings.csv")

head(earnings)
tail(earnings)




###################################################
### Load libraries
###################################################

library(ggplot2)
library(sm)



###################################################
### Log-transform Y
###################################################

earnings$Learn = log(earnings$earn)
head(earnings)


lm.0 = lm(Learn ~ education, data = earnings)
summary(lm.0)



###################################################
### Examine relationship b/w education and log(earnings)
###################################################

ggplot(data = earnings, aes(x = education, y = Learn)) +
	geom_point(size = 4) +
	geom_smooth(se = FALSE) +
	theme_bw()



###################################################
### Examine relationship b/w log(education) and log(earnings)
###################################################

earnings$Leducation = log(earnings$education)
head(earnings)

ggplot(data = earnings, aes(x = Leducation, y = Learn)) +
	geom_point(size = 4) +
	geom_smooth(se = FALSE) +
	theme_bw()




###################################################
### Fit log-log model
###################################################

lm.1 = lm(Learn ~ Leducation, data = earnings)
summary(lm.1)



###################################################
### Check residuals
###################################################

out1 = fortify(lm.1)

sm.density(out1$.stdresid, model = "normal")

ggplot(data = out1, aes(x = .fitted, y = .stdresid)) +
	geom_point(size = 4) +
	geom_hline(yintercept = 0) +
	#geom_smooth(se = FALSE) +
	theme_bw()


###################################################
### Plot of the fitted model
###################################################

plotData = expand.grid(
	Leducation = seq(from = 1.0, to = 2.9, by = 0.1)
	)

# Compute the predicted values
plotData$yhat = predict(lm.1, newdata = plotData)
head(plotData)


# Back-transform logs
plotData$earn = exp(plotData$yhat)
plotData$education = exp(plotData$Leducation)
head(plotData)


# Plot
ggplot(data = plotData, aes(x = education, y = earn)) +
	geom_line() +
	theme_bw() +
	xlab("Education") +
	ylab("Predicted earnings")



lm.2 = lm(Learn ~ education + I(education^2) + I(education^3), data = earnings)

out2 = fortify(lm.2)

sm.density(out2$.stdresid, model = "normal")

ggplot(data = out2, aes(x = .fitted, y = .stdresid)) +
	geom_point(size = 4) +
	geom_hline(yintercept = 0) +
	#geom_smooth(se = FALSE) +
	theme_bw()


plotData = expand.grid(
	education = seq(from = 3.0, to = 18, by = 0.1)
	)

# Compute the predicted values
plotData$yhat = predict(lm.2, newdata = plotData)
head(plotData)


# Back-transform logs
plotData$earn = exp(plotData$yhat)
head(plotData)


# Plot
ggplot(data = plotData, aes(x = education, y = earn)) +
	geom_line() +
	theme_bw() +
	xlab("Education") +
	ylab("Predicted earnings")




