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
### Log-transform Y
###################################################

earnings$Learn = log(earnings$earn)
head(earnings)




###################################################
### Relationship between log(earn) and height and sex
###################################################

ggplot(data = earnings, aes(x = height, y = Learn, color = factor(female))) +
	geom_point(size = 4) +
	geom_smooth(method = "lm", se = FALSE) +
	theme_bw()



###################################################
### Fit model with height and female
###################################################

lm.1 = lm(Learn ~ height + female, data = earnings)
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
	height = seq(from = 58, to = 77, by = 1),
	female = c(0, 1)
	)

# Compute the predicted values
plotData$yhat = predict(lm.1, newdata = plotData)
head(plotData)


# Back-transform logs
plotData$earn = exp(plotData$yhat)
head(plotData)

# Turn female into a factor for better plotting
plotData$female = factor(plotData$female, levels = c(0, 1), labels = c("Males", "Females"))
head(plotData)

# Plot
ggplot(data = plotData, aes(x = height, y = earn, color = female)) +
	geom_line() +
	theme_bw() +
	xlab("Height (in inches)") +
	ylab("Predicted earnings") +
	scale_color_brewer(palette = "Set1", name = "")








lm.1 = lm(Learn ~ female + education + female:education, data = earnings)

summary(lm.1)


plotData = expand.grid(
	education = 3:18,
	female = c(0, 1)
	)

# Compute the predicted values
plotData$yhat = predict(lm.1, newdata = plotData)
head(plotData)


# Back-transform logs
plotData$earn = exp(plotData$yhat)
head(plotData)

# Turn female into a factor for better plotting
plotData$female = factor(plotData$female, levels = c(0, 1), labels = c("Males", "Females"))
head(plotData)

# Plot
ggplot(data = plotData, aes(x = education, y = earn, color = female)) +
	geom_line() +
	theme_bw() +
	xlab("Height (in inches)") +
	ylab("Predicted earnings") +
	scale_color_brewer(palette = "Set1", name = "")









