###################################################
### Read in the data
###################################################

mn = read.csv(file = "/Users/andrewz/Documents/EPsy-8262/data/mnSchools.csv")

head(mn)
tail(mn)




###################################################
### Load libraries
###################################################

library(ggplot2)
library(sm)



###################################################
### Relationship between gradRate and SAT
###################################################

ggplot(data = mn, aes(x = sat, y = gradRate)) +
	geom_point(size = 4) +
	geom_smooth(se = FALSE) +
	geom_smooth(method = "lm", se = FALSE, color = "maroon") +
	theme_bw()



###################################################
### Create log base-2 predictor
###################################################

mn$L2sat = log(mn$sat, base = 2)
head(mn)


ggplot(data = mn, aes(x = L2sat, y = gradRate)) +
	geom_point(size = 4) +
	geom_smooth(se = FALSE) +
	#geom_smooth(method = "lm", se = FALSE, color = "maroon") +
	theme_bw()



###################################################
### Fit log model
###################################################

lm.1 = lm(gradRate ~ L2sat, data = mn)
summary(lm.1)



###################################################
### Examine residuals
###################################################

out.1 = fortify(lm.1)

sm.density(out.1$.stdresid, model = "normal")

ggplot(data = out.1, aes(x = .fitted, y = .stdresid)) +
	geom_point(size = 4) +
	geom_hline(yintercept = 0) +
	theme_bw()



###################################################
### Plot the log base-2 model
###################################################

# Set up data
plotData = expand.grid(
    L2sat = seq(from = 9.80, to = 10.5, by = 0.1)
    )

# Predict
plotData$yhat = predict(lm.1, newdata = plotData)

# Examine data
head(plotData)

# Back-transform any log terms
plotData$sat = 2 ^ plotData$L2sat

# Re-examine data
head(plotData)

# Plot
ggplot(data = plotData, aes(x = sat, y = yhat)) +
	geom_line() +
	theme_bw()



###################################################
### Base-10 Log
###################################################

# Create log base-10 predictor
mn$L10sat = log(mn$sat, base = 10)

# Fit model
lm.2 = lm(gradRate ~ L10sat, data = mn)
summary(lm.2)

# Examine residuals
out.2 = fortify(lm.2)
sm.density(out.2$.stdresid, model = "normal")

ggplot(data = out.2, aes(x = .fitted, y = .stdresid)) +
	geom_point(size = 4) +
	geom_hline(yintercept = 0) +
	theme_bw()



###################################################
### Compare residuals between the two log models
###################################################

head(out.1) # base-2 residuals
head(out.2) # base-10 residuals




###################################################
### Plot log base-10 model
###################################################

# Set up data
plotData = expand.grid(
    L10sat = seq(from = 2.95, to = 3.15, by = 0.01)
    )

# Predict
plotData$yhat = predict(lm.2, newdata = plotData)

# Back-transform the logs
plotData$sat = 10 ^ plotData$L10sat

# Examine data
head(plotData)

# Plot
ggplot(data = plotData, aes(x = sat, y = yhat)) +
	geom_line() +
	theme_bw()




###################################################
### Quadratic model
###################################################

lm.3 = lm(gradRate ~ sat + I(sat^2), data = mn)
summary(lm.3)

# Check residuals
out.3 = fortify(lm.3)
sm.density(out.3$.stdresid, model = "normal")

ggplot(data = out.3, aes(x = .fitted, y = .stdresid)) +
	geom_point(size = 4) +
	geom_hline(yintercept = 0) +
	theme_bw()

