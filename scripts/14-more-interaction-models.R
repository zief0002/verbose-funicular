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



###################################################
### Relationship between gradRate and SAT
###################################################

ggplot(data = mn, aes(x = sat, y = gradRate)) +
	geom_point(size = 4) +
	geom_smooth(method = "lm", se = FALSE) +
	theme_bw()

lm.1 = lm(gradRate ~ sat, data = mn)
summary(lm.1)



###################################################
### Scale the SAT predictor
###################################################

mn$sat100 = mn$sat / 100
head(mn)


ggplot(data = mn, aes(x = sat100, y = gradRate)) +
	geom_point(size = 4) +
	geom_smooth(method = "lm", se = FALSE) +
	theme_bw()

lm.2 = lm(gradRate ~ sat100, data = mn)
summary(lm.2)




###################################################
### Main-effects model
###################################################

lm.3 = lm(gradRate ~ sat100 + tuition, data = mn)
summary(lm.3)



###################################################
### Plot main-effects model
###################################################

plotData = expand.grid(
	sat100 = seq(from = 8.9, to = 14, by = 0.1),
	tuition = c(21000, 35000, 41000)
	)

# Compute the predicted values
plotData$yhat = predict(lm.3, newdata = plotData)

# Turn tuition into a factor for better plotting
plotData$tuition = factor(plotData$tuition,
	levels = c(21000, 35000, 41000),
	labels = c("25th Percentile", "50th Percentile", "75th Percentile")
	)

# Rescale the sat100 variable back for better interpretation on the plot
plotData$sat = plotData$sat100 * 100

ggplot(data = plotData, aes(x = sat, y = yhat, color = tuition)) +
	geom_line() +
	theme_bw() +
	scale_color_brewer(palette = "Set1", name = "") +
	xlab("Median SAT score") +
	ylab("Predicted graduation rate")


pdf(file = "~/Desktop/plot.pdf")
pdf(file = "~/Desktop/plot.pdf", width = 8, height = 6)
dev.off()




###################################################
### Interaction model: Include both main-effects and the interaction term
###################################################

# intercepts and slopes are different
lm.4 = lm(gradRate ~ sat100 + tuition + sat100:tuition, data = mn)
summary(lm.4)




###################################################
### Alternative method for testing interaction
###################################################

anova(lm.3)
anova(lm.4)


# Nested model (Delta F-test)
anova(lm.3, lm.4)



###################################################
### Plot interaction model
###################################################

plotData = expand.grid(
	sat100 = seq(from = 8.9, to = 14, by = 0.1),
	tuition = c(21000, 35000, 41000)
	)

# Compute the predicted values
plotData$yhat = predict(lm.4, newdata = plotData)

# Turn tuition into a factor for better plotting
plotData$tuition = factor(plotData$tuition,
	levels = c(21000, 35000, 41000),
	labels = c("25th Percentile", "50th Percentile", "75th Percentile")
	)

# Rescale the sat100 variable back for better interpretation on the plot
plotData$sat = plotData$sat100 * 100

# Plot
ggplot(data = plotData, aes(x = sat, y = yhat, color = tuition)) +
	geom_line() +
	theme_bw() +
	scale_color_brewer(palette = "Set1", name = "") +
	xlab("Median SAT score") +
	ylab("Predicted graduation rate")




###################################################
### Census data: Revisited
###################################################

census = read.csv(file = "/Users/andrewz/Documents/EPsy-8262/data/census-sample.csv")
head(census)

# Main-effects model
lm.a = lm(income ~ education + black + hispanic, data = census)

# Interaction model
lm.b = lm(income ~ education + black + hispanic + education:black + education:hispanic, 
	data = census)

# Delta F-test
anova(lm.a, lm.b)



###################################################
### Quadratic effect of SAT on graduation rate?
###################################################

lm.1 = lm(gradRate ~ sat, data = mn)

out1 = fortify(lm.1)

ggplot(data = out1, aes(x = .fitted, y = .stdresid)) +
	geom_point(size = 4) +
	geom_hline(yintercept = 0) +
	geom_smooth(se = FALSE) +
	theme_bw()



###################################################
### Fit quadratic model
###################################################

# Create product term
mn$sat2 = mn$sat * mn$sat
head(mn)

# Fit model (include main-effect and interaction)
lm.q = lm(gradRate ~ sat + sat2, data = mn)
summary(lm.q)

# Fit model (use I() function)
lm.q2 = lm(gradRate ~ sat + I(sat^2), data = mn)
summary(lm.q2)




###################################################
### Plot quadratic model
###################################################

plotData = expand.grid(
	sat = seq(from = 890, to = 1400, by = 10)
	)


# Compute the predicted values
plotData$yhat = predict(lm.q2, newdata = plotData)


# Plot
ggplot(data = plotData, aes(x = sat, y = yhat)) +
	geom_line() +
	theme_bw() +
	xlab("Median SAT score") +
	ylab("Predicted graduation rate")


###################################################
### Quadratic model controlling for sector (public)
###################################################

lm.q3 = lm(gradRate ~ sat + I(sat^2) + public, data = mn)
summary(lm.q3)

# Create plot data
plotData = expand.grid(
	sat = seq(from = 890, to = 1400, by = 10),
	public = c(0, 1)
	)


# Compute the predicted values
plotData$yhat = predict(lm.q3, newdata = plotData)

# Coerce public into a factor for better plotting
plotData$public = factor(plotData$public, labels = c("Private", "Public"))


# Plot
ggplot(data = plotData, aes(x = sat, y = yhat, color = public)) +
	geom_line() +
	theme_bw() +
	xlab("Median SAT score") +
	ylab("Predicted graduation rate") +
	scale_color_brewer(palette = "Set1", name = "")



###################################################
### Interaction with quadratic effect
###################################################

# Interaction with linear term
lm.q4 = lm(gradRate ~ sat + I(sat ^ 2) + public + sat:public, data = mn)

# Interaction with quadratic term
lm.q5 = lm(gradRate ~ sat + I(sat ^ 2) + public + sat:public + I(sat ^ 2):public, data = mn)

# Nested F-test
anova(lm.q3, lm.q4, lm.q5)







