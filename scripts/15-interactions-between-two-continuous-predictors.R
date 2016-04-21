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
### Interaction effect between tuition and SAT?
###################################################

lm.1 = lm(gradRate ~ sat + tuition + sat:tuition, data = mn)
summary(lm.1)



###################################################
### Mean center both predictors
###################################################

mn$sat_c = mn$sat - mean(mn$sat)
mn$tuition_c = mn$tuition - mean(mn$tuition)
head(mn)

# Refit interaction model using centered predictors
lm.2 = lm(gradRate ~ sat_c + tuition_c + sat_c:tuition_c, data = mn)
summary(lm.2)



###################################################
### Mean center AND scale both predictors (z-scores)
###################################################

mn$sat_z = (mn$sat - mean(mn$sat)) / sd(mn$sat)
mn$tuition_z = (mn$tuition - mean(mn$tuition)) / sd(mn$tuition)
head(mn)

# Refit interaction model using z-scores of predictors
lm.3 = lm(gradRate ~ sat_z + tuition_z + sat_z:tuition_z, data = mn)
summary(lm.3)



###################################################
### Plot interaction-effects model
###################################################

plotData = expand.grid(
	sat_z = seq(from = -1.9, to = 2.7, by = 0.1),
	tuition_z = c(-0.96, 0.28, 0.81)
	)

# Compute the predicted values
plotData$yhat = predict(lm.3, newdata = plotData)

# Turn tuition_z into a factor for better plotting
plotData$tuition_z = factor(plotData$tuition_z,
	levels = c(-0.96, 0.28, 0.81),
	labels = c("25th Percentile", "50th Percentile", "75th Percentile")
	)

# Plot
ggplot(data = plotData, aes(x = sat_z, y = yhat, group = tuition_z, color = tuition_z)) +
	geom_line() +
	theme_bw() +
	scale_color_brewer(palette = "Set1", name = "") +
	xlab("Median SAT score") +
	ylab("Predicted graduation rate")




###################################################
### Plot un-centered, un-scaled model
###################################################

plotData = expand.grid(
  sat = seq(from = 890, to = 1400, by = 10),
  tuition = c(21404, 35400, 41420)
)

# Compute the predicted values
plotData$yhat = predict(lm.1, newdata = plotData)

# Turn tuition_z into a factor for better plotting
plotData$tuition = factor(plotData$tuition,
                            levels = c(21404, 35400, 41420),
                            labels = c("25th Percentile", "50th Percentile", "75th Percentile")
)

# Plot
ggplot(data = plotData, aes(x = sat, y = yhat, group = tuition, color = tuition)) +
  geom_line() +
  theme_bw() +
  scale_color_brewer(palette = "Set1", name = "") +
  xlab("Median SAT score") +
  ylab("Predicted graduation rate")




