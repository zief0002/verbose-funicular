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
	theme_bw()



###################################################
### Fit linear model
###################################################

lm.1 = lm(gradRate ~ sat, data = mn)
summary(lm.1)

# Examine residuals
out = fortify(lm.1)

sm.density(out$.stdresid, model = "normal")

ggplot(data = out, aes(x = .fitted, y = .stdresid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  theme_bw()



###################################################
### Create quadratic polynomial term
###################################################

mn$sat_quadratic = mn$sat * mn$sat
head(mn)



###################################################
### Fit quadratic model
###################################################

lm.1 = lm(gradRate ~ sat + sat_quadratic, data = mn)
summary(lm.1)



###################################################
### Re-fit quadratic model using I() function
###################################################

lm.2 = lm(gradRate ~ sat + I(sat ^ 2), data = mn)
summary(lm.2)



###################################################
### Plot quadratic model
###################################################

# Set up data
plotData = expand.grid(
  sat = seq(from = 890, to = 1400, by = 10)
  )

# Predict
plotData$yhat = predict(lm.2, newdata = plotData)

# Examine data
head(plotData)

# Plot
ggplot(data = plotData, aes(x = sat, y = yhat)) +
  geom_line() +
  theme_bw() +
  xlab("SAT Score") +
  ylab("Predicted Graduation Rate")



###################################################
### Examine residuals from quadratic model
###################################################

out2 = fortify(lm.2)

# Check normality
sm.density(out2$.stdresid, model = "normal")

# Check other assumptions
ggplot(data = out2, aes(x = .fitted, y = .stdresid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  theme_bw()



###################################################
### Add additional predictors
###################################################

lm.3 = lm(gradRate ~ sat + I(sat ^ 2) + public, data = mn)
summary(lm.3)



###################################################
### Plot model results
###################################################

# Set up data
plotData = expand.grid(
  sat = seq(from = 890, to = 1400, by = 10),
  public = c(0, 1)
)

# Predict
plotData$yhat = predict(lm.3, newdata = plotData)

# Coerce public into a factor for better plotting
plotData$public = factor(plotData$public, levels = c(0, 1), labels = c("Private", "Public"))

# Examine data
head(plotData)

# Plot
ggplot(data = plotData, aes(x = sat, y = yhat, group = public, color = public)) +
  geom_line() +
  theme_bw() +
  xlab("SAT Score") +
  ylab("Predicted Graduation Rate") +
  scale_color_brewer(name = "Sector", palette = "Set1")