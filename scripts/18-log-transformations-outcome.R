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

ggplot(data = earnings, aes(x = education, y = earn)) +
	geom_point(size = 4) +
	geom_smooth(se = FALSE) +
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


# Unsure about non-linearity...fit linear model and examine residuals
lm.1 = lm(earn ~ education, data = earnings)
out = fortify(lm.1)

ggplot(data = out, aes(x = .fitted, y = .stdresid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  theme_bw()



###################################################
### Log-transform Y
###################################################

earnings$Learn = log(earnings$earn)
head(earnings)


# e
exp(1)

#e^6.907755
exp(6.907755)



###################################################
### Relationship between log(earn) and height
###################################################

ggplot(data = earnings, aes(x = education, y = Learn)) +
	geom_point(size = 4) +
	geom_smooth(se = FALSE) +
	theme_bw()



###################################################
### Fit log-linear model
###################################################

lm.2 = lm(Learn ~ education, data = earnings)
summary(lm.2)


# Understanding the model
new = data.frame(education = c(10, 11, 12))
predict(lm.2, newdata = new)
exp(predict(lm.2, newdata = new))

10774.47 * 1.130368 
12179.12 * 1.130368 



###################################################
### Back to the MN schools data
###################################################

mn = read.csv(file = "/Users/andrewz/Documents/EPsy-8262/data/mnSchools.csv")
mn$Lsat = log(mn$sat)
head(mn)

# Fit model
lm.3 = lm(gradRate ~ Lsat, data = mn)
summary(lm.3)

# Understanding the model
new = data.frame(Lsat = log(c(1000, 1010, 1020.1)))
predict(lm.3, newdata = new)

48.40581 - 46.87784
49.93378 - 48.40581

# Direct computation of difference
153.6 * log(1.01)



###################################################
### Plot results from earnings model
###################################################

# Set up data
plotData = expand.grid(
  education = seq(from = 3, to = 18, by = 0.1)
)

# Predict
plotData$yhat = predict(lm.2, newdata = plotData)

# Examine data
head(plotData)

# Back-transform any log terms
plotData$earn = exp(plotData$yhat)

# Re-examine data
head(plotData)

ggplot(data = plotData, aes(x = education, y = earn)) +
  geom_line() +
  theme_bw() +
  xlab("Education") +
  ylab("Predicted Earnings")



###################################################
### Examine residuals of earning model
###################################################

out = fortify(lm.2)

sm.density(out$.stdresid, model = "normal")

ggplot(data = out, aes(x = .fitted, y = .stdresid)) +
	geom_point(size = 4) +
	geom_hline(yintercept = 0) +
	geom_smooth(se = FALSE) +
	theme_bw()



###################################################
### Still non-linearity?
###################################################

ggplot(data = earnings, aes(x = education, y = Learn)) +
  geom_point(size = 4) +
  geom_smooth(se = FALSE) +
  theme_bw()

# Transform predictor as well
earnings$Leducation = log(earnings$education)
head(earnings)

# Fit log-log model
lm.4 = lm(Learn ~ Leducation, data = earnings)
summary(lm.4)



###################################################
### Examine residuals of model
###################################################

out = fortify(lm.4)

ggplot(data = out, aes(x = .fitted, y = .stdresid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  theme_bw()



###################################################
### Plot results of the  model
###################################################

# Set up data
plotData = expand.grid(
  Leducation = seq(from = 1.0, to = 2.9, by = 0.1)
)

# Predict
plotData$yhat = predict(lm.4, newdata = plotData)

# Back-transform any log terms
plotData$education = exp(plotData$Leducation)
plotData$earn = exp(plotData$yhat)

# Examine data
head(plotData)

# Plot
ggplot(data = plotData, aes(x = education, y = earn)) +
  geom_line() +
  theme_bw() +
  xlab("Education") +
  ylab("Predicted Earnings")



###################################################
### Categorical predictor
###################################################

lm.5 = lm(Learn ~ female, data =earnings)
summary(lm.5)



###################################################
### ANCOVA model
###################################################

lm.6 = lm(Learn ~ female + Leducation, data =earnings)
summary(lm.6)



###################################################
### Plot ANCOVA model
###################################################

# Set up data
plotData = expand.grid(
  Leducation = seq(from = 1.0, to = 2.9, by = 0.1),
  female = c(0, 1)
)

# Predict
plotData$yhat = predict(lm.6, newdata = plotData)

# Back-transform any log terms
plotData$education = exp(plotData$Leducation)
plotData$earn = exp(plotData$yhat)

# Turn female into a factor
plotData$female = factor(plotData$female, levels = c(0, 1), labels = c("Male", "Female"))

# Examine data
head(plotData)

# Plot
ggplot(data = plotData, aes(x = education, y = earn, group = female, color = female)) +
  geom_line() +
  theme_bw() +
  xlab("Education") +
  ylab("Earnings") +
  scale_color_brewer(name = "", palette = "Set1")



