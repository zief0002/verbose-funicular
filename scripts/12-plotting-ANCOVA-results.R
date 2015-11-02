##################################################
### Read in data
##################################################

mn = read.csv(file = "/Users/andrewz/Documents/EPsy-8262/data/mnSchools.csv")

head(mn)
tail(mn)




##################################################
### Examine groups
##################################################

library(psych)
library(ggplot2)
library(sm)


sm.density(mn$public)


##################################################
### Fit ANCOVA model
##################################################

lm.2 = lm(gradRate ~ public + sat, data = mn)
summary(lm.2)






##################################################
### Examine assumptions (lm.2)
##################################################


out2 = fortify(lm.2)

# Normality?
sm.density(out2$.stdresid, model = "normal")

# Residual plot
ggplot(data = out2, aes(x = .fitted, y = .stdresid)) +
    geom_point(size = 4) +
    theme_bw() +
    geom_hline(yintercept = 0) +
    geom_smooth(se = FALSE)

ggplot(data = out2, aes(x = sat, y = .stdresid)) +
    geom_point(size = 4) +
    theme_bw() +
    geom_hline(yintercept = 0) +
    geom_smooth(se = FALSE)

sm.density(mn$gradRate)


ggplot(data = mn, aes(x = sat, y = gradRate)) +
    geom_point(size = 4) +
    theme_bw() +
    facet_wrap(~public) +
    geom_smooth(se = FALSE)




##################################################
### Plot of the lm.2 model
##################################################

# Create new data frame
plotData = expand.grid(
	sat = seq(from = 890, to = 1400, by = 10),
	public = c(0, 1)
	)

head(plotData)

# Predict graduation rates from observations in the new dataframe
plotData$yhat = predict(lm.2, newdata = plotData)
head(plotData)

# Create sector variable which is a factor
plotData$Sector = factor(plotData$public, levels = c(0, 1), labels = c("Private", "Public"))
head(plotData)

# Plot of the model
ggplot(data = plotData, aes(x = sat, y= yhat, group = Sector, color = Sector)) +
	geom_line()


##################################################
### Fit ANCOVA model 2
##################################################

lm.3 = lm(gradRate ~ public + sat + tuition, data = mn)
summary(lm.3)



##################################################
### Examine assumptions (lm.3)
##################################################

out3 = fortify(lm.3)

# Normality?
sm.density(out3$.stdresid, model = "normal")

# Residual plot
ggplot(data = out3, aes(x = .fitted, y = .stdresid)) +
    geom_point(size = 4) +
    theme_bw() +
    geom_hline(yintercept = 0)



##################################################
### Plot of the lm.3 model
##################################################

# Create new data frame
plotData = expand.grid(
	sat = seq(from = 890, to = 1400, by = 10),
	public = c(0, 1),
	tuition = c(21000, 41000)
	)

head(plotData)

# Predict graduation rates from observations in the new dataframe
plotData$yhat = predict(lm.3, newdata = plotData)
head(plotData)

# Create sector variable which is a factor and tuition variable as factor
plotData$Sector = factor(plotData$public, levels = c(0, 1), labels = c("Private", "Public"))

plotData$Tuition = factor(plotData$tuition, levels = c(21000, 41000), labels = c("Low", "High"))

head(plotData)

# Plot of the model
ggplot(data = plotData, aes(x = sat, y= yhat, group = Sector:Tuition, color = Sector)) +
	geom_line() +
	theme_bw() +
	scale_color_brewer(palette = "Set1") +
	xlab("75th Percentile SAT Score") +
	ylab("Predicted Graduation Rate") +
	annotate("text", x = 1175, y = 61, angle = 30, label = "Low Tuition") +
	annotate("text", x = 1150, y = 72, angle = 30, label = "High Tuition")







