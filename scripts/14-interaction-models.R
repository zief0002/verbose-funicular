###################################################
### Read in the data
###################################################

beauty = read.csv(file = "/Users/andrewz/Documents/epsy-8251/data/beauty.csv")

head(beauty)
tail(beauty)





###################################################
### Load libraries
###################################################

library(ggplot2)



###################################################
### Interaction of beauty and tenure?
###################################################

ggplot(data = beauty, aes(x = btystdave, y = avgeval, color = factor(tenured))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  scale_color_brewer(palette = "Set1", name = "", labels = c("Non-Tenured", "Tenured")) +
  xlab("Beauty")

# Create product term between beauty and tenure
beauty$int = beauty$btystdave * beauty$tenured

# Fit main-effects model
lm.me = lm(avgeval ~ btystdave + tenured, data = beauty)

# Fit interaction model
lm.int =lm(avgeval ~ btystdave + tenured + int, data = beauty)

# Model comparison using nested F-test
anova(lm.me, lm.int)

# Can also just examine the summary output from the interaction model
summary(lm.int)



###################################################
### Using the : notation
###################################################

beauty = read.csv(file = "/Users/andrewz/Documents/epsy-8251/data/beauty.csv")
head(beauty)

lm.1 = lm(avgeval ~ btystdave + tenured + btystdave:tenured, data = beauty)
summary(lm.1)



###################################################
### Interaction of beauty and tenure after controlling for percentevaluating?
###################################################

lm.2 = lm(avgeval ~ btystdave + tenured + percentevaluating + btystdave:tenured, data = beauty)
summary(lm.2)


# Plot of the model
plotData = expand.grid(
  btystdave = seq(from = -1.5, to = 1.9, by = 0.1),
	tenured = c(0, 1),
  percentevaluating = 74.43
	)

# Use the interaction model fitted with the colon (:)
plotData$yhat = predict(lm.2, newdata = plotData)

plotData$tenured = factor(plotData$tenured,
	levels = c(0, 1),
	labels = c("Non-Tenured", "Tenured")
	)

ggplot(data = plotData, aes(x = btystdave, y = yhat, color = tenured)) +
	geom_line() +
	theme_bw() +
	scale_color_brewer(palette = "Set1", name = "") +
	xlab("Beauty") +
	ylab("Predicted Course Evaluation Score")




###################################################
### Interaction of beauty and tenure after controlling for percentevaluating and female?
###################################################

lm.3 = lm(avgeval ~ btystdave + tenured + percentevaluating + female + btystdave:tenured, data = beauty)
summary(lm.3)


# Plot of the model
plotData = expand.grid(
  btystdave = seq(from = -1.5, to = 1.9, by = 0.1),
  tenured = c(0, 1),
  female = c(0, 1),
  percentevaluating = 74.43
)

# Use the interaction model fitted with the colon (:)
plotData$yhat = predict(lm.3, newdata = plotData)

plotData$tenured = factor(plotData$tenured,
                          levels = c(0, 1),
                          labels = c("Non-Tenured", "Tenured")
  )

plotData$female = factor(plotData$female,
                          levels = c(0, 1),
                          labels = c("Male", "Female")
  )

ggplot(data = plotData, aes(x = btystdave, y = yhat, group = tenured:female, color = tenured)) +
  geom_line() +
  theme_bw() +
  scale_color_brewer(palette = "Set1", name = "") +
  xlab("Beauty") +
  ylab("Predicted Course Evaluation Score") +
  facet_wrap(~female)

# Alternative plot
ggplot(data = plotData, aes(x = btystdave, y = yhat, group = tenured:female, color = tenured, linetype = female)) +
  geom_line() +
  theme_bw() +
  scale_color_brewer(palette = "Set1", name = "") +
  scale_linetype(name = "") +
  xlab("Beauty") +
  ylab("Predicted Course Evaluation Score")



