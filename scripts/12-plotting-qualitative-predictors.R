###################################################
### Read in the data
###################################################

cehd = read.csv(file = "~/Documents/github/EPsy-8251/data/cehd_pay.csv")
head(cehd)



##################################################
### Load libraries
##################################################

library(ggplot2)



##################################################
### Fit regression model using factor (ANCOVA)
##################################################

lm.2 = lm(pay ~ dept + experience, data = cehd)





##################################################
### Create new plotting data
##################################################

plotData = expand.grid(
	experience = seq(from = 0, to = 47, by = 1),
	dept = levels(cehd$dept)
	)
head(plotData)

# Predict y-hat values
plotData$yhat = predict(lm.2, newdata = plotData)
head(plotData)



##################################################
### Plot
##################################################

ggplot(data = plotData, aes(x = experience, y = yhat, group = dept, color = dept)) +
	geom_line() +
	theme_bw() +
	scale_color_brewer(name = "Department", palette = "Set1") +
	xlab("Years of Experience") +
	ylab("Pay")





##################################################
### Department differences controlling for experience and title
##################################################

# Fit regression model using factor (not dummies)
lm.3 = lm(pay ~ dept + experience + title, data = cehd)


# Create new plotting data
plotData = expand.grid(
  experience = seq(from = 0, to = 47, by = 1),
  dept = levels(cehd$dept),
  title = levels(cehd$title)
)

# Predict y-hat values
plotData$yhat = predict(lm.3, newdata = plotData)

# Examine data
head(plotData)

# Plot (Option 1)
ggplot(data = plotData, aes(x = experience, y = yhat, group = dept:title, color = dept, linetype = title)) +
  geom_line() +
  theme_bw() +
  scale_color_brewer(name = "Department", palette = "Set1") +
  xlab("Years of Experience") +
  ylab("Pay")


# Plot (Option 2)
ggplot(data = plotData, aes(x = experience, y = yhat, group = dept, color = dept)) +
  geom_line() +
  theme_bw() +
  scale_color_brewer(name = "Department", palette = "Set1") +
  xlab("Years of Experience") +
  ylab("Pay") +
  facet_wrap(~title)
