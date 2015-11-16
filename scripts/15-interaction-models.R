###################################################
### Read in the data
###################################################

census = read.csv(file = "/Users/andrewz/Documents/EPsy-8262/data/census-sample.csv")

head(census)
tail(census)


census$minority = ifelse(census$ethnicity == "white", 0, 1)



###################################################
### Load libraries
###################################################

library(ggplot2)



###################################################
### Minority diffeences in income?
###################################################

ggplot(data = census, aes(x = factor(minority), y = income, fill = factor(minority))) +
	geom_boxplot() +
	theme_bw() +
	scale_fill_brewer(palette = "Set1", name = "", labels = c("Non-Minority", "Minority")) +
	xlab("Minority Status")


ggplot(data = census, aes(x = factor(minority), y = education, fill = factor(minority))) +
	geom_boxplot() +
	theme_bw() +
	scale_fill_brewer(palette = "Set1", name = "", labels = c("Non-Minority", "Minority")) +
	xlab("Minority Status")

lm.1 = lm(income ~ minority, data = census)
summary(lm.1)



###################################################
### Relationship between income and education
###################################################

# Overall
ggplot(data = census, aes(x = education, y = income)) +
	geom_point(size = 4) +
	geom_smooth(method = "lm", se = FALSE) +
	theme_bw()

lm.education = lm(income ~ education, data = census)
summary(lm.education)







###################################################
### Relationship between income and education controlling for minority
###################################################

# intercepts are different, slopes are the same
lm.2 = lm(income ~ education + minority, data = census)
summary(lm.2)

plotData = expand.grid(
	education = 7:20,
	minority = c(0, 1)
	)

# Use the interaction model fitted with the colon (:)
plotData$yhat = predict(lm.2, newdata = plotData)

plotData$minority = factor(plotData$minority,
	levels = c(0, 1),
	labels = c("Non-Minority", "Minority")
	)

ggplot(data = plotData, aes(x = education, y = yhat, color = minority)) +
	geom_line() +
	theme_bw() +
	scale_color_brewer(palette = "Set1", name = "") +
	xlab("Years of education") +
	ylab("Predicted annual income (in thousands of dollars)")




###################################################
### Is the relationship between income and education different for minorities?
###################################################

ggplot(data = census, aes(x = education, y = income, color = factor(minority))) +
	geom_point(size = 4) +
	#geom_smooth(method = "lm", se = FALSE) +
	theme_bw() +
	scale_color_brewer(palette = "Set1")





###################################################
### Create interaction term
###################################################

census$educMin = census$education * census$minority



###################################################
### Fit model 3: Include both main-effects and the interaction term
###################################################

# intercepts and slopes are different
lm.3 = lm(income ~ education + minority + educMin, data = census)
summary(lm.3)



###################################################
### Fit model 4: Alternative method
###################################################

lm.4 = lm(income ~ education + minority + education:minority, data = census)
summary(lm.4)



###################################################
### Plot model
###################################################

plotData = expand.grid(
	education = 7:20,
	minority = c(0, 1)
	)

# Use the interaction model fitted with the colon (:)
plotData$yhat = predict(lm.4, newdata = plotData)

plotData$minority = factor(plotData$minority,
	levels = c(0, 1),
	labels = c("Non-Minority", "Minority")
	)

ggplot(data = plotData, aes(x = education, y = yhat, color = minority)) +
	geom_line() +
	theme_bw() +
	scale_color_brewer(palette = "Set1", name = "") +
	xlab("Years of education") +
	ylab("Predicted annual income (in thousands of dollars)")



###################################################
### Using three dummy variables to represent ethnicity
###################################################

# Main-effects model
lm.5 = lm(income ~ education + black + hispanic, data = census)
summary(lm.5)




# Interaction model
lm.6 = lm(income ~ education + black + hispanic + education:black + education:hispanic, 
	data = census)
summary(lm.6)

# Interaction model 2
lm.7 = lm(income ~ education + white + hispanic + education:white + education:hispanic, 
	data = census)
summary(lm.7)


plotData = expand.grid(
	black = 0:1,
	hispanic = 0:1,
	education = 7:9
	)



###################################################
### Plot model
###################################################


# Fit model using ethnicity factor
lm.8 = lm(income ~ education + ethnicity + education:ethnicity, data = census)
summary(lm.8)

# Set up plotting data
plotData = expand.grid(
	education = 7:20,
	ethnicity = levels(census$ethnicity)
	)

# Use the interaction model fitted with the colon (:)
plotData$yhat = predict(lm.8, newdata = plotData)

# Plot
ggplot(data = plotData, aes(x = education, y = yhat, color = ethnicity)) +
	geom_line() +
	theme_bw() +
	scale_color_brewer(palette = "Set1", name = "Ethnicity", labels = c("Black", "Hispanic", "White")) +
	xlab("Years of education") +
	ylab("Predicted annual income (in thousands of dollars)") 








