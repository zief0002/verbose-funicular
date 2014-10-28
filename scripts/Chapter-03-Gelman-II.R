###################################################
### Read in Data
###################################################

mn = read.csv(file = "~/Documents/GitHub/EPsy-8251/data/mnSchoolsClean.csv")
head(mn)


###################################################
### Libraries
###################################################

library(ggplot2)
library(sm)
library(psych)



###################################################
### Marginal distributions
###################################################

# Outcome
sm.density(mn$gradRate)
describe(mn$gradRate)

# Predictor
sm.density(mn$tuition)
describe(mn$tuition)


###################################################
### Conditional distribution of grades
###################################################

library(ggplot2)
ggplot(data = mn, aes(x = tuition, y = gradRate)) +
	geom_point() +
	theme_bw()


ggplot(data = mn, aes(x = tuition, y = gradRate, color = sector)) +
	geom_point() +
	theme_bw()


###################################################
### Correlation b/w grades and hwork
###################################################

# Create dummy predictor for sector
mn$public = ifelse(mn$sector == "Public", 1, 0)


cor(mn[c("gradRate", "tuition", "public")])



###################################################
### Fit multiple regression model (regress gradRate on tuition and public)
###################################################

lm.1 = lm(gradRate ~ tuition + public, data = mn)

# Get regression output
summary(lm.1)

# Check assumptions
out1 = fortify(lm.1)

sm.density(out1$.stdresid, model = "normal")

ggplot(data = out1, aes(x = .fitted, y = .stdresid)) +
	geom_point() +
	geom_hline(yintercept = 0) +
	geom_hline(yintercept = c(-2, 2), lty = "dashed") +
	theme_bw()

# Add ID numbers to the fortified data
out1$id = mn$id

# Show ID numbers
ggplot(data = out1, aes(x = .fitted, y = .stdresid)) +
	geom_text(aes(label = id)) +
	geom_hline(yintercept = 0) +
	geom_hline(yintercept = c(-2, 2), lty = "dashed") +
	theme_bw()	

# Identify ID 14
mn[14, ]


###################################################
### Re-fit multiple regression model w/o row 14
###################################################

lm.1 = lm(gradRate ~ tuition + public, data = mn, subset = -c(14))
summary(lm.1)

# Check assumptions
out1 = fortify(lm.1)

sm.density(out1$.stdresid, model = "normal")

ggplot(data = out1, aes(x = .fitted, y = .stdresid)) +
	geom_point() +
	geom_hline(yintercept = 0) +
	geom_hline(yintercept = c(-2, 2), lty = "dashed") +
	theme_bw()



###################################################
### Be careful....
###################################################

# Add ID numbers to the fortified data
out1$id = mn$id

# Add ID numbers to the fortified data (for realz this time)
out1$id = mn$id[-c(14)]

# Show ID numbers
ggplot(data = out1, aes(x = .fitted, y = .stdresid)) +
	geom_text(aes(label = id)) +
	geom_hline(yintercept = 0) +
	geom_hline(yintercept = c(-2, 2), lty = "dashed") +
	theme_bw()	

# Identify id 8
mn[8, ]

# Get regression output
summary(lm.1)



###################################################
### Making predictions
###################################################

myData = data.frame(
	tuition = 20000,
	public = 1
	)

predict(lm.1, newdata = myData)

# Prediction interval includes sampling error and "individual" differences
predict(lm.1, newdata = myData, interval = "prediction")



###################################################
### Making predictions (Part II)
###################################################

myData = expand.grid(
	tuition = c(20000, 30000),
	public = c(0, 1)
	)
myData


yhat = predict(lm.1, newdata = myData)
yhat

myData = cbind(myData, yhat)
myData

# Prediction interval includes sampling error and "individual" differences
predict(lm.1, newdata = myData, interval = "prediction")







