###################################################
### Read in Data
###################################################

mn = read.csv(file =  "~/Documents/GitHub/EPsy-8251/data/MN-Colleges-2008.csv")
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
sm.density(mn$expend)
describe(mn$expend)


###################################################
### Conditional distribution of grades
###################################################

library(ggplot2)
ggplot(data = mn, aes(x = expend, y = gradRate)) +
	geom_point() +
	theme_bw()




###################################################
### Correlation b/w grades and hwork
###################################################


cor(mn[c("gradRate", "expend")])



###################################################
### Fit multiple regression model (regress gradRate on tuition and public)
###################################################

lm.1 = lm(gradRate ~ expend, data = mn)

# Get regression output
summary(lm.1)

# Check assumptions
out1 = fortify(lm.1)

sm.density(out1$.stdresid, model = "normal")

ggplot(data = out1, aes(x = .fitted, y = .stdresid)) +
	geom_point() +
	geom_hline(yintercept = 0) +
	#geom_hline(yintercept = c(-2, 2), lty = "dashed") +
	theme_bw()

# Add ID numbers to the fortified data
out1$id = mn$id

# Show ID numbers
ggplot(data = out1, aes(x = .fitted, y = .stdresid)) +
	geom_text(aes(label = id)) +
	geom_hline(yintercept = 0) +
	#geom_hline(yintercept = c(-2, 2), lty = "dashed") +
	theme_bw()	

# Identify ID 14
mn[c(7, 13), ]


###################################################
### Re-fit multiple regression model w/o row 14
###################################################

lm.1 = lm(gradRate ~ expend, data = mn, subset = -c(7, 13))
summary(lm.1)

# Check assumptions
out1 = fortify(lm.1)

sm.density(out1$.stdresid, model = "normal")

ggplot(data = out1, aes(x = .fitted, y = .stdresid)) +
	geom_point() +
	geom_hline(yintercept = 0) +
	#geom_hline(yintercept = c(-2, 2), lty = "dashed") +
	theme_bw()



###################################################
### Alternate Model
###################################################

ggplot(data = mn, aes(x = expend, y = gradRate)) +
	geom_point() +
	geom_smooth(se = FALSE) +
	theme_bw()



# Add a column of the log2 expenditures
mn$L2Expend = log(mn$expend, base = 2)

# Plot the relationship between L2Expend and Gradrate
ggplot(data = mn, aes(x = L10Expend, y = gradRate)) +
    geom_point(size = 3) +
    xlab("Student-Related Expenditures (log2)") +
    ylab("Six-Year Graduation Rate")  +
    theme_bw()

# Fit the model
lm.4 = lm(gradRate ~ L10Expend, data = mn)
summary(lm.4)

out3 = fortify(lm.3)
out3$id = mn$id

# Check assumptions
sm.density(out3$.stdresid, model = "Normal", xlab = "Studentized Residuals")

ggplot(data = out3, aes(x = L2Expend, y = .stdresid)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = -2, lty = "dotted") + 
  geom_hline(yintercept = 2, lty = "dotted") +   
  xlab("Student-Related Expenditures") +
  ylab("Studentized Residuals") + theme_bw() +
  geom_text(data = out3[abs(out3$.stdresid) >= 2 , ], aes(label = id))


###################################################
### Drop observations and refit alternate model
###################################################

mn2 = mn[-c(13, 7), ]
row.names(mn2) = 1:nrow(mn2)

lm.4 = lm(gradRate ~ L2Expend, data = mn2)
summary(lm.4)
confint(lm.4)

out4 = fortify(lm.4)
out4$id = mn2$id

# Check assumptions
sm.density(out4$.stdresid, model = "Normal", xlab = "Studentized Residuals")

ggplot(data = out4, aes(x = L2Expend, y = .stdresid)) +
  geom_point(size = 5) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = -2, lty = "dotted") + 
  geom_hline(yintercept = 2, lty = "dotted") +   
  xlab("Student-Related Expenditures") +
  ylab("Studentized Residuals") + 
  theme_bw() +
  geom_text(data = out4[abs(out4$.stdresid) >= 2 , ], aes(label = id))



###################################################
### Scatterplot with learning curve
###################################################

myCurve = function(x) log(2 ^ -277.4 * x ^ 25.4, base = 2)

ggplot(data = mn, aes(x = expend, y = gradRate)) +
    geom_point(size = 4) +
    stat_function(fun = myCurve, color = "blue") +
    xlab("Student-Related Expenditures") +
    ylab("Graduation Rate") +
    theme_bw()



###################################################
### Scatterplot with learning curve (Alternative method)
###################################################

# Create data frame with values of L2Expend
myData = data.frame(
  L2Expend = seq(from = 12.5, to = 14.7, by = 0.1)
  )

# Obtain fitted values and Confidence Limits
preds = predict(lm.3, newdata = myData, interval = "confidence")

# Bind the L2Expend, fitted values, and Confidence Limits together
myData = cbind(myData, preds)

# Transform the x values back to the raw scale
myData$expend = 2 ^ myData$L2Expend
myData$rawlwr = 2 ^ myData$lwr
myData$rawupr = 2 ^ myData$upr

# Plot
ggplot(data = myData, aes(x = expend, y = fit)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey80") +
    geom_line(color = "blue") +
    xlab("Student-Related Expenditures") +
    ylab("Predicted Graduation Rate") +
    theme_bw()



ggplot(data = myData, aes(x = L2Expend, y = fit)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey80") +
    geom_line(color = "blue") +
    xlab("Student-Related Expenditures") +
    ylab("Predicted Graduation Rate") +
    theme_bw()

















