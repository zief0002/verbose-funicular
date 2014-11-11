###################################################
### Read in Data
###################################################

Prestige = read.csv(file =  "/Users/andrewz/Documents/EPSY-8251/data/Prestige.csv")
head(Prestige)


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
sm.density(Prestige$prestige)
describe(Prestige$prestige)

# Predictor
sm.density(Prestige$education)
describe(Prestige$education)




###################################################
### RQ1: Is there an effect of education on occupational prestige?
###################################################

ggplot(data = Prestige, aes(x = education, y = prestige)) +
	geom_point() +
	theme_bw()

cor(Prestige[c("prestige", "education")])

lm.1 = lm(prestige ~ education, data = Prestige)
summary(lm.1)

out1 = fortify(lm.1)

sm.density(out1$.stdresid, model = "normal")

ggplot(data = out1, aes(x = .fitted, y = .stdresid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw()






###################################################
### RQ2: Is there an effect of education on occupational prestige 
### even after controlling for occupation type?
###################################################

table(Prestige$blue_collar)

cor(Prestige[c("prestige", "education", "blue_collar")])

lm.2 = lm(prestige ~ education + blue_collar, data = Prestige)
summary(lm.2)

out2 = fortify(lm.2)

sm.density(out2$.stdresid, model = "normal")

ggplot(data = out2, aes(x = .fitted, y = .stdresid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw()



###################################################
### Plot results from main-effects model
###################################################

# Create data frame with values of education and blue_collar
myData = expand.grid(
  education = seq(from = 6.3, to = 16, by = 0.1),
  blue_collar = c(0, 1)
  )

# Obtain fitted values 
preds = predict(lm.2, newdata = myData)

# Bind the data and predicted values
myData = cbind(myData, preds)

# Change blue_collar to a factor for better plotting
myData$blue_collar = factor(myData$blue_collar,
  levels = c(0, 1),
  labels = c("Blue Collar", "Non Blue-Collar")
  )

# Plot
ggplot(data = myData, aes(x = education, y = preds, color = blue_collar)) +
    geom_line() +
    xlab("Education") +
    ylab("Predicted Occupational Prestige") +
    scale_color_brewer(name = "", palette = "Set1") +
    theme_bw()







###################################################
### Do the data suggest that the effect of education on 
### occupational prestige is the same for all levels of occupation type?
###################################################

ggplot(data = Prestige, aes(x = education, y = prestige)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  facet_wrap(~blue_collar)



###################################################
### Interaction model
###################################################

# Create interaction between education and blue_collar
Prestige$educ_bc = Prestige$education * Prestige$blue_collar
head(Prestige)
tail(Prestige)

# Fit model
lm.3 = lm(prestige ~ education + blue_collar + educ_bc, data = Prestige)
summary(lm.3)




###################################################
### Alternative method to fit interaction model
###################################################

lm.4 = lm(prestige ~ education + blue_collar + education:blue_collar, data = Prestige)


###################################################
### Plot results from interaction model
###################################################

# Create data frame with values of main effects
myData = expand.grid(
  education = seq(from = 0, to = 16, by = 0.1),
  blue_collar = c(0, 1)
  )

# Obtain fitted values (must use the : notation to fit model)
preds = predict(lm.4, newdata = myData)

# Bind the data and predicted values
myData = cbind(myData, preds)

# Change blue_collar to a factor for better plotting
myData$blue_collar = factor(myData$blue_collar,
  levels = c(0, 1),
  labels = c("Non Blue-Collar", "Blue Collar")
  )

# Plot
ggplot(data = myData, aes(x = education, y = preds, color = blue_collar)) +
    geom_line() +
    xlab("Education") +
    ylab("Predicted Occupational Prestige") +
    scale_color_brewer(name = "", palette = "Set1") +
    theme_bw()



###################################################
### RQ3: Is there still an interacion effect between education 
### and occupation type on occupational prestige after controlling 
### for income level?
###################################################

# Income predictor
sm.density(Prestige$income)
describe(Prestige$income)


ggplot(data = Prestige, aes(x = income, y = prestige)) +
  geom_point() +
  theme_bw()

# Log transform income
Prestige$L2income = log(Prestige$income, base = 2)

ggplot(data = Prestige, aes(x = L2income, y = prestige)) +
  geom_point() +
  theme_bw()

cor(Prestige[c("prestige", "education", "blue_collar", "L2income")])

lm.5 = lm(prestige ~ education + blue_collar + L2income + education:blue_collar, data = Prestige)
summary(lm.5)

out5 = fortify(lm.5)

sm.density(out5$.stdresid, model = "normal")

ggplot(data = out5, aes(x = .fitted, y = .stdresid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw()



###################################################
### Plot results from interaction model
###################################################

# Create data frame with values of main effects
myData = expand.grid(
  education = seq(from = 0, to = 16, by = 0.1),
  blue_collar = c(0, 1),
  L2income = c(9, 15)
  )

# Obtain fitted values (must use the : notation to fit model)
preds = predict(lm.5, newdata = myData)

# Bind the data and predicted values
myData = cbind(myData, preds)

# Change blue_collar to a factor for better plotting
myData$blue_collar = factor(myData$blue_collar,
  levels = c(0, 1),
  labels = c("Non Blue-Collar", "Blue Collar")
  )

# Change L2income to a factor for better plotting
myData$L2income = factor(myData$L2income,
  levels = c(9, 15),
  labels = c("Low Income", "High Income")
  )

# Plot
ggplot(data = myData, aes(x = education, y = preds, color = blue_collar, linetype = L2income)) +
    geom_line() +
    xlab("Education") +
    ylab("Predicted Occupational Prestige") +
    scale_color_brewer(name = "", palette = "Set1") +
    scale_linetype(name = "") +
    theme_bw()

# Low income = 2 ^ 9
# High income = 2 ^ 15


pdf(file = "~/Desktop/Plot.pdf", width = 10, height = 6)
pdf(file = "~/Desktop/Plot.pdf")
dev.off()









