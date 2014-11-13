###################################################
### Read in Data (Prestige2.csv)
###################################################

Prestige = read.csv(file =  "/Users/andrewz/Documents/EPSY-8251/data/Prestige2.csv")
head(Prestige)

# Log transform income
Prestige$L2income = log(Prestige$income, base = 2)


###################################################
### Libraries
###################################################

library(ggplot2)
library(sm)
library(psych)


###################################################
### Occupation type
###################################################

table(Prestige$type)




###################################################
### RQ1: Is there an effect of education on occupational prestige
### controlling for occupation type and income level?
###################################################


# Create dummy variables
Prestige$prof = ifelse(Prestige$type == "prof", 1, 0)
Prestige$bc = ifelse(Prestige$type == "bc", 1, 0)
Prestige$wc = ifelse(Prestige$type == "wc", 1, 0)

summary(Prestige[c("bc", "wc", "prof")])

cor(Prestige[c("prestige", "education", "L2income", "bc", "wc", "prof")])

# Fit main-effects model (controlling for L2income)
lm.bc = lm(prestige ~ L2income + wc + prof + education, data = Prestige)
summary(lm.bc)


###################################################
### Plot results from main-effects model
###################################################

# Fit model using type (factor rather than dummy predictors)
lm.bc2 = lm(prestige ~ L2income + type + education, data = Prestige)
summary(lm.bc2)

# Obtain the contrast matrix for the dummy variables
contrasts(Prestige$type)

# Create data frame with values of education, L2income, and type
myData = expand.grid(
  education = seq(from = 6.3, to = 16, by = 0.1),
  L2income = mean(Prestige$L2income),
  type = c("bc", "wc", "prof")
  )

# Or....
myData = expand.grid(
  education = seq(from = 6.3, to = 16, by = 0.1),
  L2income = mean(Prestige$L2income),
  type = c(levels(Prestige$type))
  )


# Obtain fitted values 
preds = predict(lm.bc2, newdata = myData)

# Bind the data and predicted values
myData = cbind(myData, preds)


# Plot
ggplot(data = myData, aes(x = education, y = preds, color = type)) +
    geom_line() +
    xlab("Education") +
    ylab("Predicted Occupational Prestige") +
    scale_color_brewer(
      name = "Occupation Type", 
      labels = c("Blue-Collar", "White-Collar", "Professional"), 
      palette = "Set1") +
    theme_bw()


###################################################
### Changing the reference group
###################################################

# Change the reference group
contrasts(Prestige$type) = contr.treatment(levels(Prestige$type), base = 2)
contrasts(Prestige$type)

lm.bc3 = lm(prestige ~ L2income + type + education, data = Prestige)
summary(lm.bc3)

# Set up new data to plot
myData = expand.grid(
  education = seq(from = 6.3, to = 16, by = 0.1),
  L2income = mean(Prestige$L2income),
  type = c(levels(Prestige$type))
  )

# Obtain fitted values 
preds = predict(lm.bc3, newdata = myData)

# Bind the data and predicted values
myData = cbind(myData, preds)

# Plot
ggplot(data = myData, aes(x = education, y = preds, color = type)) +
    geom_line() +
    xlab("Education") +
    ylab("Predicted Occupational Prestige") +
    scale_color_brewer(
      name = "Occupation Type", 
      labels = c("Blue-Collar", "White-Collar", "Professional"), 
      palette = "Set1") +
    theme_bw()



###################################################
### Changing the type of contrasts (effect coding)
###################################################

# Change the type of contrast matrix to effects coding
contrasts(Prestige$type) = contr.sum(levels(Prestige$type))
contrasts(Prestige$type)

lm.bc4 = lm(prestige ~ L2income + type + education, data = Prestige)
summary(lm.bc4)

# Set up new data to plot
myData = expand.grid(
  education = seq(from = 6.3, to = 16, by = 0.1),
  L2income = mean(Prestige$L2income),
  type = c(levels(Prestige$type))
  )

# Obtain fitted values 
preds = predict(lm.bc4, newdata = myData)

# Bind the data and predicted values
myData = cbind(myData, preds)

# Plot
ggplot(data = myData, aes(x = education, y = preds, color = type)) +
    geom_line() +
    xlab("Education") +
    ylab("Predicted Occupational Prestige") +
    scale_color_brewer(
      name = "Occupation Type", 
      labels = c("Blue-Collar", "White-Collar", "Professional"), 
      palette = "Set1") +
    theme_bw()




###################################################
### Do the data suggest that the effect of education on 
### occupational prestige is the same for all levels of occupation type?
###################################################

ggplot(data = Prestige, aes(x = education, y = prestige)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  facet_wrap(~type)



###################################################
### RQ2: Is there an interaction effect between education and
### occupation type on occupational prestige, controlling for
### level of income?
###################################################

# Change the contrasts to dummy coding with bc as the reference group
contrasts(Prestige$type) = contr.treatment(levels(Prestige$type))
contrasts(Prestige$type)


# Fit interaction model
lm.bc.int = lm(prestige ~ L2income + education + type + education:type, data = Prestige)
summary(lm.bc.int)

# Using dummy variables
lm.bc.int = lm(prestige ~ L2income + education + wc + prof + education:wc + education:prof, data = Prestige)
summary(lm.bc.int)



###################################################
### Delta F-test
###################################################

# Fit main-effects model
lm.bc.me = lm(prestige ~ L2income + education + type, data = Prestige)

# Fit interaction model
lm.bc.int = lm(prestige ~ L2income + education + type + education:type, data = Prestige)

# Delta F-test
anova(lm.bc.me, lm.bc.int)



###################################################
### Is the effect of education for white-collar occupations 
### different than the effect of education for professional occupations?
###################################################

# Change the contrasts to dummy coding with bc as the reference group
contrasts(Prestige$type) = contr.treatment(levels(Prestige$type), base = 3)
contrasts(Prestige$type)


# Fit interaction model
lm.wc.int = lm(prestige ~ L2income + education + type + education:type, data = Prestige)
summary(lm.wc.int)

# Set up new data to plot
myData = expand.grid(
  education = seq(from = 6.3, to = 16, by = 0.1),
  L2income = mean(Prestige$L2income),
  type = c(levels(Prestige$type))
  )

# Obtain fitted values 
preds = predict(lm.wc.int, newdata = myData)

# Bind the data and predicted values
myData = cbind(myData, preds)

# Plot
ggplot(data = myData, aes(x = education, y = preds, color = type)) +
    geom_line() +
    xlab("Education") +
    ylab("Predicted Occupational Prestige") +
    scale_color_brewer(
      name = "Occupation Type", 
      labels = c("Blue-Collar", "White-Collar", "Professional"), 
      palette = "Set1") +
    theme_bw()



###################################################
### Interpreting the summary() output
###################################################

# Fit the model with the dummy predictors (it does not matter which group you use as the reference group)
lm.bc.int = lm(prestige ~ L2income + education + wc + prof + 
  education:wc + education:prof, data = Prestige)
summary(lm.bc.int)

mean(Prestige$L2income)






###################################################
### Is there still an interaction effect between income and 
### occupation type on occupational prestige after controlling for education level?
###################################################

# Fit main-effects model
lm.bc.me = lm(prestige ~ L2income + education + type, data = Prestige)

# Fit interaction model
lm.bc.income.int = lm(prestige ~ L2income + education + type + 
  L2income:type, data = Prestige)

# Delta F-test
anova(lm.bc.me, lm.bc.income.int)

# Set up new data to plot
myData = expand.grid(
  L2income = seq(from = 10.6, to = 14.7, by = 0.1),
  education = mean(Prestige$education),
  type = c(levels(Prestige$type))
  )

# Obtain fitted values 
preds = predict(lm.bc.income.int, newdata = myData)

# Bind the data and predicted values
myData = cbind(myData, preds)

# Plot
ggplot(data = myData, aes(x = L2income, y = preds, color = type)) +
    geom_line() +
    xlab("Income (Log-2)") +
    ylab("Predicted Occupational Prestige") +
    scale_color_brewer(
      name = "Occupation Type", 
      labels = c("Blue-Collar", "White-Collar", "Professional"), 
      palette = "Set1") +
    theme_bw()



###################################################
### Include both interactions
###################################################

# Fit main-effects model
lm.bc.me = lm(prestige ~ L2income + education + type, data = Prestige)

# Fit interaction model
lm.bc.both.int = lm(prestige ~ L2income + education + type + 
  education:type + L2income:type, data = Prestige)

# Delta F-test
anova(lm.bc.me, lm.bc.both.int)



###################################################
### Which interaction is important?
###################################################

# Fit interaction model (only education:type)
lm.bc.educ.int = lm(prestige ~ L2income + education + type + 
  education:type, data = Prestige)

# Fit interaction model (only L2income:type)
lm.bc.Income.int = lm(prestige ~ L2income + education + type + 
  L2income:type, data = Prestige)

# Fit interaction model (both)
lm.bc.both.int = lm(prestige ~ L2income + education + type + 
  education:type + L2income:type, data = Prestige)

# Delta F-test (to test L2income)
anova(lm.bc.educ.int , lm.bc.both.int)

# Delta F-test (to test education)
anova(lm.bc.income.int , lm.bc.both.int)



###################################################
### Examine output
###################################################

summary(lm.bc.both.int)


###################################################
### Plot showing both interactions
###################################################


# Create data frame with values of main effects
myData = expand.grid(
  education = seq(from = 0, to = 16, by = 0.1),
  type = c(levels(Prestige$type)),
  L2income = c(9, 15)
  )

# Obtain fitted values (must use the : notation to fit model)
preds = predict(lm.bc.both.int, newdata = myData)

# Bind the data and predicted values
myData = cbind(myData, preds)

# Change L2income to a factor for better plotting
myData$L2income = factor(myData$L2income,
  levels = c(9, 15),
  labels = c("Low Income", "High Income")
  )


# Plot
ggplot(data = myData, aes(x = education, y = preds, color = type)) +
    geom_line() +
    xlab("Education") +
    ylab("Predicted Occupational Prestige") +
    scale_color_brewer(
      name = "Occupation Type", 
      labels = c("Blue-Collar", "White-Collar", "Professional"), 
      palette = "Set1") +
    theme_bw() +
    facet_wrap(~L2income)






