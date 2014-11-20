###################################################
### Read in Data (Prestige2.csv)
###################################################

Prestige = read.csv(file =  "/Users/andrewz/Documents/EPSY-8251/data/Prestige2.csv")
head(Prestige)



###################################################
### Libraries
###################################################

library(ggplot2)
library(sm)
library(psych)


###################################################
### RQ1: Is there an interaction effect between education 
### and income on occupational prestige?
###################################################

# Fit interaction model 
lm.1 = lm(prestige ~ income + education + education:income, data = Prestige)
summary(lm.1)



###################################################
### Plot results from Model 1
###################################################

summary(Prestige[c("education", "income")])

# Create data frame with sequence of values for education, 
# and pick discrete values for income
myData = expand.grid(
  education = seq(from = 6.3, to = 16, by = 0.1),
  income = c(4250, 6036, 8226)
  )


# Obtain fitted values 
preds = predict(lm.1, newdata = myData)

# Bind the data and predicted values
myData = cbind(myData, preds)
head(myData)

# Coerce income into a factor
myData$income = factor(myData$income,
  levels = c(4250, 6036, 8226),
  labels = c("Low", "Moderate", "High")
  )


# Plot
ggplot(data = myData, aes(x = education, y = preds, color = income)) +
    geom_line() +
    xlab("Education") +
    ylab("Predicted Occupational Prestige") +
    scale_color_brewer(
      name = "Income Level", 
      palette = "Set1") +
    theme_bw()


###################################################
### Plot Model 1 (Option 2)
###################################################

summary(Prestige[c("education", "income")])

# Create data frame with sequence of values for income, 
# and pick discrete values for education
myData = expand.grid(
  income = 1656:25879,
  education = c(8, 10, 13)
  )

# Obtain fitted values 
preds = predict(lm.1, newdata = myData)

# Bind the data and predicted values
myData = cbind(myData, preds)
head(myData)

# Coerce education into a factor
myData$education = factor(myData$education,
  levels = c(8, 10, 13),
  labels = c("Junior High", "Some High School", "Some Post-Secondary")
  )


# Plot
ggplot(data = myData, aes(x = income, y = preds, color = education)) +
    geom_line() +
    xlab("Income") +
    ylab("Predicted Occupational Prestige") +
    scale_color_brewer(
      name = "Education Level", 
      palette = "Set1") +
    theme_bw()




###################################################
### Full Interaction Model
###################################################

# Fit the model with the dummy predictors (it does not matter which group you use as the reference group)
lm.2 = lm(prestige ~ income + education + type + income:education + income:type +
  education:type + education:income:type, data = Prestige)
summary(lm.2)


# Alternate notation
lm.2 = lm(prestige ~ income*education*type, data = Prestige)
summary(lm.2)




###################################################
### Drop highest order interaction...
###################################################

lm.3 = lm(prestige ~ income + education + type + income:education + income:type +
  education:type, data = Prestige)
summary(lm.3)

# Alternate notation
lm.3 = lm(prestige ~ (income + education + type)^2, data = Prestige)
summary(lm.3)



###################################################
### Drop non-significant income:education interaction...
###################################################

lm.4 = lm(prestige ~ income + education + type + income:type + education:type, data = Prestige)
summary(lm.4)

# Alternate notation
lm.4 = lm(prestige ~ (income + education + type)^2 - income:education, data = Prestige)
summary(lm.4)



###################################################
### Plot
###################################################

# Create data frame with sequence of values for income, 
# and pick discrete values for education and type
myData = expand.grid(
  income = 1656:25879,
  education = c(8, 10, 13),
  type = levels(Prestige$type)
  )

# Obtain fitted values 
preds = predict(lm.4, newdata = myData)

# Bind the data and predicted values
myData = cbind(myData, preds)
head(myData)

# Coerce education into a factor
myData$education = factor(myData$education,
  levels = c(8, 10, 13),
  labels = c("Junior High", "Some High School", "Some Post-Secondary")
  )

# Plot (to show interaction b/w income & type)
ggplot(data = myData, aes(x = income, y = preds, color = type)) +
    geom_line() +
    xlab("Income") +
    ylab("Predicted Occupational Prestige") +
    scale_color_brewer(
      name = "Education Level", 
      palette = "Set1") +
    theme_bw() +
    facet_wrap(~education)

# Plot (to show no interaction between income and education)
ggplot(data = myData, aes(x = income, y = preds, color = education)) +
    geom_line() +
    xlab("Income") +
    ylab("Predicted Occupational Prestige") +
    scale_color_brewer(
      name = "Education Level", 
      palette = "Set1") +
    theme_bw() +
    facet_wrap(~type)


# Show all on the same panel
ggplot(data = myData, aes(x = income, y = preds, group = type:education)) +
    geom_line(aes(color = type, size = education)) +
    xlab("Income") +
    ylab("Predicted Occupational Prestige") +
    theme_bw()


