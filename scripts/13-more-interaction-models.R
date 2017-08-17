##################################################
### Load libraries
##################################################

library(dplyr)
library(ggplot2)
library(readr)
library(sm)



###################################################
### Read in the data
###################################################

beauty = read_csv(file = "~/Dropbox/epsy-8251/data/beauty.csv")
head(beauty)



##################################################
### Examine data
##################################################

summary(beauty)


# Discretize the age variable into 3 categories
beauty = beauty %>% mutate( age_discrete = cut(beauty$age, breaks = 3) )
summary(beauty)




##################################################
### Examine effect of age
##################################################

ggplot(data = beauty, aes(x = btystdave, y = avgeval, color = age_discrete)) + 
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  xlab("Beauty rating") +
  ylab("Average course evaluation score") +
  scale_color_brewer(palette = "Set2") + 
  facet_wrap(~age_discrete)



##################################################
### Fit interaction model
##################################################

lm.1 = lm(avgeval ~ 1 + btystdave + age + btystdave:age, data = beauty) 
summary(lm.1)



##################################################
### Delta F-test for nested models
##################################################

# Fit main-effects model
lm.me = lm(avgeval ~ 1 + btystdave + age, data = beauty)

# Fit interaction model
lm.int = lm(avgeval ~ 1 + btystdave + age + btystdave:age, data = beauty)

# Test effects
anova(lm.me, lm.int)



##################################################
### Plot interaction model results
##################################################

# Create new data set with main effects
myData = expand.grid(
  btystdave = seq(from = -1.6, to = 1.9, by = 0.1), 
  age = c(40, 50, 60)
)


# Use fitted model to compute fitted values for the data
myData = myData %>% mutate( yhat = predict(lm.1, newdata = myData) ) 
head(myData)


# Plot the fitted model
ggplot(data = myData, aes(x = btystdave, y = yhat, color = factor(age))) + 
  geom_line() +
  theme_bw() +
  xlab("Beauty score") +
  ylab("Predicted average course evaluation score") +
  scale_color_brewer(name = "Age", palette = "Set1", labels = c("40 years old", "50 years old", "60 years old")) +
  ylim(0, 5)
  
  

##################################################
### Interpret interaction model effects
##################################################

summary(lm.1)



##################################################
### Include covariates in the model
##################################################

lm.2 = lm(avgeval ~ 1 + btystdave + age + female + btystdave:age, data = beauty) 
summary(lm.2)



##################################################
### Plot the model results
##################################################

# Create new data set with main effects
myData = expand.grid(
  btystdave = seq(from = -1.6, to = 1.9, by = 0.1), age = c(40, 60),
  female = c(0, 1)
)


# Use fitted model to compute fitted values for the data
myData = myData %>% mutate( yhat = predict(lm.2, newdata = myData) )


# Convert female and age into factors for better plotting
myData = myData %>% 
  mutate(Sex = factor(female, 
                      levels = c(0, 1), 
                      labels = c("Males", "Females")
                      )
  )

myData = myData %>% 
  mutate(Age = factor(age, 
                      levels = c(40, 60), 
                      labels = c("40 year olds", "60 year olds")
                      )
  )

head(myData)

              
# Plot the fitted model
ggplot(data = myData, aes(x = btystdave, y = yhat, color = Age, linetype = Sex)) + 
  geom_line() +
  theme_bw() +
  xlab("Beauty score") +
  ylab("Predicted average course evaluation score") + 
  scale_color_brewer(name = "Age", palette = "Set1")                                                                                                                                      



##################################################
### Higher order interactions
##################################################

lm.3 = lm(avgeval ~ 1 + btystdave + age + female + 
            btystdave:age + btystdave:female + female:age + 
            btystdave:age:female, data = beauty)
summary(lm.3)
            
            

##################################################
### Plot the model results
##################################################

# Create new data set with main effects
myData = expand.grid(
  btystdave = seq(from = -1.6, to = 1.9, by = 0.1),
  age = c(40, 60),
  female = c(0, 1)
)

# Use fitted model to compute fitted values for the data
myData = myData %>% mutate( yhat = predict(lm.3, newdata = myData) )

# Convert female and age into factors for better plotting
myData = myData %>% 
  mutate(Sex = factor(myData$female, 
                      levels = c(0, 1), 
                      labels = c("Males", "Females")
                      )
  )

myData = myData %>% 
  mutate(age = factor(myData$age, 
                      levels = c(40, 60), 
                      labels = c("40 year olds", "60 year olds")
                      )
  )

head(myData)

# Plot the fitted model
ggplot(data = myData, aes(x = btystdave, y = yhat, color = age)) +
  geom_line() +
  theme_bw() +
  xlab("Beauty score") +
  ylab("Predicted average course evaluation score") +
  scale_color_brewer(name = "Age", palette = "Set1") +
  facet_wrap(~Sex)

