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
describe(mn$gradRate)
describeBy(mn$gradRate, mn$public)





##################################################
### Fit model
##################################################

lm.public = lm(gradRate ~ public, data = mn)
summary(lm.public)





##################################################
### Residual plot
##################################################

mn$private = ifelse(mn$public == 0, 1, 0)
head(mn)


lm.private = lm(gradRate ~ private, data = mn)
summary(lm.private)





##################################################
### Examine assumptions
##################################################

library(ggplot2)
out = fortify(lm.public)

# Normality?
library(sm)
sm.density(out$.stdresid, model = "normal")

# Residual plot
ggplot(data = out, aes(x = .fitted, y = .stdresid)) +
    geom_point(size = 4) +
    theme_bw() +
    geom_hline(yintercept = 0)


##################################################
### Fit ANCOVA model
##################################################

lm.2 = lm(gradRate ~ public + sat, data = mn)
summary(lm.2)



##################################################
### Fit ANCOVA model 2
##################################################

lm.3 = lm(gradRate ~ public + sat + tuition, data = mn)
summary(lm.3)




