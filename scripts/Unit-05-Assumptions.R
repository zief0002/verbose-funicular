##################################################
### Read in and prepare data
##################################################

wr = data.frame(
  words.30  = c(7, 3, 6, 6, 5, 8, 6, 7),
  words.60  = c(7, 11, 9, 11, 10, 10, 11, 11),
  words.180 = c(8, 14, 10, 11, 12, 10, 11, 12)
  )

library(reshape2)
wr = melt(wr, variable.name = "condition", value.name = "words")
levels(wr$condition) = c("30s", "60s", "180s")

wr$con30 = ifelse(wr$condition == "30s", 1, 0)
wr$con60 = ifelse(wr$condition == "60s", 1, 0)
wr$con180 = ifelse(wr$condition == "180s", 1, 0)

library(psych)
describeBy(wr$words, wr$condition)



##################################################
### Fit ANOVA model
##################################################

lm.a = lm(words ~ condition, data = wr)
anova(lm.a)



##################################################
### Fortify model
##################################################

library(ggplot2)
outa = fortify(lm.a)
head(outa)



##################################################
### Compute omega-squared
##################################################

describeBy(outa$.resid, outa$condition)



##################################################
### Compute error variances
##################################################

# Compute the variance for the 30s group
1.51 ^ 2

# Compute the variance for the 60s group
1.41 ^ 2

# Compute the variance for the 180s group
1.77 ^ 2



##################################################
### Levene's Test.....Bad, Bad, Bad
##################################################

library(car)
leveneTest(lm.a)




