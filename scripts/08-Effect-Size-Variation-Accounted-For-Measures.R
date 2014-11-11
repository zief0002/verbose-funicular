##################################################
### Read in data
##################################################

wr = data.frame(
  words.30  = c(7, 3, 6, 6, 5, 8, 6, 7),
  words.60  = c(7, 11, 9, 11, 10, 10, 11, 11),
  words.180 = c(8, 14, 10, 11, 12, 10, 11, 12)
  )

wr = melt(wr, variable.name = "condition", value.name = "words")
levels(wr$condition) = c("30s", "60s", "180s")

wr$con30 = ifelse(wr$condition == "30s", 1, 0)
wr$con60 = ifelse(wr$condition == "60s", 1, 0)
wr$con180 = ifelse(wr$condition == "180s", 1, 0)

library(psych)
describeBy(wr$words, wr$condition)



##################################################
### Compute Cohen's f
##################################################

num = (6 - 9)^2 + (10 - 9)^2 + (11 - 9)^2
den = 3 * 1.574^2

sqrt(num/den)



##################################################
### Fit regression models to get p-values
##################################################

lm.30 = lm(words ~ 1 + con60 + con180, data = wr)
summary(lm.30)



##################################################
### Compute Cohen's f from eta-squared
##################################################

sqrt(0.116/(1- 0.116))



##################################################
### Compute omega-squared
##################################################

anova(lm.30)


(112 - 2 * 2.476) / (164 + 2.476)

#(112 - 2 * 1.574^2) / (164 + 1.574^2)




##################################################
### Compute confidence interval for eta-squared
##################################################

# Note: The gsl library may need to be installed manually for this function to work
library(MBESS)
ci.R2(R2 = 0.6829, df.1 = 2, df.2 = 21, Random.Predictors = FALSE)



##################################################
### Raw CIs for contrasts
##################################################

# Set up the contrasts to test
contr = c("`30s` - `60s` = 0",
          "`30s` - `180s` = 0",
          "`60s` - `180s` = 0")

# Fit a model using the grouping variable
lm.fac = lm(words ~ condition, data = wr)

# Load multcomp library (you may need to install it first)
library(multcomp)

# Use the ANOVA model to test pairwise contrasts
glht.1 = glht(lm.fac, linfct = mcp(condition = contr))

# Assign the summary output to an object
benHoch = summary(glht.1, test = adjusted("BH"))

# Get the Benjamani-Hochberg adjusted confidence intervals
confint(benHoch)



##################################################
### Standardized effect and CI for contrasts
##################################################

smd(
    Mean.1 = 6, 
    s.1 = 1.51,
    n.1 = 8,
    Mean.2 = 10,
    s.2 = 1.41,
    n.2 = 8
    )

ci.smd(smd = -2.738121, n.1 = 8, n.2 = 8)



##################################################
### Contrast testing
##################################################

# Check order of levels
levels(wr$condition)

# Create vector of contrast weights
w = c(1, -1, 0)
w

# Create vector of means
m = tapply(wr$words, wr$condition, mean)
m

# Create vector of lengths
l = tapply(wr$words, wr$condition, length)
l

# Compute estimate for the contrast
sum(w * m)

# Compute denominator
sum(w^2 / l)

# Compute SS
sum(w * m) ^ 2 / sum(w^2 / l)



w = matrix(w, nrow = 1)

# Turn w into a row matrix
w = rbind("`30s` - `60s`" = w)

glht.1 = glht(lm.30, linfct = w)




w1 = c(1, -1, 0)
w2 = c(1, 0, -1)
w3 = c(0, 1, -1)

w = matrix(c(w1, w2, w3), nrow = 3)



anova(lm(words ~ 1 + con60, data = wr, subset = condition != "180s"))
summary(lm.30)

pdf(file = "~/Desktop/plot.pdf")
dev.off()



