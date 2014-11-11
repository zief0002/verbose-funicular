##################################################
### Create data
##################################################

wr = data.frame(
  words.30  = c(7, 3, 6, 6, 5, 8, 6, 7),
  words.60  = c(7, 11, 9, 11, 10, 10, 11, 11),
  words.180 = c(8, 14, 10, 11, 12, 10, 11, 12)
  )
 


##################################################
### Load libraries
##################################################

library(ggplot2)
library(psych)
library(reshape2)
library(sm)



##################################################
### Prep data
##################################################

wr = melt(wr, variable.name = "condition", value.name = "words")
levels(wr$condition) = c("30s", "60s", "180s")

wr$con30 = ifelse(wr$condition == "30s", 1, 0)
wr$con60 = ifelse(wr$condition == "60s", 1, 0)
wr$con180 = ifelse(wr$condition == "180s", 1, 0)



##################################################
### Fit regression models to get p-values
##################################################

lm.30 = lm(words ~ 1 + con60 + con180, data = wr)
summary(lm.30)

lm.60 = lm(words ~ 1 + con30 + con180, data = wr)
summary(lm.60)

lm.180 = lm(words ~ 1 + con30 + con60, data = wr)
summary(lm.180)



##################################################
### p-values for pairwise contrasts
##################################################

0.0000492  #30s vs 60s
0.00000267 #30s vs 180s
0.218      #60s vs 180s

# Put p-values in a vector
p.values = c(0.0000492, 0.00000267, 0.218)



##################################################
### Fit regression model w/only 30s and 60s
##################################################

no180s = subset(wr, condition != "180s")
no180s

# Fit model using con30 dummy
lm.a = lm(words ~ 1 + con30, data = no180s)
summary(lm.a)

# Results from lm.a
# con30   -4.0000   0.7319   -5.465   .0000833

# Results from lm.60
# con30   -4.0000   0.7868   -5.084   .0000492

# Two-sample t-test
t.test(words ~ condition, data = no180s, var.equal = TRUE)



##################################################
### p-value adjustment
##################################################

# Set R options to depress scientific notation
options(scipen = 99)

# Bonferroni adjustment
p.values * 3

# Method 2 (better way to get Bonferroni adjusted p-values)
p.adjust(p.values, method = "bonferroni")


levels(wr$condition)


# Set up the contrasts to test
contr = c("`30s` - `60s` = 0",
          "`30s` - `180s` = 0",
          "`60s` - `180s` = 0")

# Fit the lm using the grouping variabel
lm.1 = lm(words ~ condition, data = wr)

# Fit the glht
library(multcomp)
glht.1 = glht(lm.1, linfct = mcp(condition = contr))

# Obtain the results
summary(glht.1, test = adjusted("none"))        #Unadjusted p-values
summary(glht.1, test = adjusted("bonferroni"))  #Bonferroni adjusted p-values
summary(glht.1, test = adjusted("BH"))          #Benjamani-Hochberg adjusted p-values
summary(glht.1, test = adjusted("Shaffer"))     #Shaffer adjusted p-values

