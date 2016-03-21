###################################################
### Read in the data
###################################################

cehd = read.csv(file = "~/Desktop/cehd_pay.csv")
head(cehd)





##################################################
### Load libraries
##################################################

library(dplyr)
library(ggplot2)



##################################################
### Examine sample mean differences
##################################################

cehd %>% group_by(dept) %>% summarize(M = mean(pay), SD = sd(pay))


ggplot(data = cehd, aes(x = dept, y = pay)) +
	geom_boxplot() +
	theme_bw()



##################################################
### Examine levels of factor and create dummy variables
##################################################

levels(cehd$dept)

cehd$curr = ifelse(cehd$dept == "CI",   1, 0)
cehd$epsy = ifelse(cehd$dept == "EPSY", 1, 0)
cehd$icd  = ifelse(cehd$dept == "ICD",  1, 0)
cehd$kin  = ifelse(cehd$dept == "KIN",  1, 0)



##################################################
### Fit regression models (ANOVA)
##################################################

lm.1.curr = lm(pay ~ epsy + icd + kin, data = cehd)
summary(lm.1.curr)

lm.1.epsy = lm(pay ~ curr + icd + kin, data = cehd)
summary(lm.1.epsy)

lm.1.icd = lm(pay ~ curr + epsy + kin, data = cehd)
summary(lm.1.icd)

lm.1.kin = lm(pay ~ curr + epsy + icd, data = cehd)
summary(lm.1.kin)



##################################################
### Adjust p-values for multiple comparisons
##################################################

p.values = c(
	0.74,      # C&I vs. EPSY
	0.0000013, # C&I vs. ICD
	0.43,      # C&I vs. KIN
	0.0000018, # EPSY vs. ICD
	0.59,      # EPSY vs. KIN
	0.000095   # ICD vs. KIN
)

p.adjust(p.values, method = "BH")
#p.adjust(p.values, method = "bonferroni")




##################################################
### Fit regression models (ANCOVA)
##################################################

lm.2.curr = lm(pay ~ epsy + icd + kin + faculty, data = cehd)
summary(lm.2.curr)

lm.2.epsy = lm(pay ~ curr + icd + kin + faculty, data = cehd)
summary(lm.2.epsy)

lm.2.icd = lm(pay ~ curr + epsy + kin + faculty, data = cehd)
summary(lm.2.icd)

lm.2.kin = lm(pay ~ curr + epsy + icd + faculty, data = cehd)
summary(lm.2.kin)



##################################################
### Adjust p-values for multiple comparisons
##################################################

p.values.2 = c(
  0.432,      # C&I vs. EPSY
  0.00000069, # C&I vs. ICD
  0.072,      # C&I vs. KIN
  0.0000047, # EPSY vs. ICD
  0.23,      # EPSY vs. KIN
  0.0012     # ICD vs. KIN
)

p.adjust(p.values.2, method = "BH")


library(paircompviz)
paircomp(obj=ecls$reading, grouping=ecls$ethnic, test="t", p.adjust = "BH", result = TRUE)


