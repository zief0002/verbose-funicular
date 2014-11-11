###################################################
### Read in Data
###################################################

nels = read.csv(file = "/Users/andrewz/Documents/GitHub/EPsy-8251/data/NELS.csv")
head(nels)


###################################################
### Libraries
###################################################

#library(ggplot2)
#library(sm)
#library(psych)



###################################################
### Marginal distributions
###################################################

sm.density(nels$grades)
describe(nels$grades)

sm.density(nels$hwork)
describe(nels$hwork)



###################################################
### Conditional distribution of grades
###################################################

library(ggplot2)
ggplot(data = nels, aes(x = hwork, y = grades)) +
	geom_point() +
	theme_bw()



###################################################
### Correlation b/w grades and hwork
###################################################

cor(nels[c("grades", "hwork")])



###################################################
### Fit simple regression model (regress grades on hwork)
###################################################

lm.1 = lm(grades ~ hwork, data = nels)
summary(lm.1)
anova(lm.1)


ggplot(data = nels, aes(x = hwork, y = grades)) +
	geom_point() +
	#geom_smooth(se = F, method = "lm") +
	#geom_hline(yintercept = 80.47) +
	theme_bw()

lm.1 = lm(grades ~ 1, data = nels)
summary(lm.1)

out1 = fortify(lm.1)

ggplot(data = out1, aes(x = .fitted, y = .stdresid)) +
	geom_point() +
	geom_hline(yintercept = 0) +
	geom_hline(yintercept = c(-2, 2), lty = "dashed") +
	theme_bw()


sm.density(out1$.resid, model = "normal")
sm.density(out1$.stdresid, model = "normal")

lm.2 = lm(grades ~ 1 + hwork + pared, data = nels)
summary(lm.2)

out2 = fortify(lm.2)

ggplot(data = out2, aes(x = .fitted, y = .stdresid)) +
	geom_point() +
	geom_hline(yintercept = 0) +
	geom_hline(yintercept = c(-2, 2), lty = "dashed") +
	theme_bw()

ggplot(data = out2, aes(x = .fitted, y = .stdresid)) +
	geom_text(aes(label = id)) +
	geom_hline(yintercept = 0) +
	geom_hline(yintercept = c(-2, 2), lty = "dashed") +
	theme_bw()

lm.3 = lm(grades ~ 1 + hwork + pared, data = nels, subset = -c(34))
summary(lm.3)



