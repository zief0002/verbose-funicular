###################################################
### Read in the data
###################################################

border = read.csv(file = "/Users/andrewz/Documents/GitHub/EPsy-8251/data/college-bordering-mn.csv")
head(border)




###################################################
### Examine distributions
###################################################

library(sm)
# outcome
sm.density(border$tuition_in_state) 

# potential predictors
sm.density(border$act75)
sm.density(border$admission)
sm.density(border$avg_fac_salary)
sm.density(border$completion)
sm.density(border$pct_pell)



###################################################
### Examine bivariate correlations
###################################################

cor(border[c("tuition_in_state", "public", "act75", "admission", 
	         "avg_fac_salary", "completion", "pct_pell")])



###################################################
### Examine functional form of outcome
###################################################

library(ggplot2)

# Examine public
ggplot(data = border, aes(x = public, y = tuition_in_state)) +
	geom_point(size = 4) +
	theme_bw() 

# Log-transform tuition
ggplot(data = border, aes(x = public, y = log(tuition_in_state))) +
	geom_point(size = 4) +
	theme_bw() 	

# Residual plot
plot(lm(log(tuition_in_state) ~ public, data = border), 1)



###################################################
### Examine functional form of predictor: act75
###################################################

ggplot(data = border, aes(x = act75, y = log(tuition_in_state), color = factor(public))) +
	geom_point(size = 4) +
	geom_smooth(se = FALSE, linetype = "dashed") +
	#geom_smooth(method = "lm", se = FALSE) +
	theme_bw() 

# Residual plot
plot(lm(log(tuition_in_state) ~ public + act75, data = border), 1)

###################################################
### Examine functional form of predictor: admission
###################################################

ggplot(data = border, aes(x = admission, y = log(tuition_in_state), color = factor(public))) +
	geom_point(size = 4) +
	geom_smooth(se = FALSE, linetype = "dashed") +
	geom_smooth(method = "lm", se = FALSE) +
	theme_bw() 

# Log-transform admission?
ggplot(data = border, aes(x = log(admission), y = log(tuition_in_state), color = factor(public))) +
	geom_point(size = 4) +
	geom_smooth(se = FALSE, linetype = "dashed") +
	geom_smooth(method = "lm", se = FALSE) +
	theme_bw() 

		
# Residual plots
plot(lm(log(tuition_in_state) ~ public + admission, data = border), 1)
plot(lm(log(tuition_in_state) ~ public + log(admission), data = border), 1)



###################################################
### Examine functional form of predictor: avg_faculty_salary
###################################################

ggplot(data = border, aes(x = avg_fac_salary, y = log(tuition_in_state), color = factor(public))) +
	geom_point(size = 4) +
	geom_smooth(se = FALSE, linetype = "dashed") +
	geom_smooth(method = "lm", se = FALSE) +
	theme_bw() 

#Residual plot
plot(lm(log(tuition_in_state) ~ public + avg_fac_salary, data = border), 1)



###################################################
### Examine functional form of predictor: completion
###################################################

ggplot(data = border, aes(x = completion, y = log(tuition_in_state), color = factor(public))) +
	geom_point(size = 4) +
	geom_smooth(se = FALSE, linetype = "dashed") +
	geom_smooth(method = "lm", se = FALSE) +
	theme_bw() 

#Residual plot
plot(lm(log(tuition_in_state) ~ public + completion, data = border), 1)



###################################################
### Examine functional form of predictor: pct_pell
###################################################

ggplot(data = border, aes(x = pct_pell, y = log(tuition_in_state), color = factor(public))) +
	geom_point(size = 4) +
	geom_smooth(se = FALSE, linetype = "dashed") +
	geom_smooth(method = "lm", se = FALSE) +
	theme_bw() 

#Residual plot
plot(lm(log(tuition_in_state) ~ public + pct_pell, data = border), 1)


###################################################
### Examine functional form of predictor: states
###################################################

ggplot(data = border, aes(x = state, y = log(tuition_in_state), color = factor(public))) +
	geom_point(size = 3, alpha = 0.4) +
	stat_summary(fun.y = mean, geom = "point", size = 4) +
	stat_summary(fun.y = mean, geom = "line", linetype = "dashed", aes(group = factor(public))) +
	theme_bw() 

#Residual plot
plot(lm(log(tuition_in_state) ~ public + state, data = border), 1)



###################################################
### Build main-effects model
###################################################

# Public, admission, and act75
lm.1 = lm(log(tuition_in_state) ~ public + admission + act75, data = border)

# state?
lm.2 = lm(log(tuition_in_state) ~ public + admission + act75 + state, data = border)
anova(lm.1, lm.2)

# avg_fac_salary?
summary(lm(log(tuition_in_state) ~ public + admission + act75 + state + avg_fac_salary, data = border))

# completion?
summary(lm(log(tuition_in_state) ~ public + admission + act75 + state + avg_fac_salary +
								   completion, data = border))

# pct_pell?
summary(lm(log(tuition_in_state) ~ public + admission + act75 + state + avg_fac_salary +
								   pct_pell, data = border))


final.me = lm(log(tuition_in_state) ~ public + admission + act75 + state + avg_fac_salary, data = border)
plot(final.me, 1)



###################################################
### Check polynomial terms for continuous predictors (quadratics)
###################################################

summary(lm(log(tuition_in_state) ~ public + admission + act75 + state + 
	                               avg_fac_salary + I(admission^2), data = border))

summary(lm(log(tuition_in_state) ~ public + admission + act75 + state + 
	                               avg_fac_salary + I(act75^2), data = border))

summary(lm(log(tuition_in_state) ~ public + admission + act75 + state + 
	                               avg_fac_salary + I(avg_fac_salary^2), data = border))

summary(lm(log(tuition_in_state) ~ public + admission + act75 + state + 
	                               avg_fac_salary + I(avg_fac_salary^2) + I(admission^2), 
	                               data = border))

final.quad = lm(log(tuition_in_state) ~ public + admission + act75 + state + 
	                               avg_fac_salary + I(avg_fac_salary^2), data = border)

plot(final.quad, 1)



###################################################
### Check for interaction-effects with the focal predictor
###################################################

# with states?

lm.int.states = lm(log(tuition_in_state) ~ public + admission + act75 + state + 
	avg_fac_salary + I(avg_fac_salary^2) + public:state, data = border)

anova(final.quad, lm.int.states)

# with admission?
summary(lm(log(tuition_in_state) ~ public + admission + act75 + state + 
	avg_fac_salary + I(avg_fac_salary^2) + public:admission, data = border))

# with act75?
summary(lm(log(tuition_in_state) ~ public + admission + act75 + state + 
	avg_fac_salary + I(avg_fac_salary^2) + public:act75, data = border))

# with avg_faculty_salary (linear)?
summary(lm(log(tuition_in_state) ~ public + admission + act75 + state + 
	avg_fac_salary + I(avg_fac_salary^2) + public:avg_fac_salary, data = border))

# with avg_faculty_salary (quadratic)?
summary(lm(log(tuition_in_state) ~ public + admission + act75 + state + 
	avg_fac_salary + I(avg_fac_salary^2) + 
	public:avg_fac_salary + public:I(avg_fac_salary^2), data = border))



###################################################
### Examine final model
###################################################

final = lm(log(tuition_in_state) ~ public + admission + act75 + state + 
	avg_fac_salary + I(avg_fac_salary^2) + 
	public:avg_fac_salary + public:I(avg_fac_salary^2), data = border)

out = fortify(final)

sm.density(out$.stdresid, model = "normal")

ggplot(data = out, aes(x = .fitted, y = .stdresid)) +
	geom_hline(yintercept = 0) +
	geom_point(size = 4) +
	geom_smooth(se = FALSE) +
	theme_bw()



###################################################
### Identify outlying colleges
###################################################

# Add name variable to fortified data
out$name = 	border$name
head(out)

# Get dataset of outliers
library(dplyr)
outliers = out %>% filter(abs(.stdresid) >= 2.5)
outliers

ggplot(data = out, aes(x = .fitted, y = .stdresid)) +
	geom_hline(yintercept = 0) +
	geom_point(size = 4) +
	geom_smooth(se = FALSE) +
	theme_bw() +
	geom_text(data = outliers, aes(label = name))



###################################################
### Remove Martin Luther College
###################################################

final2 = lm(log(tuition_in_state) ~ public + admission + act75 + state + 
	avg_fac_salary + I(avg_fac_salary^2) + 
	public:avg_fac_salary + public:I(avg_fac_salary^2), data = border,
	subset = -c(33))

out2 = fortify(final2)

sm.density(out2$.stdresid, model = "normal")

ggplot(data = out2, aes(x = .fitted, y = .stdresid)) +
	geom_hline(yintercept = 0) +
	geom_point(size = 4) +
	geom_smooth(se = FALSE) +
	theme_bw()

# Probably leave it in the data set



###################################################
### Scaling for no scientific notation
###################################################

border$adm = border$admission * 100
border$salary = border$avg_fac_salary / 1000

final3 = lm(log(tuition_in_state) ~ public + adm + act75 + state + 
	salary + I(salary^2) + 
	public:salary + public:I(salary^2), data = border)

summary(final3)



###################################################
### Plot
###################################################

# Set up plot data
plotData = expand.grid(
	salary = seq(from = 3.8, to = 10.9, by = 0.1),
	public = c(0, 1),
	adm = 72.32,
	act75 = 25.86,
	state = c("IA", "MN", "ND", "SD", "WI")
	)

# Predict from model
plotData$yhat = predict(final3, newdata = plotData)

# Back-transform tuition
plotData$tuition = exp(plotData$yhat)

# Re-scale salary
plotData$avg_fac_salary = plotData$salary * 1000

# Turn public into a factor
plotData$public = factor(plotData$public, levels = c(0, 1), labels = c("Private", "Public"))

# Examine data
head(plotData)

ggplot(data = plotData, aes(x = avg_fac_salary, y = tuition, color = public)) +
	geom_line() +
	facet_wrap(~state) +
	theme_bw() +
	scale_color_brewer(name = "Sector", palette = "Set1") +
	xlab("Average Monthly Faculty Salary") +
	ylab("Predicted In-State Tuition")
