##################################################
### Read in data
##################################################

math = read.csv(file = "~/Documents/EPsy-8262/data/homework-achievement.csv")

head(math)
tail(math)




##################################################
### Correlation
##################################################

cor(math[ , c("homework", "achievement")])



##################################################
### Regress achievement on homework
##################################################

lm.a = lm(achievement ~ homework, data = math)
summary(lm.a)



##################################################
### Measuring the homework variable in minutes
##################################################

math$homework_minutes = math$homework * 60
head(math)

lm.b = lm(achievement ~ homework_minutes, data = math)
summary(lm.b)



##################################################
### Example of Centering and Scaling
##################################################

X = c(1, 2, 2, 3, 3, 4, 6)

library(plotrix)
dotplot.mtb(X)

mean(X)
sd(X)


# Centering
X_centered = X - mean(X)

dotplot.mtb(X_centered)
mean(X_centered)
sd(X_centered)


# Scaling
X_scaled = X_centered / sd(X_centered)

dotplot.mtb(X_scaled)
mean(X_scaled)
sd(X_scaled)



##################################################
### Center and Scale the outcome and predictor
##################################################

math$c_homework = math$homework - mean(math$homework)
head(math)

# Distribution of centered predictor
sm.density(math$c_homework)

# Scatterplot of Cx and y
ggplot(data = math, aes(x = c_homework, y = achievement)) +
	geom_point(size = 5) +
	xlab("Time Spent on Homework (centered)") +
	ylab("Mathematics Achievement") +
	theme_bw()

# Correlation
cor(math[ , c("c_homework", "achievement")])

# Regression of Y on Cx
lm.b = lm(achievement ~ c_homework, data = math)
summary(lm.b)





##################################################
### Center predictor and outcome
##################################################

math$c_achievement = math$achievement - mean(math$achievement)
head(math)

# Distribution of centered outcome
sm.density(math$c_achievement)


# Scatterplot of Cx and y
ggplot(data = math, aes(x = c_homework, y = achievement)) +
	geom_point(size = 5) +
	xlab("Time Spent on Homework (centered)") +
	ylab("Mathematics Achievement") +
	theme_bw()

# Correlation
cor(math[ , c("c_homework", "achievement")])

# Regression of Cy on Cx
lm.c = lm(c_achievement ~ c_homework, data = math)
summary(lm.c)




##################################################
### Center and Scale the outcome and predictor
##################################################

# Create centered and scaled outcome
math$z_achievement = (math$achievement - mean(math$achievement)) / sd(math$achievement)

# Create centered and scaled predictor
math$z_homework = (math$homework - mean(math$homework)) / sd(math$homework)

head(math)

# Examine distributions of the transformed distributions
sm.density(math$z_achievement)
sm.density(math$z_homework)

# Scatterplot of Zx and Zy
ggplot(data = math, aes(x = z_homework, y = z_achievement)) +
	geom_point(size = 5) +
	xlab("Time Spent on Homework (z-Score)") +
	ylab("Mathematics Achievement (z-Score)") +
	theme_bw()

# Correlation
cor(math[ , c("z_homework", "z_achievement")])

# Regression of Zy on Zx
lm.b = lm(z_achievement ~ z_homework, data = math)
summary(lm.b)

# Plot of the fitted regression
ggplot(data = math, aes(x = z_homework, y = z_achievement)) +
	geom_smooth(method = "lm", se = FALSE, lwd = 2) +
	xlab("Time Spent on Homework (z-Score)") +
	ylab("Mathematics Achievement (z-Score)") +
	theme_bw()


