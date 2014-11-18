
cost = read.csv(file.choose())

meta = read.csv(file.choose())

cost_of_living = read.csv(file.choose())


library(plyr)
cost2 = join(x = cost, y = meta, by = "team" , type = "left")
cost3 = join(x = cost2, y = cost_of_living, by = "location" , type = "left")

cost3$ageStadium = 2014 - cost2$yearOpened

library(sm)
sm.density(cost3$fci)

cost2$Lfci = log(cost2$fci)
sm.density(cost3$Lfci)

lm.1 = lm(Lfci ~ league, data = cost3)
summary(lm.1)

sm.density(cost3$ageStadium)

cost3$Lage = log(cost3$ageStadium + 1)

lm.2 = lm(Lfci ~ Lage, data = cost3)
summary(lm.2)

plot(lm.2)

lm.3 = lm(Lfci ~ Lage + I(Lage ^ 2), data = cost3)
summary(lm.3)

lm.4 = lm(Lfci ~ Lage + I(Lage ^ 2) + league, data = cost3)


myData = expand.grid(
	Lage = seq(from = 0, to = 4.7, by = 0.1),
	league = levels(cost3$league)
	)

preds = predict(lm.4, newdata = myData)

myData = cbind(myData, preds)
head(myData)

library(ggplot2)
ggplot(data = myData, aes(x = Lage, y = preds, color = league)) +
	geom_line()



myData$FCI = exp(myData$preds)
head(myData)

ggplot(data = myData, aes(x = Lage, y = FCI, color = league)) +
	geom_line()

myData$ageStadium = exp(myData$Lage) - 1
head(myData)

ggplot(data = myData, aes(x = ageStadium, y = FCI, color = league)) +
	geom_line() +
	theme_bw() +
	scale_color_grey()






