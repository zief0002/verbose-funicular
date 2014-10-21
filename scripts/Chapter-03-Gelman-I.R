nels = read.csv(file = "/Users/andrewz/Documents/GitHub/EPsy-8251/data/NELS.csv")
head(nels)

library(sm)
library(psych)

sm.density(nels$grades)
describe(nels$grades)

sm.density(nels$hwork)
describe(nels$hwork)


library(ggplot2)
ggplot(data = nels, aes(x = hwork, y = grades)) +
	geom_point() +
	theme_bw()


cor(nels[c("grades", "hwork")])

lm.1 = lm(grades ~ hwork, data = nels)
summary(lm.1)
anova(lm.1)

