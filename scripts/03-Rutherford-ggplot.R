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
library(reshape2)



##################################################
### Prep data
##################################################

wr = melt(wr, variable.name = "condition", value.name = "words")

levels(wr$condition) = c("30's", "60's", "180's")



##################################################
### Figure 2.2 (p.25)
##################################################

ggplot(data = wordRecall, aes(x = condition, y = words)) +
  geom_point(pch = 1) +
  theme_bw() +
  scale_x_discrete(
  	name = "Experimental conditions",
  	labels = c("1-30 s", "2-60 s", "3-180 s")
  	) +
  scale_y_continuous(
  	name = "Number of words recalled", 
  	lim = c(0, 20)
  	)



##################################################
### Figure 2.3 (p.32)
##################################################

ggplot(data = wordRecall, aes(x = condition, y = words)) +
  geom_hline(yintercept = 9, lty = "dotted") +
  stat_summary(
  	fun.y = "mean", 
  	geom = "point", 
  	pch = 1, 
  	size = 4
  	) +
  stat_summary(aes(group = 1), fun.y = "mean", geom = "line") +
  theme_bw() +
  scale_x_discrete(
  	name = "Experimental conditions",
  	labels = c("30 s", "60 s", "90 s")
  	) +
  scale_y_continuous(
  	name = "Number of words recalled", 
  	lim = c(6, 15)
  	) + 
  annotate(
  	"text", 
  	x = 3.5, 
  	y = 9, 
  	label = "bar(Y)[G]", # ?plotmath
  	parse = TRUE         # without parse=TRUE the label would be printed as text
  	)




