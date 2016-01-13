##################################################
### Read in data
##################################################

vlss = read.csv("~/Documents/EPSY-8261/data/vlss.csv")

head(vlss)
tail(vlss)
summary(vlss)



##################################################
### Load ggplot2 library
##################################################

library(ggplot2)



##################################################
### Boxplot of expend versus region
##################################################

ggplot(data = vlss, aes(x = region, y = expend)) +
	geom_boxplot()



##################################################
### Add jittered points
##################################################

ggplot(data = vlss, aes(x = region, y = expend)) +
	geom_jitter() +
     geom_boxplot()
	



##################################################
### Colored boxplots
##################################################

ggplot(data = vlss, aes(x = region, y = expend, color = region)) +
     geom_boxplot(color = "black", fill = "steelblue") +
     geom_jitter()



##################################################
### Add mean points
##################################################

ggplot(data = vlss, aes(x = region, y = expend, fill = region)) +
     geom_boxplot() +
     stat_summary(fun.y = mean, geom = "point")

# Customize the mean points
ggplot(data = vlss, aes(x = region, y = expend, fill = region)) +
     geom_boxplot() +
     stat_summary(
          fun.y = mean, 
          geom = "point", 
          color = "black", 
          pch = 19,
          size = 2
          )



##################################################
### Boxplots of expend versus urban
##################################################

ggplot(data = vlss, aes(x = urban, y = expend)) +
     geom_boxplot()

# Coerce urban into a factor
vlss$urban2 = factor(vlss$urban, 
    levels = c(0, 1), 
    labels = c("Rural", "Urban")
    )

# Plot that works correctly
ggplot(data = vlss, aes(x = urban2, y = expend)) +
     geom_boxplot()





##################################################
### Facet on region
##################################################

ggplot(data = vlss, aes(x = urban2, y = expend)) +
     geom_boxplot() +
     facet_wrap(~ region)


# Put facets in a single row
ggplot(data = vlss, aes(x = urban2, y = expend)) +
     geom_boxplot() +
     facet_wrap(~ region, nrow = 1)



##################################################
### Add axis label
##################################################

ggplot(data = vlss, aes(x = urban2, y = expend)) +
     geom_boxplot() +
     facet_wrap(~ region, nrow = 1) +
     xlab("Type of Household")



##################################################
### Change axis limits
##################################################

ggplot(data = vlss, aes(x = urban2, y = expend)) +
     geom_boxplot() +
     facet_wrap(~ region, nrow = 1) +
     xlab("Type of Household") +
     ylim(0, 2000)



##################################################
### Both labels and y-limit
##################################################

ggplot(data = vlss, aes(x = urban2, y = expend)) +
     geom_boxplot(aes(fill = urban2)) +
     facet_wrap(~ region, nrow = 1) +
     xlab("Type of Household") +
     ylab("Expenditures (in U.S. Dollars)") +
     ylim(0, 2000)



##################################################
### Customize fill color
##################################################

ggplot(data = vlss, aes(x = urban2, y = expend)) +
     geom_boxplot(aes( fill = urban2)) +
     facet_wrap(~ region, nrow = 1) +
     xlab("Type of Household") +
     ylab("Expenditures (in U.S. Dollars)") +
     scale_fill_manual(
          values = c("#E69F00", "#56B4E9")
          )



##################################################
### Change legend title
##################################################

ggplot(data = vlss, aes(x = urban2, y = expend)) +
     geom_boxplot(aes(fill = urban2)) +
     facet_wrap(~ region, nrow = 1) +
     xlab("Type of Household") +
     ylab("Expenditures (in U.S. Dollars)") +
     scale_fill_manual(
          values = c("#E69F00", "#56B4E9"),
          name = "Region in Vietnam"
          )


##################################################
### Default palette
##################################################

ggplot(data = vlss, aes(x = urban2, y = expend)) +
     geom_boxplot(aes(fill = urban2)) +
     facet_wrap(~ region, nrow = 1) +
     xlab("Type of Household") +
     ylab("Expenditures (in U.S. Dollars)") +
     scale_fill_hue(name = "Region in Vietnam")



##################################################
### Greyscale palette
##################################################

ggplot(data = vlss, aes(x = urban2, y = expend)) +
     geom_boxplot(aes(fill = urban2)) +
     facet_wrap(~ region, nrow = 1) +
     xlab("Type of Household") +
     ylab("Expenditures (in U.S. Dollars)") +
     scale_fill_grey(name = "Region in Vietnam")



##################################################
### Color brewer palette
##################################################

ggplot(data = vlss, aes(x = urban2, y = expend)) +
     geom_boxplot(aes(fill = urban2)) +
     facet_wrap(~ region, nrow = 1) +
     xlab("Type of Household") +
     ylab("Expenditures (in U.S. Dollars)") +
     scale_fill_brewer(
          name = "Region in Vietnam",
          palette = "Set2"
          )



##################################################
### Remove legend
##################################################

ggplot(data = vlss, aes(x = urban2, y = expend)) +
     geom_boxplot(aes(fill = urban2)) +
     facet_wrap(~ region, nrow = 1) +
     xlab("Type of Household") +
     ylab("Expenditures (in U.S. Dollars)") +
     ylim(0, 2000) +
     scale_fill_brewer(
          name = "Region in Vietnam",
          palette = "Set2"
          ) +
     theme(legend.position = "none")



##################################################
### Black and white background
##################################################

ggplot(data = vlss, aes(x = urban2, y = expend)) +
     geom_boxplot(aes(fill = urban2)) +
     facet_wrap(~ region, nrow = 1) +
     xlab("Type of Household") +
     ylab("Expenditures (in U.S. Dollars)") +
     ylim(0, 2000) +
     scale_fill_brewer(
          name = "Region in Vietnam",
          palette = "Set2"
          ) +
     theme_bw()



##################################################
### Customized themes
##################################################

# Install ggthemes package and then load it
library(ggthemes)

# Wall Street Journal theme
ggplot(data = vlss, aes(x = urban2, y = expend)) +
     geom_boxplot(aes(fill = urban2)) +
     facet_wrap(~ region, nrow = 1) +
     xlab("Type of Household") +
     ylab("Expenditures (in U.S. Dollars)") +
     theme_wsj() +
     scale_fill_wsj(name = "Region in Vietnam", palette = "rgby")



##################################################
### Final plot
##################################################

ggplot(data = vlss, aes(x = urban2, y = expend)) +
     geom_boxplot(aes(fill = urban2)) +
     facet_wrap(~ region, nrow = 1) +
     xlab("Type of Household") +
     ylab("Expenditures (in U.S. Dollars)") +
     scale_fill_brewer(
          name = "Region in Vietnam",
          palette = "Set2"
          ) +
     theme_bw() +
     theme(legend.position = "none")




