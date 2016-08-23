##################################################
### Read in data
##################################################

cehd = read.csv("~/Desktop/cehd.csv")

head(cehd)
tail(cehd)

summary(cehd)



##################################################
### Load ggplot2 library
##################################################

library(ggplot2)



##################################################
### Boxplot of title versus annual pay
##################################################

ggplot(data = cehd, aes(x = title, y = annual_pay)) +
	geom_boxplot()



##################################################
### Add jittered points
##################################################

ggplot(data = cehd, aes(x = title, y = annual_pay)) +
  geom_boxplot() +
	geom_jitter()
	



##################################################
### Colored boxplots
##################################################

# Global aesthetics
ggplot(data = cehd, aes(x = title, y = annual_pay, color = title)) +
  geom_boxplot() +
  geom_jitter() 


# local aesthetics
ggplot(data = cehd, aes(x = title, y = annual_pay)) +
  geom_boxplot(aes(color = title)) +
  geom_jitter()


# Fixed color
ggplot(data = cehd, aes(x = title, y = annual_pay, color = title)) +
  geom_boxplot(color = "black", fill = "steelblue") +
  geom_jitter()+ guides(color = FALSE)



##################################################
### Facet on department
##################################################

ggplot(data = cehd, aes(x = years_at_u, y = annual_pay)) +
  geom_point() +
  facet_wrap(~ department)


# Put facets in a single row
ggplot(data = cehd, aes(x = years_at_u, y = annual_pay)) +
  geom_point() +
  facet_wrap(~ department, nrow = 2)



##################################################
### Add axis label
##################################################

ggplot(data = cehd, aes(x = years_at_u, y = annual_pay)) +
  geom_point() +
  facet_wrap(~ department, nrow = 2) +
  xlab("Years at the University of Minnesota")



##################################################
### Change axis limits
##################################################

ggplot(data = cehd, aes(x = years_at_u, y = annual_pay)) +
  geom_point() +
  facet_wrap(~ department, nrow = 2) +
  xlab("Years at the University of Minnesota") +
        ylim(0, 200000)
        



##################################################
### Adios-ing scientific notation
##################################################

options(scipen = 10000)

ggplot(data = cehd, aes(x = years_at_u, y = annual_pay)) +
  geom_point() +
  facet_wrap(~ department, nrow = 2) +
  xlab("Years at the University of Minnesota") +
  scale_x_continuous(labels = comma)




##################################################
### Fine-tuning the axis scales
##################################################

ggplot(data = cehd, aes(x = years_at_u, y = annual_pay)) +
  geom_point() +
  facet_wrap(~ department, nrow = 2) +
  xlab("Years at the University of Minnesota") +
  scale_y_continuous(
    name = "Annual Base Salary", 
    breaks = c(50000, 100000, 150000, 200000, 250000, 300000)
  )



##################################################
### Prettying up scales
##################################################

library(scales)

ggplot(data = cehd, aes(x = years_at_u, y = annual_pay)) +
  geom_point() +
  facet_wrap(~ department, nrow = 2) +
  xlab("Years at the University of Minnesota") +
  scale_y_continuous(
    name = "Annual Base Salary",
    labels = dollar
  )



##################################################
### Customize color
##################################################

ggplot(data = cehd, aes(x = years_at_u, y = annual_pay)) +
  geom_point(aes(color = title)) +
  scale_color_manual(
    values = c("#599ad3", "#f9a65a", "#9e66ab")
    )



##################################################
### Change legend title
##################################################

ggplot(data = cehd, aes(x = years_at_u, y = annual_pay)) +
  geom_point(aes(color = title)) +
  scale_color_manual(
    values = c("#599ad3", "#f9a65a", "#9e66ab"),
    name = "Faculty Title"
  )


##################################################
### Default palette
##################################################

ggplot(data = cehd, aes(x = years_at_u, y = annual_pay)) +
  geom_point(aes(color = title)) +
  scale_color_hue(name = "Faculty Title")



##################################################
### Greyscale palette
##################################################

ggplot(data = cehd, aes(x = years_at_u, y = annual_pay)) +
  geom_point(aes(color = title)) +
  scale_color_grey(name = "Faculty Title")



##################################################
### Color brewer palette
##################################################

ggplot(data = cehd, aes(x = years_at_u, y = annual_pay)) +
  geom_point(aes(color = title)) +
  scale_color_brewer(
    name = "Faculty Title",
    palette = "Set2"
    )



##################################################
### Changing theme elements
##################################################

ggplot(data = cehd, aes(x = years_at_u, y = annual_pay)) +
  geom_point(aes(color = title)) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "italic"),
    legend.title = element_text(color = "blue")
    )



##################################################
### Black and white background
##################################################

ggplot(data = cehd, aes(x = years_at_u, y = annual_pay)) +
  geom_point(aes(color = title)) +
  theme_bw()



##################################################
### Customized themes
##################################################

# Install ggthemes package and then load it
library(ggthemes)

# Wall Street Journal theme
ggplot(data = cehd, aes(x = years_at_u, y = annual_pay)) +
  geom_point(aes(color = title)) +
  theme_wsj() +
  scale_color_wsj(name = "Faculty Title", palette = "rgby")



##################################################
### Putting it all together
##################################################

ggplot(data = cehd, aes(x = years_at_u, y = annual_pay)) +
  geom_point(aes(color = title)) +
  scale_color_brewer(name = "Faculty Title", palette = "Blues") +
  xlab("Years at the University of Minnesota") +
  scale_y_continuous(name = "Annual Base Salary", labels = dollar) +
  theme_bw() +
  facet_wrap(~ department, nrow = 2) +
  guides(color = FALSE)



