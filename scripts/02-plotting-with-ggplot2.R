##################################################
### Load libraries
##################################################

library(ggplot2)
library(readr)



##################################################
### Read in data
##################################################

city = read_csv("~/Dropbox/epsy-8251/data/riverside.csv")

head(city)
tail(city)

summary(city)



##################################################
### Scatterplot of income versus education
##################################################

ggplot(data = city, aes(x = education, y = income)) + 
  geom_point()



##################################################
### Add a loess smoother
##################################################

ggplot(data = city, aes(x = education, y = income)) + 
  geom_point() + 
  geom_smooth()



##################################################
### More readable syntax
##################################################

ggplot(data = city, aes(x = education, y = income)) +
  geom_point() +
  geom_smooth()



##################################################
### Global vs local aesthetics
##################################################

# Global aesthetics
ggplot(data = city, aes(x = education, y = income, color = gender)) +
  geom_point() +
  geom_smooth()


# local aesthetics
ggplot(data = city, aes(x = education, y = income)) +
  geom_point(aes(color = gender)) +
  geom_smooth()



##################################################
### Fixed aesthetics
##################################################

ggplot(data = city, aes(x = education, y = income, color = gender)) +
  geom_point() +
  geom_smooth(color = "yellow", fill = "darkblue") 



##################################################
### Other useful point aesthetics
##################################################

ggplot(data = city, aes(x = seniority, y = education)) +
  geom_point(aes(color = party, pch = gender), size = 5)



##################################################
### Facet on political party
##################################################

ggplot(data = city, aes(x = education, y = income)) +
  geom_point() +
  facet_wrap(~ party)



##################################################
### Add axis label
##################################################

ggplot(data = city, aes(x = education, y = income)) +
  geom_point() +
  facet_wrap(~ party) +
  xlab("Education level (in years)")



##################################################
### Change axis limits
##################################################

ggplot(data = city, aes(x = education, y = income)) +
  geom_point() +
  facet_wrap(~ party) +
  ylim(0, 100000)



##################################################
### Fine-tuning the axis scales
##################################################

ggplot(data = city, aes(x = education, y = income)) +
  geom_point() +
  facet_wrap(~ party) +
  scale_x_continuous(
    name = "Education level (in years)",
    breaks = c(10, 12, 14, 16, 18, 20, 22, 24)
  )



##################################################
### Prettying up scales
##################################################

library(scales)

ggplot(data = city, aes(x = education, y = income)) +
  geom_point() +
  facet_wrap(~ party) +
  scale_y_continuous(
    name = "Annual income",
    labels = dollar
    )



##################################################
### Customize color
##################################################

ggplot(data = city, aes(x = education, y = income)) +
  geom_point(aes(color = party)) +
  scale_color_manual(
    values = c("#00afbb", "#e7b800", "#fc4e07")
  )



##################################################
### Change legend title
##################################################

ggplot(data = city, aes(x = education, y = income)) +
  geom_point(aes(color = party)) +
  scale_color_manual(
    values = c("#00afbb", "#e7b800", "#fc4e07"),
    name = "Political affiliation"
  )



##################################################
### Greyscale palette
##################################################

ggplot(data = city, aes(x = education, y = income)) +
  geom_point(aes(color = party)) +
  scale_color_grey(name = "Political affiliation")



##################################################
### Color brewer palette
##################################################

ggplot(data = city, aes(x = education, y = income)) +
  geom_point(aes(color = party)) +
  scale_color_brewer(
    name = "Political affiliation",
    palette = "Set2"
    )



##################################################
### Changing theme elements
##################################################

ggplot(data = city, aes(x = education, y = income)) +
  geom_point(aes(color = party)) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "italic"),
    legend.title = element_text(color = "blue")
    )



##################################################
### Black and white background
##################################################

ggplot(data = city, aes(x = education, y = income)) +
  geom_point(aes(color = party)) +
  theme_bw()



##################################################
### Customized themes
##################################################

# Install ggthemes package and then load it
library(ggthemes)

# Wall Street Journal theme
ggplot(data = city, aes(x = education, y = income)) +
  geom_point(aes(color = party)) +
  theme_wsj() +
  scale_color_wsj(name = "Political affiliation", palette = "rgby")



##################################################
### Putting it all together
##################################################

ggplot(data = city, aes(x = education, y = income)) +
  geom_point(aes(color = party)) +
  scale_color_manual(
    name = "Political affiliation", 
    values = c("#00afbb", "#e7b800", "#fc4e07")
  ) +
  xlab("Education level (in years)") +
  scale_y_continuous(
    name = "Annual income", 
    labels = dollar
  ) +
  theme_bw() +
  facet_wrap(~gender)



