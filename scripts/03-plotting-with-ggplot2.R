##################################################
### Load libraries
##################################################

library(dplyr)
library(ggplot2)
library(readr)



##################################################
### Read in data
##################################################

city = read_csv("~/Documents/github/epsy-8251/data/riverview.csv")

head(city)



##################################################
### Understanding the basic syntax
##################################################

ggplot(data = city, aes(x = education, y = income)) + 
  geom_point() +
  theme_bw()



##################################################
### Adding layers
##################################################

ggplot(data = city, aes(x = education, y = income)) + 
  geom_point() + 
  geom_smooth()



##################################################
### Good syntactic habits
##################################################

ggplot(data = city, aes(x = education, y = income)) +
  geom_point() +
  geom_smooth() +
  theme_bw()



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

ggplot(data = city, aes(x = education, y = income)) + 
  geom_point(color = "red", size = 4) +
  theme_bw()



##################################################
### Your Turn 
##################################################

ggplot(data = city, aes(x = education, y = income, color = gender)) +
  geom_point() +
  geom_smooth(color = "yellow", fill = "darkblue")




##################################################
### Useful point aesthetics
##################################################

ggplot(data = city, aes(x = education, y = income)) +
  geom_point(aes(color = party, shape = gender), size = 4)



##################################################
### Faceting: Separate plots for subgroups
##################################################

ggplot(data = city, aes(x = education, y = income)) +
  geom_point() +
  facet_wrap(~ party, nrow = 2)



##################################################
### Faceting on multiple variables
##################################################

ggplot(data = city, aes(x = education, y = income)) +
  geom_point() +
  facet_grid(party ~ gender)



##################################################
### Changing the axis label
##################################################

ggplot(data = city, aes(x = education, y = income)) +
  geom_point() +
  xlab("Education level (in years)") +
  ylab("Income level (in U.S. dollars)")



##################################################
### Changing the axis limits
##################################################

ggplot(data = city, aes(x = education, y = income)) +
  geom_point(size = 4) +
  xlab("Education level (in years)") +
  ylab("Income level (in U.S. dollars)") +
  xlim(0, 30) +
  ylim(0, 100000)



##################################################
### Fine-tuning the axis scales
##################################################

ggplot(data = city, aes(x = education, y = income)) +
  geom_point() +
  scale_x_continuous(
    name = "Education level (in years)",
    breaks = c(10, 12, 14, 16, 18, 20, 22, 24)
  )



##################################################
### Customizing the color
##################################################

ggplot(data = city, aes(x = seniority, y = education)) +
  geom_point(aes(color = party)) +
  scale_color_manual(
    values = c("#00afbb", "#e7b800", "#fc4e07")
  )



##################################################
### Customizing the legend
##################################################

ggplot(data = city, aes(x = seniority, y = education)) +
  geom_point(aes(color = party)) +
  scale_color_manual(
    name = "Political affiliation",
    values = c("#00afbb", "#e7b800", "#fc4e07"),
    labels = c("Dem", "Ind", "Rep")
  )



##################################################
### Greyscale palette
##################################################

ggplot(data = city, aes(x = education, y = income)) +
  geom_point(aes(color = party)) +
  scale_color_grey(
    name = "Political affiliation"
  )



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
### Themes
##################################################

ggplot(data = city, aes(x = education, y = income)) +
  geom_point() +
  theme_bw()



##################################################
### Fine-tuning the theme
##################################################

ggplot(data = city, aes(x = education, y = income)) +
  geom_point() +
  theme_bw() +
  theme(
    axis.title.x = element_text(face = "bold", color = "blue"),
    axis.title.y = element_text(face = "italic")
  )



##################################################
### The ggthemes package
##################################################

# Load ggthemes package
library(ggthemes)

# Plot with Wall Street Journal theme
ggplot(data = city, aes(x = education, y = income)) +
  geom_point() +
  theme_wsj()



##################################################
### The scales package
##################################################

# Load the scales package
library(scales)

# Plot with dollar signs on the Y labels
ggplot(data = city, aes(x = education, y = income)) +
  geom_point() +
  scale_y_continuous(
    name = "Annual income",
    labels = dollar
  )



##################################################
### Your turn
##################################################

# Load scales library
library(scales) # To format $ in the labels

# Plot
ggplot(data = city, aes(x = education, y = income)) +
  geom_point(aes(color = party), size = 4) +
  scale_color_manual(
    name = "Political affiliation", 
    values = c("#00afbb", "#e7b800", "#fc4e07")
  ) +
  xlab("Education level (in years)") +
  scale_y_continuous(
    name = "Annual income (in U.S. dollars)", 
    labels = dollar
  ) +
  theme_bw() +
  facet_wrap(~ gender)



##################################################
### Combining ggplot with dplyr
##################################################

city %>%
  filter(gender == "female") %>%
  ggplot(aes(x = education, y = income)) +
  geom_point()

