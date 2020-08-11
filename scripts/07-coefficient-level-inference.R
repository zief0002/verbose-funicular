##################################################
### Load libraries
##################################################

library(broom)
library(corrr)
library(dotwhisker)
library(dplyr)
library(ggExtra)
library(ggplot2)
library(readr)



##################################################
### Set options so up to 6 significant digits print
##################################################

options(pillar.sigfig = 6)



##################################################
### Read in data
##################################################

keith = read_csv(file = "https://raw.githubusercontent.com/zief0002/modeling/master/data/keith-gpa.csv")
head(keith)



##################################################
### Exploration
##################################################

# Scatterplot
p1 = ggplot( data = keith, aes(x = homework, y = gpa) ) +
  geom_point() +
  theme_bw() +
  xlab("Time spent on homework per week (in hours)") +
  ylab("GPA (on a 100-pt. scale)")


# Plot scatterplot and density plots on single graph
ggMarginal(p1, margins = "both", type = "density")


# Summary statistics
keith %>%
  summarize(
    M_gpa  = mean(gpa),
    SD_gpa = sd(gpa),
    M_hw   = mean(homework),
    SD_hw  = sd(homework)
    )


# Correlation
keith %>%
  select(gpa, homework) %>%
  correlate()



##################################################
### Fit regression model
##################################################

lm.a = lm(gpa ~ 1 + homework, data = keith)
lm.a



##################################################
### Coefficient-level output
##################################################

tidy(lm.a)



##################################################
### Interval estimates of the regression parameters
##################################################

tidy(lm.a, conf.int = TRUE, conf.level = 0.95)



##################################################
### Coefficient plot -- All coefficients
##################################################

# Store output from tidy
mod_1 = tidy(lm.a) %>%
  mutate(model = "Model A")


# Create plot
dwplot(mod_1, show_intercept = TRUE) +
  theme_bw() +
  scale_color_manual(
    name = "Model", 
    values = c("#c62f4b")
    ) +
  scale_x_continuous(name = "Estimate") +
  scale_y_discrete(
    name = "Coefficients", 
    labels = c("Time spent\non homework", "Intercept")
    )



##################################################
### Coefficient plot -- Omit intercept
##################################################

dwplot(mod_1, show_intercept = FALSE) +
  theme_bw() +
  scale_color_manual(
    name = "Model", 
    values = c("#c62f4b")
  ) +
  scale_x_continuous(name = "Estimate") +
  scale_y_discrete(
    name = "Coefficients", 
    labels = c("Time spent\non homework", "Intercept")
  )

