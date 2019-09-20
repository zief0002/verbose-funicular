##################################################
### Load libraries
##################################################

library(broom)
library(corrr)
library(dplyr)
library(ggplot2)
library(readr)



##################################################
### Set options so up to 6 significant digits print
##################################################

options(pillar.sigfig = 6)



##################################################
### Read in data
##################################################

keith = read_csv(file = "~/Documents/github/epsy-8251/data/keith-gpa.csv")

head(keith)



##################################################
### Exploration
##################################################

# Density plots of the marginal distribution of homework
ggplot(data = keith, aes(x = gpa)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "yellow") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("GPA (on a 100-pt. scale)") +
  ylab("Probability density") +
  ggtitle("Outcome: GPA")


# Density plots of the marginal distribution of homework
ggplot(data = keith, aes(x = homework)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "yellow") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Time spent on homework per week (in hours)") +
  ylab("Probability density")  +
  ggtitle("Predictor: Homework")


# Scatterplot
ggplot( data = keith, aes(x = homework, y = gpa) ) +
  geom_point() +
  theme_bw() +
  xlab("Time spent on homework per week (in hours)") +
  ylab("GPA (on a 100-pt. scale)")


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

lm.1 = lm(gpa ~ 1 + homework, data = keith)
lm.1



##################################################
### Coefficient-level output
##################################################

tidy(lm.1)



##################################################
### Interval estimates of the regression parameters
##################################################

confint(lm.1, level = 0.95)



##################################################
### Coefficient plot
##################################################

# Install ungeviz package (only need to do this once)
# library(devtools)
# install_github("wilkelab/ungeviz")


# Load ungeviz library
library(ungeviz)


# Create plot
ggplot(data = tidy(lm.1), aes(x = estimate, y = term)) +
  stat_confidence_density(aes(moe = std.error, confidence = 0.68, fill = stat(ndensity)), 
                          height = 0.15) +
  geom_point(aes(x = estimate), size = 2) +
  scale_fill_gradient(low = "#eff3ff", high = "#6baed6") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(name = "Estimate", limits = c(0, 80)) +
  scale_y_discrete(name = "Coefficients", labels = c("Intercept", "Time spent\non homework"))







