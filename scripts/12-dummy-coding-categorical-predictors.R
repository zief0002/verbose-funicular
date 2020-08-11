##################################################
### Load libraries
##################################################

library(broom)
library(corrr)
library(dplyr)
library(dotwhisker)
library(educate)
library(ggplot2)
library(patchwork)
library(readr)



##################################################
### Set options so up to 6 significant digits print
##################################################

options(pillar.sigfig = 6)



##################################################
### Read in data
##################################################

mn = read_csv(file = "https://raw.githubusercontent.com/zief0002/modeling/master/data/mn-schools.csv")
head(mn)



##################################################
### Exploration
##################################################

# Density plot of graduation rates
ggplot(data = mn, aes(x = grad)) +
  stat_density(geom = "line", color = "#c62f4b") +
  theme_bw() +
  xlab("Six-year graduation rate") +
  ylab("Probability density")


# Bar plot of education sector
ggplot(data = mn, aes(x = sector)) +
  geom_bar(fill = "#c62f4b") +
  theme_bw() +
  xlab("Educational sector") +
  ylab("Frequency")


# Scatterplot
ggplot(data = mn, aes(x = sector, y = grad)) +
  geom_point() +
  theme_bw() +
  xlab("Educational sector") +
  ylab("Six-year graduation rate")


# Summary statistics
mn %>% 
  group_by(sector) %>%
  summarize(
    M = mean(grad),
    SD = sd(grad),
    N = length(grad)
  )



##################################################
### Indicator variables: 5 and 10
##################################################

# Create indicator variable
mn = mn %>%
  mutate(
    indicator = if_else(sector == "Public", 5, 10)
  )


# Examine data frame
head(mn)


# Correlation
mn %>%
  select(grad, indicator) %>%
  correlate() %>%
  fashion(decimals = 3)


# Fit regression model
lm.a = lm(grad ~ 1 + indicator, data = mn)

print(glance(lm.a), width = Inf) # Model-level output
tidy(lm.a)                       # Coefficient-level output



##################################################
### Indicator variables: 2 and 7
##################################################

# Create indicator variable
mn = mn %>%
  mutate(
    indicator_2 = if_else(sector == "Public", 2, 7)
  )


# Examine data frame
head(mn)


# Correlation
mn %>%
  select(grad, indicator_2) %>%
  correlate() %>%
  fashion(decimals = 3)


# Fit regression model
lm.b = lm(grad ~ 1 + indicator_2, data = mn)

print(glance(lm.b), width = Inf) # Model-level output
tidy(lm.b)                       # Coefficient-level output



##################################################
### Dummy indicator variables: 0 and 1 (private)
##################################################

# Create indicator variable
mn = mn %>%
  mutate(
    private = if_else(sector == "Private", 1, 0)
  )


# Examine data frame
head(mn)


# Correlation
mn %>%
  select(grad, private) %>%
  correlate() %>%
  fashion(decimals = 3)


# Fit regression model
lm.c = lm(grad ~ 1 + private, data = mn)

print(glance(lm.c), width = Inf) # Model-level output
tidy(lm.c)                       # Coefficient-level output



##################################################
### Private schools as reference group
##################################################

# Create indicator variable
mn = mn %>%
  mutate(
    public = if_else(sector == "Public", 1, 0)
  )


# Examine data frame
head(mn)


# Correlation
mn %>%
  select(grad, public) %>%
  correlate() %>%
  fashion(decimals = 3)


# Fit regression model
lm.d = lm(grad ~ 1 + public, data = mn)

print(glance(lm.d), width = Inf) # Model-level output
tidy(lm.d)                       # Coefficient-level output



##################################################
### Examine assumptions
##################################################

# Obtain the fitted values and residuals
aug_d = augment(lm.d)


# View augmented data frame
head(aug_d)


# Density plot of the marginal standardized residuals
ggplot(data = aug_d, aes(x = .std.resid)) +
  stat_density_confidence(model ="normal") +
  stat_density(geom = "line", color = "#c62f4b") +
  theme_bw() +
  xlab("Standardized residual") +
  ylab("Probability density")


# Scatterplot of the standardized residuals versus the fitted values
ggplot(data = aug_d, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted value") +
  ylab("Standardized residual")



##################################################
### Examine assumptions by sector
##################################################

# Get private schools
aug_private = aug_d %>% 
  filter(public == 0)


# Get public schools
aug_public = aug_d %>% 
  filter(public == 1)


# Density plot of the private schools' standardized residuals
ggplot(data = aug_private, aes(x = .std.resid)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line", color = "#c62f4b") +
  theme_bw() +
  xlab("Standardized residual") +
  ylab("Probability density")


# Density plot of the public schools' standardized residuals
ggplot(data = aug_public, aes(x = .std.resid)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line", color = "#c62f4b") +
  theme_bw() +
  xlab("Standardized residual") +
  ylab("Probability density")



##################################################
### Including other predictors (ANCOVA model)
##################################################

# Correlation matrix
mn %>%
  select(grad, private, sat) %>%
  correlate() %>%
  fashion(decimals = 3)


# Fit regression model
lm.e = lm(grad ~ 1 + sat + private, data = mn)

print(glance(lm.e), width = Inf) # Model-level
tidy(lm.e)                       # Coefficient-level info



##################################################
### Compute adjusted means
##################################################

# Compute mean SAT
mn %>%
  summarize(
    M = mean(sat)
    )


# Compute adjusted mean for private schools
-84.4 + 0.127*1101.2 + 8.4*1


# Compute adjusted mean for public schools
-84.4 + 0.127*1101.2 + 8.4*0


# Compute adjusted mean difference
63.9 - 55.5



##################################################
### One last model
##################################################

# Correlation matrix
mn %>%
  select(grad, private, sat, tuition) %>%
  correlate() %>%
  fashion(decimals = 3)


# Fit regression model
lm.f = lm(grad ~ 1 + sat + tuition + private, data = mn)

print(glance(lm.f), width = Inf) # Model-level
tidy(lm.f)                       # Coefficient-level info


# Obtain the fitted values and residuals
aug_f = augment(lm.f)


# View augmented data frame
head(aug_f)


# Density plot of the marginal standardized residuals
ggplot(data = aug_f, aes(x = .std.resid)) +
  stat_density_confidence(model ="normal") +
  stat_density(geom = "line", color = "#c62f4b") +
  theme_bw() +
  xlab("Standardized residual") +
  ylab("Probability density")


# Scatterplot of the standardized residuals versus the fitted values
ggplot(data = aug_f, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted value") +
  ylab("Standardized residual")



##################################################
### Coefficient plot
##################################################

# Create tidy() objects and identify each with a model column
m1 = tidy(lm.c) %>% mutate(model = "Model A")
m2 = tidy(lm.e) %>% mutate(model = "Model B")
m3 = tidy(lm.f) %>% mutate(model = "Model C")


# Combine all three tidy() outputs, filter out intercepts, and drop missing values
all_models = rbind(m1, m2, m3)


# Create plot
dwplot(all_models, show_intercept = FALSE) +
  theme_bw() +
  scale_color_manual(name = "Model", values = c("#c62f4b", "#c62f4b", "#c62f4b")) +
  scale_x_continuous(name = "Estimate") +
  scale_y_discrete(
    name = "Coefficients", 
    labels = c("Tuition", "Median SAT score", "Private institution\n(dummy coded)")
  ) +
  guides(color = FALSE) +
  facet_wrap(~model)



##################################################
### Plot of the fitted model (mean tuition rate)
##################################################

ggplot(data = mn, aes(x = sat, y = grad)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = -53.7, slope = 0.10, color = "#e69f00", linetype = "solid") +
  geom_abline(intercept = -53.0, slope = 0.10, color = "#56b4e9", linetype = "dashed") +
  theme_bw() +
  xlab("Median SAT score") +
  ylab("Predicted graduation rate")


