##################################################
### Load libraries
##################################################

library(broom)
library(corrr)
library(dplyr)
library(educate)
library(ggplot2)
library(readr)
library(tidyr)
library(ungeviz)



##################################################
### Set options so up to 6 significant digits print
##################################################

options(pillar.sigfig = 6)



##################################################
### Read in data
##################################################

mn = read_csv(file = "~/Documents/github/epsy-8251/data/mn-schools.csv")
head(mn)




##################################################
### Exploration
##################################################

# Scatterplot
ggplot(data = mn, aes(x = as.factor(public), y = grad)) +
  geom_point() +
  theme_bw() +
  scale_x_discrete(name = "Educational sector", labels = c("Private", "Public")) +
  ylab("Six-year graduation rate")


# Descriptive statistics
mn %>% 
  group_by(public) %>%
  summarize(
    M = mean(grad),
    SD = sd(grad),
    N = length(grad)
  )


# Correlation matrix
mn %>%
  select(grad, public) %>%
  correlate() %>%
  fashion(decimals = 3)



##################################################
### Fit regression model
##################################################

lm_public = lm(grad ~ 1 + public, data = mn)

glance(lm_public)
tidy(lm_public)




##################################################
### Examine assumptions
##################################################

# Augment the model
model_output = augment(lm_public)
head(model_output)


# Density plot of the marginal standardized residuals
ggplot(data = model_output, aes(x = .std.resid)) +
  stat_watercolor_density(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Studentized residuals") +
  ylab("Probability density")


# Scatterplot of the studentized residuals versus the fitted values
ggplot(data = model_output, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Studentized residuals")



##################################################
### Examine assumptions by sector
##################################################

model_output_private = model_output %>% 
  filter(public == 0)

# Get public schools
model_output_public = model_output %>% 
  filter(public == 1)

# Density plot of the private schools' sttudentized residuals
ggplot(data = model_output_private, aes(x = .std.resid)) +
  stat_watercolor_density(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Studentized residuals") +
  ylab("Probability density")

# Density plot of the public schools' sttudentized residuals
ggplot(data = model_output_public, aes(x = .std.resid)) +
  stat_watercolor_density(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Studentized residuals") +
  ylab("Probability density")



##################################################
### Including other predictors (ANCOVA model)
##################################################

# Correlation matrix
mn %>%
  select(grad, public, sat) %>%
  correlate() %>%
  fashion(decimals = 3)


# Fit regression model
lm.2 = lm(grad ~ 1 + public + sat, data = mn)

# Model-level info
glance(lm.2)

# Coefficient-level info
tidy(lm.2)



##################################################
### Compute adjusted means
##################################################

# Compute mean SAT
mean(mn$sat)

# Compute adjusted mean for private schools
-76.1 - 8.4*0 + 0.127*1101.2

# Compute adjusted mean for public schools
-76.1 - 8.4*1 + 0.127*1101.2

# Compute adjusted mean difference
63.7 - 55.4



##################################################
### Fit ANCOVA model 2
##################################################

# Correlation matrix
mn %>%
  select(grad, public, sat, tuition) %>%
  correlate() %>%
  fashion(decimals = 3)

# Fit regression model
lm.3 = lm(grad ~ 1 + public + sat + tuition, data = mn)

# Model-level info
glance(lm.3)

# Coefficient-level info
tidy(lm.3)



##################################################
### Check assumptions
##################################################

# Augment the model
lm.3_output = augment(lm.3)

head(lm.3_output)


# Density plot of the marginal studentized residuals
ggplot(data = lm.3_output, aes(x = .std.resid)) +
  stat_watercolor_density(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Studentized residuals") +
  ylab("Probability density")


# Scatterplot of the studentized residuals versus the fitted values
ggplot(data = lm.3_output, aes(x = .fitted, y = .std.resid)) +
  geom_point(size = 4) +
  geom_smooth(se = TRUE) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Standardized residuals")



##################################################
### Coefficient plot
##################################################

# Create tidy() objects and identify each with a model column
m1 = tidy(lm_public) %>% mutate(model = "Model 1")
m2 = tidy(lm.2) %>% mutate(model = "Model 2")
m3 = tidy(lm.3) %>% mutate(model = "Model 3")


# Combine all three tidy() outputs, filter out intercepts, and drop missing values
all_models = rbind(m1, m2, m3) %>%
  filter(term != "(Intercept)") %>%
  drop_na()


# Create coefficient plots
ggplot(data = all_models, aes(x = estimate, y = term)) +
  stat_confidence_density(aes(moe = std.error, confidence = 0.68, fill = stat(ndensity)), 
                          height = 0.15) +
  geom_point(aes(x = estimate), size = 2) +
  scale_fill_gradient(low = "#eff3ff", high = "#6baed6") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(name = "Estimate", limits = c(-25, 5)) +
  scale_y_discrete(
    name = "Coefficients", 
    labels = c("Public institution", "Median SAT score", "Tuition")
    ) +
  facet_wrap(~model)



##################################################
### Plot of the fitted model (lm.3)
##################################################

ggplot(data = mn, aes(x = sat, y = grad)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = -68.93, slope = 0.10, color = "#e69f00", linetype = "solid") +
  geom_abline(intercept = -68.28, slope = 0.10, color = "#56b4e9", linetype = "dashed") +
  theme_bw() +
  xlab("Median SAT score") +
  ylab("Predicted graduation rate")

