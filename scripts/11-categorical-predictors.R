##################################################
### Load libraries
##################################################

library(broom)
library(corrr)
library(dotwhisker)
library(dplyr)
library(ggplot2)
library(readr)
library(sm)
library(tidyr)



##################################################
### Read in data
##################################################

mn = read_csv(file = "~/Documents/github/epsy-8251/data/mn-schools.csv")
head(mn)




##################################################
### Exploration
##################################################

# Scatterplot
ggplot(data = mn, aes(x = as.factor(public), y = gradRate)) +
  geom_point(size = 5) +
  theme_bw() +
  scale_x_discrete(name = "Educational sector", labels = c("Private", "Public")) +
  ylab("Six-year graduation rate")


# Descriptive statistics
mn %>% 
  group_by(public) %>%
  summarize(
    M = mean(gradRate),
    SD = sd(gradRate),
    N = length(gradRate)
  )


# Correlation matrix
mn %>%
  select(gradRate, public) %>%
  correlate() %>%
  fashion(decimals = 3)



##################################################
### Fit regression model
##################################################

lm_public = lm(gradRate ~ 1 + public, data = mn)

glance(lm_public)
tidy(lm_public)




##################################################
### Examine assumptions
##################################################

# Augment the model
out = augment(lm_public)
head(out)


# Density plot of the marginal standardized residuals
sm.density(out$.std.resid, model = "normal")


# Normality by sector
out_private = out %>% filter(public == 0)
sm.density(out_private$.std.resid, model = "normal")

out_public = out %>% filter(public == 1)
sm.density(out_public$.std.resid, model = "normal")


# Scatterplot of the standardized residuals versus the fitted values
ggplot(data = out, aes(x = .fitted, y = .std.resid)) +
  geom_point(size = 4) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  xlab("Fitted values") +
  ylab("Standardized residuals")



##################################################
### Including other predictors (ANCOVA model)
##################################################

# Correlation matrix
mn %>%
  select(gradRate, public, sat) %>%
  correlate() %>%
  fashion(decimals = 3)


# Fit regression model
lm.2 = lm(gradRate ~ 1 + public + sat, data = mn)

glance(lm.2)
tidy(lm.2)



##################################################
### Compute adjusted means
##################################################

# Compute mean SAT
m_sat = mean(mn$sat)

# Compute adjusted means
avg_inst = crossing(
  public = c(0, 1),
  sat = m_sat
)

predict(lm.2, newdata = avg_inst)

# Compute adjusted mean difference
63.5 - 55.1



##################################################
### Fit ANCOVA model 2
##################################################

# Correlation matrix
mn %>%
  select(gradRate, public, sat, tuition) %>%
  correlate() %>%
  fashion(decimals = 3)

# Fit regression model
lm.3 = lm(gradRate ~ 1 + public + sat + tuition, data = mn)

glance(lm.3)
tidy(lm.3)



##################################################
### Check assumptions
##################################################

# Augment the model
out3 = augment(lm.3)
head(out3)


# Density plot of the marginal standardized residuals
sm.density(out3$.std.resid, model = "normal")


# Scatterplot of the standardized residuals versus the fitted values
ggplot(data = out3, aes(x = .fitted, y = .std.resid)) +
  geom_point(size = 4) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  xlab("Fitted values") +
  ylab("Standardized residuals")



##################################################
### Coefficient plot
##################################################

# Create tidy model objects
m1 = tidy(lm_public) %>% mutate(model = "Model 1")
m2 = tidy(lm.2) %>% mutate(model = "Model 2")
m3 = tidy(lm.3) %>% mutate(model = "Model 3")

# Bind into a single object
all_models = rbind(m1, m2, m3)

# Coefficient plot
dw_plot(all_models, show_intercept = FALSE) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.4) +
  scale_y_discrete(
    name = "", 
    labels = c("Tuition", "Median SAT score", "Public")
  ) +
  scale_color_manual(
    name = "", 
    labels = c("Model 1", "Model 2", "Model 3"),
    values = c("#999999", "#e69f00", "#56b4e9")
  )



##################################################
### Plot of the fitted model (lm.3)
##################################################

# Set up the data to plot
plot_data = crossing(
  sat = seq(from = 890, to = 1400, by = 10), 
  public = c(0, 1),
  tuition = mean(mn$tuition)
) 

plot_data %>%
  mutate(
    # Get predicted values
    yhat = predict(lm.3, newdata = plot_data),
    # Change public into a factor
    sector = factor(public, 
                    levels = c(0, 1), 
                    labels = c("Public institution", "Private institution")
    )
  ) %>%
  # Create plot
  ggplot(aes(x = sat, y = yhat, group = sector, color = sector)) +
  geom_line() +
  theme_bw() + 
  xlab("Median SAT score") + 
  ylab("Predicted graduation rate") +
  scale_color_viridis_d(name = "")

