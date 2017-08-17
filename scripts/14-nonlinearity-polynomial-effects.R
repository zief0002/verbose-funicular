###################################################
### Load libraries
###################################################

library(broom)
library(ggplot2)
library(readr)
library(sm)



###################################################
### Read in the data
###################################################

mn = read_csv(file = "~/Dropbox/epsy-8251/data/mnSchools.csv")
head(mn)



###################################################
### Relationship between gradRate and SAT
###################################################

ggplot(data = mn, aes(x = sat, y = gradRate)) +
  geom_point() +
  #geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("Estimated median SAT score") +
  ylab("Six-year graduation rate")



###################################################
### Relationship between gradRate and SAT by looking at the residuals
###################################################

# Fit linear model
lm.1 = lm(gradRate ~ 1 + sat, data = mn)


# Obtain residuals
out = augment(lm.1)


# Examine assumption of linearity
ggplot(data = out, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  theme_bw()




###################################################
### Fit polynomial model
###################################################

# Create quadratic term
mn = mn %>% mutate(sat_quadratic = sat * sat)
head(mn)


# Fit polynomial (quadratic) model
lm.2 = lm(gradRate ~ 1 + sat + sat_quadratic, data = mn)
summary(lm.2)


# Better way to fit the same model
lm.2 = lm(gradRate ~ 1 + sat + I(sat ^ 2), data = mn)
summary(lm.2)



###################################################
### Plot model results
###################################################

# Set up data
plotData = expand.grid(
  sat = seq(from = 890, to = 1400, by = 10)
)

# Predict
plotData = plotData %>% mutate(yhat = predict(lm.2, newdata = plotData))

# Examine data
head(plotData)

# Plot
ggplot(data = plotData, aes(x = sat, y = yhat)) +
  geom_line() +
  theme_bw() +
  xlab("Estimated median SAT score") +
  ylab("Predicted graduation rate")




###################################################
### Examine residuals
###################################################

# Fortify the model
out2 = augment(lm.2)

# Check normality
sm.density(out2$.std.resid, model = "normal")

# Check other assumptions
ggplot(data = out2, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  theme_bw()




###################################################
### Add controls
###################################################

lm.3 = lm(gradRate ~ 1 + sat + I(sat ^ 2) + public, data = mn)
summary(lm.3)



###################################################
### Plot model results
###################################################

# Set up data
plotData = expand.grid(
  sat = seq(from = 890, to = 1400, by = 10),
  public = c(0, 1)
)

# Predict
plotData = plotData %>% mutate(yhat = predict(lm.3, newdata = plotData))

# Coerce public into a factor for better plotting
plotData = plotData %>% 
  mutate(public = factor(plotData$public,
                         levels = c(0, 1), 
                         labels = c("Private", "Public")
                         )
  )

# Examine data
head(plotData)

# Plot
ggplot(data = plotData, aes(x = sat, y = yhat, group = public, color = public)) +
  geom_line() +
  theme_bw() +
  xlab("Estimated median SAT score") +
  ylab("Predicted graduation rate") +
  scale_color_brewer(name = "Sector", palette = "Set1")







