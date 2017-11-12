mn = read_csv(file = "~/Dropbox/epsy-8251/data/mnSchools.csv")

head(mn)
tail(mn)

lm.3 = lm(gradRate ~ 1 + public + sat + tuition, data = mn)


# Augment the model
out3 = augment(lm.3) %>%
  mutate(college = mn$name)
head(out3)


# Scatterplot of the standardized residuals versus the fitted values
ggplot(data = out3, aes(x = .fitted, y = .std.resid)) +
  geom_text(aes(label = college)) +
  theme_bw() +
  geom_hline(yintercept = 0)





