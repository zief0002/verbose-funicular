fert = read_csv(file.choose())

fert = fert %>%
  mutate(educ_gni = educ_female * high_gni)

lm.1 = lm(infant_mortality ~ 1 + educ_female + high_gni + educ_gni, data = fert)
broom::tidy(lm.1)


ggplot(data = fert, aes(x = educ_female, y = infant_mortality)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = 69.4, slope = -4.93, color = "blue") +
  geom_abline(intercept = 69.4 - 38.5, slope = -4.93 + 2.89, color = "darkorange", linetype = "dashed") +
  theme_light() +
  xlab("Female education level") +
  ylab("Infant mortality rate")
