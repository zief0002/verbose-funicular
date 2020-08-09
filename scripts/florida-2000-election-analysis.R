fl = read_csv("~/Desktop/florida-vote-2000.csv")
head(fl)

nrow(fl)

fl_no_pbc = fl %>% filter(county != "Palm Beach")
nrow(fl_no_pbc)


ggplot(data = fl_no_pbc, aes(x = buchanan)) +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Number of Votes for Buchanan") +
  ylab("Probability density")


ggplot(data = fl_no_pbc, aes(x = reg_reform)) +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Number of Registered Reform Party Members") +
  ylab("Probability density")


ggplot(data = fl_no_pbc, aes(x = reg_reform, y = buchanan)) +
  #geom_text(aes(label = technology), size = 4) +
  geom_point(size = 5) +
  theme_bw() +
  xlab("Number of Registered Reform Party Members") +
  ylab("Number of Votes for Buchanan")


lm.1 = lm(buchanan ~ 1 + reg_reform, data = fl_no_pbc)

glance(lm.1)
tidy(lm.1)

ggplot(data = fl, aes(x = reg_reform, y = buchanan)) +
  geom_text(aes(label = county), size = 4) +
  #geom_point(size = 5) +
  theme_bw() +
  xlab("Number of Registered Reform Party Members") +
  ylab("Number of Votes for Buchanan") +
  geom_abline(intercept = 50.8531, slope = 2.47098)



# Palm Beach County had 337 registered Reform Party members
50.8531 + 2.47098 * 337


(3411 - 883.5734) / 82.3561



# 2.20028 + 3.78967 * 337

(3411 - 1279.319) / 301.021 


data.frame(
  bp = c(103, 109, 107, 110, 111, 106, 112, 100, 
         98, 108, 100, 104, 103, 106, 102, 105),
  cond = c(rep("talk", 8), rep("rest", 8))
) %>%
  lm(bp ~ 1 + cond, data = .) %>%
  tidy()




