

github_set_token("f9955aaa5627bca1df3df84ba09fb5fa7374c6c8")
github_get_token()



##################################################
### Scrape github repos
##################################################

commits_riders  <- ghclass::repo_commits("DSC-WAV/proj-valleyriders")
commits_rides   <- ghclass::repo_commits("DSC-WAV/proj-valleyrides")
commits_data    <- ghclass::repo_commits("Amherst-Statistics/valleybikeData")
commits_vb      <- ghclass::repo_commits("Amherst-Statistics/valleybike")
commits_proj_vb <- ghclass::repo_commits("DSC-WAV/proj-valleybike")


all_commits = rbind(commits_riders, commits_rides, commits_data, commits_vb, commits_proj_vb)




##################################################
### Clean names
##################################################

clean_names = data.frame(
  name = unique(all_commits$name),
  name2 = c("Jessica", "Konstantin", "Konstantin", "Nick", "Chris", "Nick",
            "Chris", "Umaimah", "Graham", "Kenny", "Maggie", "Kenny",
            "Kitty", "Kitty", "Graham", "Margaret", "Phebe", "Phebe") 
)

year_vb = data.frame(
  name2 = unique(clean_names$name2),
  cohort = c("Both", "Cohort 2", "Faculty", "Cohort 2", "Cohort 2", 
            "Cohort 2", "Both", "Cohort 2", "Both", 
            "Cohort 1", "Cohort 1")
  
)

 



final_data = all_commits %>%
  left_join(clean_names, by = "name") %>%
  left_join(year_vb, by = "name2") %>%
  mutate(
    date = as.Date(date, format = "%b %d, %Y"),
    name2 = factor(name2, levels = c("Margaret", "Phebe", "Chris", "Graham", "Konstantin", "Maggie", 
                                     "Umaimah", "Jessica", "Kenny", "Kitty", "Nick"))
    )


# commit_counts = all_commits %>%
#   group_by(date, login) %>%
#   summarize(N = n())


# Cohort 2 Commits
p1 = final_data %>%
  filter(date > "2020-08-23") %>%
  ggplot(data = , aes(x = date, fill = cohort)) +
  geom_dotplot(pch = 21, color = "black", method = "histodot", dotsize = 0.55) + 
  geom_vline(xintercept = as.Date("2020-09-18", "2020-10-16")) +
  #scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%b %d\n(%a)") +
  scale_x_date(
    name = "",
    breaks = as.Date(c("2020-08-24", "2020-09-18", "2020-10-16", "2020-11-06", "2020-11-20")), #"1 week", 
    labels = c("Aug\n24", "Sept\n18", "Oct\n16", "Nov\n06", "Nov\n20"),
    limits = as.Date(c("2020-08-24", "2020-11-20"))
    #date_minor_breaks = "1 week"
  ) +
  scale_y_continuous(name = " ", breaks = NULL) +
  #ggsci::scale_fill_ucscgb(name = "") +
  ggsci::scale_fill_d3(name = "Cohort") +
  theme_bw() +
  facet_grid(name2 ~ repo) +
  #guides(fill = FALSE) +
  theme(
    #axis.text.x = element_text(angle = 60, hjust = 1),
    panel.grid.minor.x = element_blank()
  )


ggsave(p1, filename = "~/Desktop/valley-bikes.png", width = 20, height = 13)



# Cohort 1 Commits
p1 = final_data %>%
  filter(date < "2020-08-23") %>%
  ggplot(data = , aes(x = date, fill = cohort)) +
  geom_dotplot(pch = 21, color = "black", method = "histodot", dotsize = 0.55) + 
  geom_vline(xintercept = as.Date("2020-09-18", "2020-10-16")) +
  #scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%b %d\n(%a)") +
  scale_x_date(
    name = "",
    breaks = as.Date(c("2020-02-24", "2020-03-25", "2020-04-08", "2020-05-13")), #"1 week", 
    labels = c("Feb\n24", "Mar\n03", "Apr\n08", "May\n13"),
    limits = as.Date(c("2020-01-27", "2020-06-01"))
    #date_minor_breaks = "1 week"
  ) +
  scale_y_continuous(name = " ", breaks = NULL) +
  #ggsci::scale_fill_ucscgb(name = "") +
  ggsci::scale_fill_d3(name = "Cohort") +
  theme_bw() +
  facet_grid(name2 ~ repo) +
  #guides(fill = FALSE) +
  theme(
    #axis.text.x = element_text(angle = 60, hjust = 1),
    panel.grid.minor.x = element_blank()
  )


ggsave(p1, filename = "~/Desktop/valley-bikes.png", width = 20, height = 13)



###################

final_data %>%
  #filter() %>%
  ggplot(aes(x = date, fill = cohort)) +
  geom_dotplot(pch = 21, color = "black", method = "histodot", dotsize = 0.55) + 
  geom_vline(xintercept = as.Date("2020-09-18", "2020-10-16")) +
  scale_x_date(
    name = "",
    breaks = as.Date(c("2020-02-24", "2020-03-25", "2020-04-08", "2020-05-13", "2020-08-24", "2020-09-18", "2020-10-16", "2020-11-06", "2020-11-20")),  
    labels = c("Feb\n24", "Mar\n03", "Apr\n08", "May\n13", "Aug\n24", "Sept\n18", "Oct\n16", "Nov\n06", "Nov\n20"),
    limits = as.Date(c("2020-01-27", "2020-11-20"))
  ) +
  scale_y_continuous(name = " ", breaks = NULL) +
  ggsci::scale_fill_d3(name = "Cohort") +
  theme_bw() +
  facet_grid(name2 ~ repo) +
  theme(panel.grid.minor.x = element_blank())


############### Do not split by repo

p2 = final_data %>%
  ggplot(aes(x = date, fill = cohort)) +
  geom_dotplot(pch = 21, color = "black", method = "histodot", dotsize = 0.4) + 
  geom_vline(xintercept = as.Date("2020-09-18", "2020-10-16")) +
  scale_x_date(
    name = "",
    breaks = as.Date(c("2020-02-24", "2020-03-25", "2020-04-08", "2020-05-13", "2020-08-24", "2020-09-18", "2020-10-16", "2020-11-06", "2020-11-20")),  
    labels = c("Feb\n24", "Mar\n03", "Apr\n08", "May\n13", "Aug\n24", "Sept\n18", "Oct\n16", "Nov\n06", "Nov\n20"),
    limits = as.Date(c("2020-01-27", "2020-11-20"))
  ) +
  scale_y_continuous(name = " ", breaks = NULL) +
  ggsci::scale_fill_d3(name = "Cohort") +
  theme_bw() +
  facet_wrap(~name2) +
  theme(panel.grid.minor.x = element_blank()) +
  guides(fill = FALSE)

ggsave(p2, filename = "~/Desktop/all-commits.png", width = 22, height = 9)


#### Summaries
final_data %>%
  mutate(cohort_work = if_else(date < "2020-06-01", "Cohort 1", "Cohort 2")) %>%
  group_by(name2, cohort_work) %>%
  summarize(N = n())


x = final_data %>%
  mutate(
    cohort_work = if_else(date < "2020-06-01", "Cohort 1", "Cohort 2"),
    day = factor(weekdays(date), level = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    ) %>%
  group_by(name2, cohort_work, day) %>%
  summarize(N = n())


x = final_data %>%
  group_by(name2, date) %>%
  summarize(N = n())

library(openair)


calendarPlot(mydata = x[x$name2 == "Margaret", ],   pollutant = "N", year = 2020, month = c(1,2,12), limits = c(0, 20), key = NULL, main = "Margaret")
calendarPlot(mydata = x[x$name2 == "Phebe", ],      pollutant = "N", year = 2020, month = 1:12, limits = c(0, 20), key = NULL, main = "Phebe")
calendarPlot(mydata = x[x$name2 == "Chris", ],      pollutant = "N", year = 2020, month = 1:12, limits = c(0, 20), key = NULL, main = "Chris")
calendarPlot(mydata = x[x$name2 == "Graham", ],     pollutant = "N", year = 2020, month = 1:12, limits = c(0, 20), key = NULL, main = "Graham")
calendarPlot(mydata = x[x$name2 == "Konstantin", ], pollutant = "N", year = 2020, month = 1:12, limits = c(0, 20), key = NULL, main = "Konstantin")
calendarPlot(mydata = x[x$name2 == "Maggie", ],     pollutant = "N", year = 2020, month = 1:12, limits = c(0, 20), key = NULL, main = "Maggie")
calendarPlot(mydata = x[x$name2 == "Umaimah", ],    pollutant = "N", year = 2020, month = 1:12, limits = c(0, 20), key = NULL, main = "Umaimah")
calendarPlot(mydata = x[x$name2 == "Jessica", ],    pollutant = "N", year = 2020, month = 1:12, limits = c(0, 20), key = NULL, main = "Jessica")
calendarPlot(mydata = x[x$name2 == "Kenny", ],      pollutant = "N", year = 2020, month = 1:12, limits = c(0, 20), key = NULL, main = "Kenny")
calendarPlot(mydata = x[x$name2 == "Kitty", ],      pollutant = "N", year = 2020, month = 1:12, limits = c(0, 20), key = NULL, main = "Kitty")
calendarPlot(mydata = x[x$name2 == "Nick", ],       pollutant = "N", year = 2020, month = 1:12, limits = c(0, 20), key = NULL, main = "Nick")


library(calendR)

x = final_data %>%
  group_by(name2, date) %>%
  summarize(N = n()) %>%
  mutate(day = strftime(date, format = "%j")) %>%
  filter(name2 == "Nick")

# Create vector from 1:366 (days in 2020); fill in with number of commits
# y = rep(0, 366)
# y[as.numeric(x$day)] = x$N

#strftime(c("2020-02-01", "2020-05-31"), format = "%j")
#strftime(c("2020-09-01", "2020-12-31"), format = "%j")

y = rep(0, 366)
y[as.numeric(x$day)] = 1

#z = y[32:152]
z = y[245:366]

calendR(
  year = 2020,
  special.days = z,
  # start_date = "2020-09-01",
  # end_date = "2020-12-31",
  start_date = "2020-02-01",
  end_date = "2020-05-31",
  gradient = TRUE,
  special.col = rgb(0.8941176, 1.0000000, 0.1019608, alpha = 1),
  #special.col = rgb(0.05098039, 0.02352941, 0.18823529, alpha = 0.6),
  low.col = "white",
  #orientation = "portrait",
  title = "Nick"
)


unique(final_data$name2)
