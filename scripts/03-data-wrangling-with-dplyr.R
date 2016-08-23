##################################################
### Read in data
##################################################

cehd = read.csv("~/Desktop/cehd.csv")

head(cehd)
tail(cehd)

summary(cehd)



##################################################
### Load dplyr library
##################################################

library(dplyr)



##################################################
### Select a subset of rows (filter)
##################################################

cehd %>% filter(department == "EPSY")


# Write output into an object
epsy = cehd %>% filter(department == "EPSY")
head(epsy)
mean(epsy$annual_pay)


assoc = cehd %>% filter(title == "Associate Professor")
mean(assoc$annual_pay)



##################################################
### Filter on multiple attributes
##################################################

epsy_assoc = cehd %>% filter(department == "EPSY", title == "Associate Professor")
epsy_assoc
mean(epsy_assoc$annual_pay)


# AND
cehd %>% filter(department == "EPSY" & title == "Associate Professor")


# OR
cehd %>% filter(department == "EPSY" | title == "Associate Professor")

# Associate professors from EPSY and C&I
cehd %>% 
  filter(title == "Associate Professor") %>%
  filter(department == "EPSY" | department == "C&I")




##################################################
### Select a subset of columns (select)
##################################################

cehd %>% select(name, department, annual_pay)

# Rename a column
cehd %>% select(name, dept = department, annual_pay)




##################################################
### Helper functions for select()
##################################################

cehd %>% select(ends_with("e"))



##################################################
### Create new variables (nutate)
##################################################

cehd %>% 
  select(name, department, annual_pay) %>%
  mutate(pay2 = annual_pay / 100000)


# Create multiple variables
cehd %>% 
  select(name, department, annual_pay) %>%
  mutate(pay2 = annual_pay / 100000,
         lastname = gsub(",.*$", "", as.character(name))
  )



##################################################
### Sort/reorder data (arrange)
##################################################

cehd %>% arrange(department)

# Sort by multiple variables
cehd %>% arrange(department, title, hire_year)

# Arrange in descending order
cehd %>% arrange(department, desc(hire_year))


##################################################
### Summarizing
##################################################

cehd %>% summarize(M = mean(annual_pay))

# Compute multiple summaries
cehd %>% 
  summarize(
    M = mean(annual_pay),
    SD = sd(annual_pay)
  )



##################################################
### Grouping
##################################################

cehd %>% 
  group_by(department) %>%
  summarize(
    M = mean(annual_pay),
    SD = sd(annual_pay)
  )



##################################################
### Plotting group output
##################################################

dept_summaries = cehd %>% 
  group_by(department) %>%
  summarize(
    M = mean(annual_pay),
    SD = sd(annual_pay),
    n = n()
  ) %>%
  mutate(
    lower_limit = M - 2 * SD / sqrt(n),
    upper_limit = M + 2 * SD / sqrt(n)
  )

dept_summaries


library(ggplot2)

ggplot(data = dept_summaries, aes(x = department, y = M)) +
  geom_segment(aes(x = department, xend = department, y = lower_limit, yend = upper_limit)) +
  geom_line(aes(group = 1), linetype = "dotted") +
  geom_point() +
  theme_bw() +
  xlab("") +
  ylab("Average Base Salary")





