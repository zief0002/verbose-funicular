##################################################
### Load dplyr library
##################################################

library(dplyr)
library(readr)



##################################################
### Read in data
##################################################

city = read_csv("~/Documents/github/epsy-8251/data/riverview.csv")

head(city)
tail(city)



##################################################
### Understanding piping
##################################################

summary(city)
city %>% summary()


nrow(city)
city %>% nrow()


filter(city, gender == "female")
city %>% filter(gender == "female")



##################################################
### Select a subset of rows (filter)
##################################################

# Write output into an object
females = city %>% 
  filter(gender == "female")

# Compute number of rows
nrow(females)

# All in one computation
city %>% 
  filter(gender == "female") %>%
  nrow()


city %>% 
  filter(education < 12) %>%
  nrow()




##################################################
### Filter on multiple attributes
##################################################

city %>% 
  filter(gender == "female", education < 12) %>%
  nrow()


# AND Logic
city %>% 
  filter(gender == "female" & education < 12)


# OR Logic
city %>% 
  filter(gender == "female" | education < 12)



##################################################
### Select a subset of columns (select)
##################################################

city %>% 
  select(education, income, gender)


# Rename a column
city %>% 
  select(education, income, gender) %>%
  rename(educ = education)



##################################################
### Helper functions for select()
##################################################

city %>% 
  select(ends_with("e"))



##################################################
### Create new variables (nutate)
##################################################

city %>% 
  mutate(
    income2 = income * 1000
  )


# Create multiple variables
city %>% 
  mutate(
    income2 = income * 1000,
    cent_educ = education - mean(education)
  )



##################################################
### Sort/reorder data (arrange)
##################################################

city %>% 
  arrange(income)


# Sort by multiple variables
city %>% 
  arrange(gender, income)


# Arrange in descending order
city %>% 
  arrange(gender, desc(income))



##################################################
### Summarizing
##################################################

city %>% 
  summarize(
    M = mean(income)
  )


# Compute multiple summaries
city %>% 
  summarize(
    M  = mean(income),
    SD = sd(income)
  )



##################################################
### Grouping
##################################################

city %>% 
  group_by(gender) %>%
  summarize(
    M  = mean(income),
    SD = sd(income)
  )


