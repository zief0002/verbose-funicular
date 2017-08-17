##################################################
### Load dplyr library
##################################################

library(dplyr)
library(ggplot2)
library(readr)



##################################################
### Read in data
##################################################

city = read_csv("~/Dropbox/epsy-8251/data/riverside.csv")

head(city)
tail(city)



##################################################
### Select a subset of rows (filter)
##################################################

city %>% filter(gender == "male")


# Write output into an object
males = city %>% filter(gender == "male")
head(males)
mean(males$income)


high_school = city %>% filter(education <= 12)
mean(high_school$income)



##################################################
### Filter on multiple attributes
##################################################

# AND Logic
males_high_school = city %>% filter(gender == "male", education <= 12)
males_high_school
mean(males_high_school$income)


# OR Logic
males_OR_high_school = city %>% filter(gender == "male" | education <= 12)
males_OR_high_school


##################################################
### Select a subset of columns (select)
##################################################

city2 = city %>% select(education, income, gender)
head(city2)


# Rename a column
city2 = city %>% select(Edu = education, income, gender)
head(city2)



##################################################
### Helper functions for select()
##################################################

city2 = city %>% select(ends_with("e"))
head(city2)



##################################################
### Create new variables (nutate)
##################################################

city3 = city %>% 
  mutate(income2 = income / 1000)

head(city3)


# Create multiple variables
city3 = city %>% 
  mutate(
    income2 = income / 1000,
    educ_after_8 = education - 8
    )

head(city3)



##################################################
### Sort/reorder data (arrange)
##################################################

city4 = city %>% arrange(income)
city4

# Sort by multiple variables
city4 = city %>% arrange(gender, income)
city4

# Arrange in descending order
city4 = city %>% arrange(gender, desc(income))
city4



##################################################
### Summarizing
##################################################

mySummaries = city %>% summarize(M = mean(income))
mySummaries


# Compute multiple summaries
mySummaries = city %>% 
  summarize(
    M = mean(income),
    SD = sd(income)
  )

mySummaries



##################################################
### Grouping
##################################################

mySummaries = city %>% 
  group_by(gender) %>%
  summarize(
    M = mean(income),
    SD = sd(income)
  )

mySummaries



##################################################
### Using dplyr and ggplot2 together
##################################################

# What if you  wanted to plot the relationship between income and education level for females?

females = city %>% filter(gender == "female")

ggplot(data = females, aes(x = education, y = income)) +
  geom_point() +
  theme_bw() +
  xlab("Education level") +
  ylab("Income")



# OPTION 2: Use piping to send the results directly to ggplot2.
# The data=. syas use the data you just piped in.

city %>% 
  filter(gender == "female") %>%
  ggplot(data = ., aes(x = education, y = income)) +
    geom_point() +
    theme_bw() +
    xlab("Education level") +
    ylab("Income")
