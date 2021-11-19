library(dplyr)
library(dslabs)
library(tidyverse)

data("polls_us_election_2016")


polls <- mutate(polls, X_hat = polls$rawpoll_clinton/100, 
                se_hat = sqrt(X_hat*(1-X_hat)/polls$samplesize), 
                lower = X_hat - qnorm(0.975)*se_hat, 
                upper = X_hat + qnorm(0.975)*se_hat)

pollster_results <- select(polls, pollster, enddate, X_hat, se_hat, lower, upper)

# Exercise 3. Comparing to actual results - p
# The final tally for the popular vote was Clinton 48.2% and Trump 46.1%. 
# Add a column called hit to pollster_results that states if the confidence 
# interval included the true proportion  or not. What proportion p = 0.482 of 
# confidence intervals included p?

# Instructions

# Finish the code to create a new object called avg_hit by following these steps.
# Use the mutate function to define a new variable called 'hit'.
# Use logical expressions to determine if each values in lower and upper span the actual proportion.
# Use the mean function to determine the average value in hit and summarize the results using summarize.

avg_hit <- pollster_results %>% 
  mutate(hit = (lower<=0.482 & upper>=0.482)) %>%
  summarise(mean(hit))
avg_hit


my_data <- as_tibble(pollster_results %>% mutate(hit = (lower<=0.482 & upper>=0.482)))


my_data %>% arrange(hit)
my_data

# Add a statement to this line of code that will add a new column named `d_hat` to `polls`. The new column should contain the difference in the proportion of voters.
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") %>%
  mutate(d_hat = ((rawpoll_clinton)-(polls$rawpoll_trump))/100)


# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- polls$samplesize[1]

# Assign the difference `d_hat` of the first poll in `polls` to a variable called `d_hat`. Print this value to the console.
d_hat <- (polls$rawpoll_clinton[1] - polls$rawpoll_trump[1])/100
d_hat
# Assign proportion of votes for Clinton to the variable `X_hat`.
X_hat <- (d_hat+1)/2

# Calculate the standard error of the spread and save it to a variable called `se_hat`. Print this value to the console.

se_hat <- 2*(sqrt(X_hat*(1-X_hat)/N))
se_hat

# Use `qnorm` to calculate the 95% confidence interval for the difference in the proportions of voters. Save the lower and then the upper confidence interval to a variable called `ci`.

Q <- qnorm(1- 0.05/2)
lower <- d_hat - Q*se_hat
upper <- d_hat + Q*se_hat

ci <- c(lower, upper)
ci

# Exercise 6. Pollster results for d
# Create a new object called pollster_results that contains the pollster's name, 
# the end date of the poll, the difference in the proportion of voters who 
# declared a vote either, and the lower and upper bounds of the confidence 
# interval for the estimate.Exercise 6. Pollster results for d

pollster_results <- polls %>% 
    mutate(
      X_hat = (d_hat+1)/2,
      se_hat = 2*(sqrt(X_hat*(1-X_hat)/samplesize)),
      lower = d_hat - qnorm(1- 0.05/2)*se_hat,
      upper = d_hat + qnorm(1- 0.05/2)*se_hat) %>%
    select(pollster, enddate, d_hat, lower, upper)


# The `polls` object has already been loaded. Examine it using the `head` function.
head(polls)

# Add variable called `error` to the object `polls` that contains the difference 
# between d_hat and the actual difference on election day. Then make a plot of the 
# error stratified by pollster.

polls %>% mutate(error = d_hat-0.021) %>% 
  group_by(pollster) %>%
  filter(n() >= 5) %>%
  ggplot(aes(x = pollster, y = error)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
