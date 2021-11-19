library(dslabs)
data("polls_us_election_2016")

library(tidyverse)
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-31" & state == "U.S.")

N <- polls$samplesize[1]
N
x_hat <- polls$rawpoll_clinton[1]/100
x_hat

# Assume there are only two candidates and construct a 95% confidence interval 
# for the election night proportion p.

# Estimated Standard Error
se_hat <- sqrt(x_hat*(1-x_hat)/N)
se_hat

# Confidence Intervals at 95% confidence
Q <- qnorm(1- 0.05/2)
lower <- x_hat - Q*se
upper <- x_hat + Q*se

ci <- c(lower, upper)


poll <- polls %>% mutate(lower, upper)

