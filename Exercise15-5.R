# 1. Write an urn model function that takes the proportion of Democrats  
# p and the sample size N as arguments and returns the sample average if 
# Democrats are 1s and Republicans are 0s. Call the function take_sample.

take_sample <- function(p, N){
  test <- sample(c(1, 0), size = N, replace = T, prob = c(p, 1-p))

  mean(test)  
}
take_sample(0.45, 100)

# 2. Now assume p <- 0.45 and that your sample size is N = 100. Take a 
# sample 10,000 times and save the vector of mean(X) - p into an object called 
# errors. Hint: use the function you wrote for exercise 1 to write this in one 
# line of code.
p <- .45
N <- 100
B <- 10000

errors <- replicate(B, p - take_sample(p, N))

mean(errors)
hist(errors)

# 4. The error ¯X−p is a random variable. In practice, the error is not observed
# because we do not know p. Here we observe it because we constructed the 
# simulation. What is the average size of the error if we define the size by 
# taking the absolute value ∣¯X−p∣?
mean(abs(errors))

# Exercise 12. Estimating the probability of a specific value of X-bar
# If  and , use the central limit theorem to estimate the probability that.

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# Calculate the probability that the estimated proportion of Democrats in the population is greater than 0.5. Print this value to the console.
se <- sqrt(p*(1-p)/N)
1-pnorm(0.5, mean = p, sd = se)