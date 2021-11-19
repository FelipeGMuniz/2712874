p <- 0.45
N <- 1000

X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))    # generate N observations

# calculate X_hat
X_hat <- mean(X)    

# calculate SE_hat, SE of the mean of N observations
SE_hat <- sqrt(X_hat*(1-X_hat)/N)    

# build interval of 2*SE above and below mean
lower_ci <- X_hat - 2*SE_hat
upper_ci <- X_hat + 2*SE_hat

c(lower_ci, upper_ci)

z <- qnorm(0.995)    # calculate z to solve for 99% confidence interval
pnorm(qnorm(0.995))    # demonstrating that qnorm gives the z value for a given probability
pnorm(qnorm(1-0.995))    # demonstrating symmetry of 1-qnorm
pnorm(z) - pnorm(-z)    # demonstrating that this z value gives correct probability for interval


B <- 10000
inside <- replicate(B, {
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  between(p, X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # TRUE if p in confidence interval
})
mean(inside)