
# Exercises 15.3

# Created a function to return the standard error (SE) depending on the sample 
# size n on a range of p values ranging from 0 to 1

s_err <- function(n){
  
  p <- seq(0, 1, length = 100)
  print(sqrt(p*(1-p)/n))
}

# N is the sample size
N <- 25

# Variable p is the estimated parameter ranging from 0 to 1
p <- seq(0, 1, length.out = 100)

# Variable se is the standard error
se <- sqrt(p*(1-p)/N)

# Plot se (y-axis) vs p (x-axis)
plot(p, se)

# 6. Copy the code above and put it inside a for-loop to make the plot for  
# N = 25, 100, 1000

for(N in c(25, 100, 1000)){
  p <- seq(0, 1, length.out = 100)
  se <- sqrt(p*(1-p)/N)
  plot(p, se)
}

# `N` represents the number of people polled
N <- 25

# `p` represents the proportion of Democratic voters
p <- 0.45

# Calculate the standard error of the spread. Print this value to the console.
2*sqrt(abs(p*(1-p)/N))

# QQ-plot
qqline(x_hat, col = "red", lwd = 2)
