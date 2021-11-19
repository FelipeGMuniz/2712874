p <- 0.48
N <- 2500

se <- sqrt(p*(1-p)/N)
se

pnorm(.01/se)-pnorm(-.01/se)