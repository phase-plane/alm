## Profit-sharing option via the U-rate
## Part I: Simulation

## test: Risk-Neutral Monte-Carlo for call options
BLSPrice <- function(S, M, cap.T, sigma, r){
  ## M = K/S
  d1 <- (log(1/M) + (r + 0.5*sigma^2)*cap.T) / (sigma*sqrt(cap.T))
  d2 <- d1 - sigma*sqrt(cap.T)
  C <- S*pnorm(d1) - S*M*exp(-r*cap.T)*pnorm(d2)
  return(C)
}

## parameters for simulation
set.seed(1)
nsims <- 1e5
s0 <- 100
K <- 100
M <- K/s0
cap.T <- 1
dt <- 1/50
mu <- 0.08
sigma <- 0.1
r <- 0.02
S <- s0 * rep(1, nsims)

for (i in 1:50){
  dW <- sqrt(dt) * rnorm(nsims)
  S <- S*(1 + r*dt + sigma*dW)
}

## comparison
vanilla.C <- BLSPrice(s0, M, cap.T, sigma, r)
monte.C <- exp(-r*cap.T) * mean(pmax(S-K,0))

