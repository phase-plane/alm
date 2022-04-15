
#######################
# Assignment Three
# Valuation of options in risk neutral world
#######################

########################
# Black Scholes formula
########################

S.0 <- 100
mu <- 0.07
sigma <- 0.25
K <- 150
T1 <- 6
t.0 <- 0
r <- 0.02


# expected stock value

log.S.0 <- log(S.0)
log.S.T1 <- log.S.0 + (mu - sigma^2/2) * (T1 - t.0)
log.S.T1
(S.T1 <- exp(log.S.T1))

# Black Scholes value of call option with strike K

d1 <- ( log(S.0/K) + (r + sigma^2/2) * (T1 - t.0)) / (sigma * sqrt(T1 - t.0))
d2 <- d1 - sigma * sqrt(T1 - t.0)

V.call.0 <- S.0 * pnorm(d1) - exp(-r*(T1 - t.0)) * K * pnorm(d2)
V.call.0

#############################
# Black-Scholes as a function
#############################

call.BS <- function(stock, strike, mu, sigma, r, expir, t) { # for european call
  d1 <- ( log(stock/strike) + (r + sigma^2/2) * (expir - t)) / (sigma * sqrt(expir - t))
  d2 <- d1 - sigma * sqrt(expir - t)
  V.call <- stock * pnorm(d1) - exp(-r*(expir - t)) * strike * pnorm(d2)
  V.call
}

call.BS(S.0, K, mu, sigma, r, T1, t.0)

#########################
# Calibration
########################

# Given is the following set of call option values for a series of strikes


strikes <- (1:10)*25
values <- c(78.9, 58.3, 41.3, 28.8, 20.3, 14.4, 10.8, 9.4, 8.4, 8.0)
plot(strikes, values)

# The calibration is done using the Black Scholes function
# We minimize the sum of squares over the parameter sigma.

calib <- function(sigma) sum(values - call.BS(S.0, strikes, mu, sigma, r, T1, t.0))^2
sigma.0 <- 0.2 # starting value

sigma.calib <- optim(sigma.0, calib, method="BFGS")

# sigma found by calibration
sigma.calib$par


#######################
# Monte Carlo Risk Neutral World
# single simulation
######################

d.t <- 0.01
M <- (T1 - t.0)/d.t
log.S <- array(0, dim=c(M)) # set up datastructure
log.S[1] <- log.S.0

for (i in (1:(M-1))) {
  d.W <- rnorm(1)
  d.log.S <- (r - sigma^2/2)*d.t + sigma * sqrt(d.t) * d.W
  log.S[i+1] <- log.S[i] + d.log.S
}

plot(log.S, type="l")


#######################
# Monte Carlo Risk Neutral World
# Set of simulations
######################

d.t <- 0.01
M <- (T1 - t.0)/d.t
N <- 10000

log.S <- array(0, dim=c(M, N)) # set up datastructure
log.S[1,] <- log(S.0) # starting value

# the same loop, but vectorwise

for (i in (1:(M-1))) {
    d.W <- rnorm(N)
    d.log.S <- (r - sigma^2/2)*d.t + sigma * sqrt(d.t) * d.W
    log.S[i+1,] <- log.S[i,] + d.log.S
}

# plot simulations

plot(ts(log.S[,1:100]), plot.type="single", col=rainbow(50))
S.sim <- exp(log.S)
plot(ts(S.sim[,1:100]), plot.type="single", col=rainbow(50))
points(1:M, rep(K, M), type="l")

# value at expiration
S.call <- pmax(S.sim[M,] - K, 0)
# the mean (in this risk neutral setting) is the value of the option

(V.call.S <- exp(-r * (T1 - t.0)) * mean(S.call))

# check with several runs the standard deviation of the result
# How much runs does one need?


#######################
# Monte Carlo Real World (for comparison)
# Set of simulations
######################

d.t <- 0.01
M <- (T1 - t.0)/d.t
N <- 150

log.S <- array(0, dim=c(M, N)) # set up datastructure
log.S[1,] <- log.S.0 # starting value

# the same loop, but vectorwise

for (i in (1:(M-1))) {
  d.W <- rnorm(N)
  d.log.S <- (mu - sigma^2/2)*d.t + sigma * sqrt(d.t) * d.W
  log.S[i+1,] <- log.S[i,] + d.log.S
}


# plot simulations

plot(ts(log.S[,1:100]), plot.type="single", col=rainbow(50))
S.sim <- exp(log.S)
plot(ts(S.sim[,1:100]), plot.type="single", col=rainbow(50))

points(1:M, rep(K, M), type="l")








