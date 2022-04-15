
#######################
# Assignment One
# ALM for pension fund
#######################


#################
# Load Package for Vector AutoRegression
#################

require(vars)

########################
# Import Data
########################

# Set working directory
# in my case

setwd("~/ALMCases/Tutorial/")

# Save beforehand Excel sheet or other data in CSV format with separator the semi-colon ";"
# in this case the decimal separator is the comma ","

shiller.data.full <- read.table("Shiller_data.csv", sep=";", skip=7, dec=",", header=T, nrows=1749)
str(shiller.data.full)

shiller.data <- shiller.data.full[,1:10]
colnames(shiller.data) <- cbind("Year.Month", "SandP", "dividend", "earnings", "CPI", "date", "GS10", "real.price", "real.dividend", "real.earnings")
attach(shiller.data)
str(shiller.data)
plot(shiller.data)

gs1 <- read.table("GS1.txt", skip=14, header=T)
str(gs1)

#gs2 <- read.table("GS2.txt", skip=14, header=T) # other starting date
gs3 <- read.table("GS3.txt", skip=14, header=T)
gs5 <- read.table("GS5.txt", skip=14, header=T)
#gs7 <- read.table("GS7.txt", skip=14, header=T)
gs10 <- read.table("GS10.txt", skip=14, header=T)
#gs20 <- read.table("GS20.txt", skip=18, header=T, na.strings=".") # incomplete
#gs30 <- read.table("GS30.txt", skip=18, header=T, na.strings=".") # incomplete

#######################
# explore eg. S&P index
######################

plot(date, SandP, type="l")
plot(date, log(SandP), type="l")
SandP.log <- log(SandP)
summary(SandP.log)
SandP.log.lm <- lm(SandP.log ~ date)
summary(SandP.log.lm)
abline(SandP.log.lm)
mu.SandP <- SandP.log.lm$coefficients[2]
mu.SandP
d.SandP.log <- SandP.log[-1] - SandP.log[-length(date)]
12 * mean(d.SandP.log) # arithmetic mean
sigma.SandP <- sd(d.SandP.log) / sqrt(1/12)
sigma.SandP

# There might be other models for growth: higher logarithms
plot(date, log(SandP.log), type="l")
SandP.log2.lm <- lm(log(SandP.log) ~ date)
summary(SandP.log2.lm)
abline(SandP.log2.lm)

# Or restrict dataset (post WW II)
(shift.ww2 <- 12 * (1950 - 1870))
SandP.2.log <- log(SandP[shift.ww2:length(date)])
date.2 <- date[shift.ww2:length(date)]
plot(date.2, SandP.2.log, type="l")
SandP.2.log.lm <- lm(SandP.2.log ~ date.2)
summary(SandP.2.log.lm)
abline(SandP.2.log.lm)
mu.SandP.2 <- SandP.2.log.lm$coefficients[2]
mu.SandP.2
d.SandP.2.log <- SandP.2.log[-1] - SandP.2.log[-length(date.2)]
sigma.SandP.2 <- sd(d.SandP.2.log) / sqrt(1/12)
sigma.SandP.2


# We choose to start the dataset at 1954 because this robust fit and because the bond prices are only available from 1954 on

shiller.data.full <- read.table("Shiller_data.csv", sep=";", skip=1004, dec=",", header=T, nrows=752)
str(shiller.data.full)
select.columns <- c(2, 3, 5, 6, 7)
shiller.data <- shiller.data.full[,select.columns]
colnames(shiller.data) <- cbind("SandP", "dividend", "CPI", "date", "GS10")
shiller.data[,5] <- shiller.data[,5]/100
attach(shiller.data) # may refer to columns directly
str(shiller.data)
length(date)
plot(shiller.data)

# again S & P
# Note that S&P 500 index includes already dividends.

plot(date, SandP, type="l")
plot(date, log(SandP), type="l")
SandP.log <- log(SandP)
summary(SandP.log)
SandP.log.lm <- lm(SandP.log ~ date)
summary(SandP.log.lm)
abline(SandP.log.lm)
mu.SandP <- SandP.log.lm$coefficients[2]
mu.SandP
d.SandP.log <- diff(SandP.log)
sigma.SandP <- sd(d.SandP.log) / sqrt(1/12)
sigma.SandP

# dividend

plot(date, log(dividend), type="l")
dividend.log <- log(dividend)
summary(dividend.log)
dividend.log.lm <- lm(dividend.log ~ date)
summary(dividend.log.lm)
abline(dividend.log.lm)
d.dividend.log <- diff(dividend.log)
(sigma.div <- sd(d.dividend.log) / sqrt(1/12))

# inflation

plot(date, log(CPI), type="l")
CPI.log <- log(CPI)
CPI.log.lm <- lm(CPI.log ~ date)
summary(CPI.log.lm)
abline(CPI.log.lm)
d.CPI.log <- diff(CPI.log)
plot(date[-1], d.CPI.log, type="l")
# moving average to go over to yearly inflation
annual.inflation <- (cumsum(d.CPI.log[13:752]) - cumsum(d.CPI.log[1:740]))/12
plot(date[-(1:12)], annual.inflation, type="l")
summary(annual.inflation)

# interest rate 10 yr
par(mfrow=c(1,1))
plot(date, GS10, type="l")
summary(GS10)
GS10.lm <- lm(GS10 ~ date)
summary(GS10.lm)
abline(GS10.lm)
d.GS10 <- diff(GS10)
# Change in value of bond is the sum of the interest over one month and the impact of the change in the rate (multiply by duration)
d.B10.log <- GS10[-length(GS10)] * 1/12 - d.GS10 * 10
plot(date[-1], d.B10.log, type="l")



#################
# call to VAR
################

# select data we expect to be stationary
var.data <- matrix(c(d.SandP.log, d.dividend.log, d.CPI.log, d.B10.log), ncol=4)
colnames(var.data) <- c("SandPyield", "dividendRate", "inflationRate", "Bond10yr")
str(var.data)
alm.var <- VAR(var.data, p=1, type="const") # note that type=both leads to a downward slope of GS10
summary(alm.var)
# Note that indeed this is stationary, although there is a root rather near the unit circle
roots(alm.var)


# break down

sigma.hat <- summary(alm.var)$covres
sigma.hat
coef.hat <- coef(alm.var)
coef.hat
l.endo <- 4

omega.hat <- matrix(c(coef.hat$SandPyield[1:l.endo,1], coef.hat$dividendRate[1:l.endo,1], 
                      coef.hat$inflationRate[1:l.endo,1], coef.hat$Bond10yr[1:l.endo,1]), ncol=l.endo, byrow=T)
omega.hat
alpha.hat <- c(coef.hat$SandPyield[l.endo+1,1], coef.hat$dividendRate[l.endo+1,1], 
               coef.hat$inflationRate[l.endo+1,1], coef.hat$Bond10yr[l.endo+1,1])
alpha.hat
mu.hat <- solve(diag(nrow=l.endo) - omega.hat) %*% alpha.hat
mu.hat
# compare to mean of dataset
skp <- 1
mu.var <- c(mean(d.SandP.log[skp:751]), mean(d.dividend.log[skp:751]), mean(d.CPI.log[skp:751]), mean(d.B10.log[skp:751]))
mu.var
mu.hat - mu.var

##############
# prediction
##############

pred.var <- predict(alm.var, n.ahead = 240)
plot(pred.var)
plot(pred.var, name = "SandPyield")
# fancier is the following plot
fanchart(pred.var, colors=rainbow(10))
fanchart(pred.var, colors=rainbow(10), name="Bond10yr")

########################
# perform a simulation
#######################

nextforecast <- function(omega, y, mu) {omega %*% (y - mu) + mu} 

y <- array(0, dim=c(240, l.endo)) # create data structure
l.var <- length(d.SandP.log)
# set starting value
y[1,] <- c(d.SandP.log[l.var], d.dividend.log[l.var], d.CPI.log[l.var], GS10[l.var]) 

# Do a run

epsilon <- mvrnorm(240, rep(0, l.endo), sigma.hat)
for (i in 2:240) { # do simulation
  y[i,] <- nextforecast(omega.hat, y[i-1,], mu.hat) + epsilon[i,]
}

# and inspect the generated data
par.old <- par(mfrow-c(1,1))
par.old <- par(mfrow=c(2, 2)) # set graphics paramaters
plot(y[,1], type="l", main="S & P yield")
plot(y[,2], type="l", main="Dividend Rate")
plot(y[,3], type="l", main="Inflation Rate")
plot(y[,4], type="l", main="10 year Bond return")
par(par.old) # reset graphics parameters


#########
# Method 2
#########

mu.tilde <- mu.var
V.tilde <- var(var.data)
W.tilde <- cov(var.data[-1,], var.data[-l.var,])
Omega.tilde <- W.tilde %*% solve(V.tilde)
Theta.tilde <- V.tilde - W.tilde %*% solve(V.tilde) %*% t(W.tilde)


y <- array(0, dim=c(240, l.endo)) # create data structure
y[1,] <- c(d.SandP.log[l.var], d.dividend.log[l.var], d.CPI.log[l.var], d.B10.log[l.var])

# Do a run

epsilon <- mvrnorm(240, rep(0, l.endo), Theta.tilde)
for (i in 2:240) { # do simulation
  y[i,] <- nextforecast(Omega.tilde, y[i-1,], mu.tilde) + epsilon[i,]
}

par(mfrow=c(2, 2)) # set graphics paramaters
plot(y[,1], type="l", main="S & P yield")
plot(y[,2], type="l", main="Dividend Rate")
plot(y[,3], type="l", main="Inflation Rate")
plot(y[,4], type="l", main="10 year Bond return")
par(par.old) # reset graphics parameters

# compare methods
Omega.tilde - omega.hat
Theta.tilde %*% solve(sigma.hat)

##############################################################
# Version with more bond data and restriction of historic data
###############################################################

# skp <- 400
skp <- 1
all.data <- shiller.data[skp:752,]

all.data$GS1 <- gs1$VALUE[(8+skp):760]/100
all.data$GS5 <- gs5$VALUE[(8+skp):760]/100
str(all.data)

d.GS1 <- diff(all.data$GS1)
# Change in value of bond is the sum of the interest over one month and the impact of the change in the rate (multiply by duration)
d.B1.log <- all.data$GS1[-length(all.data$GS1)] * 1/12 - d.GS1 * 1
plot(date[-1], d.B1.log, type="l")

d.GS5 <- diff(all.data$GS5)
# Change in value of bond is the sum of the interest over one month and the impact of the change in the rate (multiply by duration)
d.B5.log <- all.data$GS5[-length(all.data$GS5)] * 1/12 - d.GS5 * 5
plot(date[-1], d.B5.log, type="l")

var2.data <- all.data[-1,]
colnames(var2.data) <- c("SandPyield", "dividendRate", "inflationRate", "date", "Bond10yr", "Bond1yr", "Bond5yr") 
mu.var <- c(mean(d.SandP.log[skp:751]), mean(d.dividend.log[skp:751]), mean(d.CPI.log[skp:751]), mean(d.B10.log), mean(d.B1.log), mean(d.B5.log))
mu.var

var2.data$SandPyield <- d.SandP.log[skp:751] 
var2.data$dividendRate <- d.dividend.log[skp:751] 
var2.data$inflationRate <- d.CPI.log[skp:751]
var2.data$Bond10yr <- d.B10.log[skp:751]
var2.data$Bond1yr <- d.B1.log[skp:751]
var2.data$Bond5yr <- d.B5.log[skp:751]

# This is the good stationary data set

alm2.var <- VAR(var2.data[,-4], p=1, type="const") 
summary(alm2.var)
# Note that indeed this is stationary, although there are roots very near the unit circle
roots(alm2.var)

# break down

sigma2.var <- summary(alm2.var)$covres
sigma2.var
coef2.var <- coef(alm2.var)
coef2.var
coef2.var$SandPyield[,1]
l.endo <- 6

omega2.var <- matrix(c(coef2.var$SandPyield[1:l.endo,1], coef2.var$dividendRate[1:l.endo,1], 
                      coef2.var$inflationRate[1:l.endo,1], coef2.var$Bond10yr[1:l.endo,1],
                      coef2.var$Bond1yr[1:l.endo,1], coef2.var$Bond5yr[1:l.endo,1]), ncol=l.endo, byrow=T)
omega2.var
alpha2.var <- c(coef2.var$SandPyield[l.endo+1,1], coef2.var$dividendRate[l.endo+1,1], 
               coef2.var$inflationRate[l.endo+1,1], coef2.var$Bond10yr[l.endo+1,1],
               coef2.var$Bond1yr[l.endo+1,1], coef2.var$Bond3yr[l.endo+1,1], coef2.var$Bond5yr[l.endo+1,1])
alpha2.var
mu.infinity <- solve(diag(nrow=l.endo) - omega2.var) %*% alpha2.var
mu.infinity
mu.var - mu.infinity

##############
# prediction
##############

pred2.var <- predict(alm2.var, n.ahead = 240)
plot(pred2.var)
fanchart(pred2.var, colors=rainbow(10))


########################
# perform a simulation
#######################

# nextforecast <- function(omega, y, mu) {omega %*% (y - mu) + mu} 

y <- array(0, dim=c(240, l.endo)) # create data structure
colnames(y) <- c("S and P yield", "Dividend Rate", "Inflation Rate", "Bond return 10 yr", "Bond return 1 yr", "Bond return 5 yr")
l.var <- length(var2.data$SandPyield)
y[1,] <- c(d.SandP.log[l.var], d.dividend.log[l.var], d.CPI.log[l.var], d.B10.log[l.var], d.B1.log[l.var], d.B5.log[l.var]) # set initial vector

epsilon <- mvrnorm(240, rep(0, l.endo), sigma2.var)
for (i in 2:240) { # do simulation
  y[i,] <- nextforecast(omega2.var, y[i-1,], mu.var) + epsilon[i,]
}

simul.1 <- data.frame(y)

par(mfrow=c(3, 2)) # set graphics paramaters
plot(y[,1], type="l", main="S & P yield")
plot(y[,2], type="l", main="Dividend Rate")
plot(y[,3], type="l", main="Inflation Rate")
plot(y[,4], type="l", main="10 year Rate")
plot(y[,5], type="l", main="1 year Rate")
plot(y[,6], type="l", main="5 year Rate")
par(par.old) # reset graphics parameters

##############################
# Pension Fund Liabilities
##############################

# Modeled as a 10 yr bond, but with indexation according to inflation rate.
# Stationary, so the bond remains a good model

period.1 <- 1:240 / 12

# Change in value of the liabilities is the change in the value of the bond plus indexation
# So here liabilities are valued nominally, without any future indexation

indexation.factor <- 0.5
d.liabs.log <- simul.1$Bond.return.10.yr + indexation.factor * simul.1$Inflation.Rate
plot(period.1, d.liabs.log, type="l")

##################
# Asset mix
##################

# Invest in stock and bonds
#
alpha.1 <- 0.60 # percentage in stocks
liabs.0 <- 100
assets.0 <- 150

# rebalance every period
# so
d.assets.log <- alpha.1 * simul.1$S.and.P.yield + (1-alpha.1) * simul.1$Bond.return.10.yr
plot(period.1, d.assets.log, type="l")

assets.log <- log(assets.0) + cumsum(d.assets.log)
str(assets.log)
assets.full <- exp(assets.log)
plot(assets.full)
plot(assets.log)

liabs.log <- log(liabs.0) + cumsum(d.liabs.log)
liabs.full <- exp(liabs.log)
plot(liabs.log, type="l")
points(assets.log)
