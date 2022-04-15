
#######################
# Assignment Two
# Hedging Cash Flows
#######################


#################
# Load Packages for Yield Curves and for matrix calculations
#################

require(YieldCurve)
require(matrixcalc)

########################
# Import Term Structures
########################

# Set working directory
# in my case

data("ECBYieldCurve")
str(ECBYieldCurve)
? xts # the data structure is an eXtensible Time Series (xts)
head(ECBYieldCurve)
# maturities of spot rates in dataset
(mat.ECB <- c(3/12,6/12,1:30))

plot(ts(t(ECBYieldCurve[450:655,-2]), start=0), plot.type="single", col=rainbow(800), ylab="spot rate", main="Yield Term Structures (ECB 2006 - 2009")

# first curve
rts1 <- ECBYieldCurve[500,]
plot(mat.ECB, rts1, type="l")

# Determine Nelson Siegel parameters for fitting
Nelson.Siegel(rts1, mat.ECB)

# Determine Svensson - Nelson Siegel parameters
svensson.coef <- Svensson(rts1, mat.ECB)
rates.extrapol <- Srates(svensson.coef, 1:100, "Spot")
rates.extrapol


# Principal Components Analysis
# Note we choose a subset of the time data, since the overall movement happened to be atypical.
ecb.pca <- prcomp(ECBYieldCurve[590:655,])
summary(ecb.pca)
plot(ecb.pca)
print(ecb.pca)
str(ecb.pca)

plot(c(1,32),c(0, 0), type="l",ylim=c(-0.5, 0.5), ylab="change in yield (bp)", xlab="maturity", main="Principal Components")
points(ecb.pca$rotation[,1], type="l", col="orange")
points(ecb.pca$rotation[,2], type="l", col="blue")
points(ecb.pca$rotation[,3], type="l", col="red")
points(ecb.pca$rotation[,4], type="l", col="green")
legend.txt <- c("1st component (parallel shift)", "2nd component (tilt)", "3rd component (hump)", "4th component (wave)")
legend("topright", legend.txt, col = c("orange", "blue", "red", "green"), lty=c(1,1,1))

######################
# Liability
######################

# Create a cashflow
# Closed book

t <- 1:90
n <- 15
liab <- list(time=t, cashflow=(t+n) * exp(-(t+n)/n))
plot(liab$time, liab$cashflow, type="l")

###################
# Present Value
###################

# Using Svensson extrapolation

pv <- function(rts, rts.mat, liab) {
  rts.inter <- Srates(Svensson(rts, rts.mat), liab$time, "Spot")
  disc <- exp(- rts.inter/100 * liab$time) # ECB curve is based on continuous compounding
  sum(disc * liab$cashflow)
}

(liab.pv <- pv(rts1, mat.ECB, liab))


##################
# Sensitivity, Money Duration, DV01
##################

sens <- function(rts, rts.mat, liab, shock=1) { # sensitivity for parallel movements in spot rates
  # shock in bps
  dr <- shock * 10^-4
  pv.up <- pv(rts + dr*100, rts.mat, liab)
  pv.down <- pv(rts - dr*100, rts.mat, liab)  
  (pv.up - pv.down) / (2 * dr)
}


sens(rts1, mat.ECB, liab, shock=10)

dur.money <- function(rts, rts.mat, liab, shock) { 
  - sens(rts, rts.mat, liab, shock)
} 

(liab.dur.money <- dur.money(rts1, mat.ECB, liab, shock=10))
  
dv01 <- function(rts, rts.mat, liab) { # shock is 1 bps by definition
  sens(rts, rts.mat, liab, shock=1) * 10^-4
}

dv01(rts1, mat.ECB, liab) # in euro's


###############
# Duration
##############

dur <- function(rts, rts.mat, liab, shock=1) { # sensitivity for parallel movements in spot rates
  # shock in bps
  pv.liab <- pv(rts, rts.mat, liab)
  - 1/pv.liab * sens(rts, rts.mat, liab, shock)
}

(liab.dur <- dur(rts1, mat.ECB, liab, shock=1))

###################
# Macaulay Duration
###################

dur.macaulay <- function(rts, rts.mat, liab) { 
  rts.inter <- Srates(Svensson(rts, rts.mat), liab$time, "Spot")
  disc <- exp(- rts.inter/100 * liab$time)
  sum(liab$time * liab$cashflow * disc) / pv(rts, rts.mat, liab)
} 

(liab.mac.dur <- dur.macaulay(rts1, mat.ECB, liab)) 

liab.mac.dur 
# compare with
liab.dur

################
# Convexity
###############

sens.2nd <- function(rts, rts.mat, liab, shock=1) {
  dr <- shock * 10^-4
  pv.up <- pv(rts + dr*100, rts.mat, liab)
  pv.down <- pv(rts - dr*100, rts.mat, liab)
  pv.0 <- pv(rts, rts.mat, liab)
  (pv.up + pv.down - 2 * pv.0) / dr^2
}

sens.2nd(rts1, mat.ECB, liab, shock=10)

conv <- function(rts, rts.mat, liab, shock=1) { # sensitivity for parallel movements in spot rates
  # shock in bps
  pv.liab <- pv(rts, rts.mat, liab)
  1/pv.liab * sens.2nd(rts, rts.mat, liab, shock)
}

(liab.conv <- conv(rts1, mat.ECB, liab, shock=1))


##############################
# Hedging: Volume and Duration
##############################

# Take hedge: single bond with matching duration and value

# adjust hedge per month
date.shift <- rep(1, 30) + c(0,0,0,0,0,2,0,0,1,0,0,1,0,2,1,0,0,0,0,2,0,0,1,0,0,0,0,0,2,0) # to correct for non-trading days

months1 <- seq(as.Date("2007-01-01"), length.out=30, by="month") + date.shift
months1
ECBYieldCurve[months1][2]
(ECBYieldCurve.months <- ECBYieldCurve[months1])

##########
# 1st hedge
##########

(bond.mat <- dur(rts1, mat.ECB, liab))
zcb1 <- list(time=bond.mat, cashflow=1)
disc1 <- pv(rts1, mat.ECB, zcb1)
(bond.princ <- pv(rts1, mat.ECB, liab) / disc1)
hedge1 <- list(time=bond.mat, cashflow=bond.princ)

# compare
dur(rts1, mat.ECB, hedge1)
dur(rts1, mat.ECB, liab)

# compare
pv(rts1, mat.ECB, hedge1)
pv(rts1, mat.ECB, liab)

############
# try single time step
############

step1 <- function(liab, step) {list(time=liab$time - step, cashflow=liab$cashflow)}
step2 <- function(liab, step) {
  time1 <- liab$time[liab$time >= step]
  cashflow1 <- liab$cashflow[liab$time >= step]
  list(time=time1, cashflow=cashflow1)
  }
 
p.and.l.1 <- pv(ECBYieldCurve.months[3,], mat.ECB, step1(hedge1, 1/12)) - pv(ECBYieldCurve.months[3,], mat.ECB, step1(liab, 1/12))
p.and.l.1
# split off cashflow for future months
step2(step1(liab, 1/12), 1/12)

###############
# iterate time steps
###############

(le <- length(months1))
hedge.p.and.l <- rep(0, le)
month <- 1/12

liab.tmp <- liab
hedge.tmp <- hedge1

for (i in (1:le)) {
  print(i)
  liab.tmp <- step1(liab.tmp, month)
  hedge.p.and.l[i] <- - pv(ECBYieldCurve.months[i,], mat.ECB, liab.tmp) +
   pv(ECBYieldCurve.months[i,], mat.ECB, step1(hedge.tmp, month))
  liab.tmp <- step2(liab.tmp, month)
  hedge.mat <- dur(rts1, mat.ECB, liab.tmp)
  zcb1 <- list(time=hedge.mat, cashflow=1)
  hedge.princ <- pv(rts1, mat.ECB, liab.tmp) / pv(rts1, mat.ECB, zcb1)
  hedge.tmp <- list(time=hedge.mat, cashflow=hedge.princ)
}

hedge.p.and.l
summary(hedge.p.and.l)
plot(hedge.p.and.l, type="l")
(total.result <- cumsum(hedge.p.and.l))
last(total.result) / liab.pv # relative hedging error


############################
# Include Convexity in Hedge
############################

# We will hedge using three zero coupon bonds that have plausible maturities. This gives us three variables to solve three equations
# for zero-th, first and second derivative, i.e. for present value, duration and convexity. 

#############
# first hedge
############

liab.dur <- dur(rts1, mat.ECB, liab)
liab.pv <- pv(rts1, mat.ECB, liab)
a1 <- 0.5
(T1 <-  (1-a1) * liab.dur)
(T2 <-  liab.dur)
(T3 <-  (1+6*a1) * liab.dur)
mats <- c(T1, T2, T3) # maturities of zero coupon bonds of hedge

hedge.pv <- c(
  pv(rts1, mat.ECB, list(time=T1, cashflow=1)),
  pv(rts1, mat.ECB, list(time=T2, cashflow=1)),
  pv(rts1, mat.ECB, list(time=T3, cashflow=1))
)

A <- vandermonde.matrix(c(T1, T2, T3), 3)
# durations
A[,2]
# convexities
A[,3]
# money durations (first derivative)
(A.1 <- t(A * hedge.pv))
liab.vector <- c(1, liab.dur, liab.conv) * liab.pv

sol <- solve(A.1, liab.vector)
sol
# check pv, duration and convexity
sol %*% hedge.pv
sol %*% (hedge.pv * A[,2]) / liab.pv
sol %*% (hedge.pv * A[,3]) / liab.pv

# final hedge
hedge2 <- list(time=c(T1, T2, T3), cashflow=sol)
pv(rts1, mat.ECB,  hedge2)
dur(rts1, mat.ECB,  hedge2)
conv(rts1, mat.ECB,  hedge2)

p.and.l.2 <- pv(ECBYieldCurve.months[2,], mat.ECB, step1(hedge2, 1/12)) - pv(ECBYieldCurve.months[2,], mat.ECB, step1(liab, 1/12))
p.and.l.2

pv(ECBYieldCurve.months[2,], mat.ECB, step1(hedge1, 1/12))
pv(ECBYieldCurve.months[2,], mat.ECB, step1(hedge2, 1/12))
pv(ECBYieldCurve.months[2,], mat.ECB, step1(liab, 1/12))

#########################


(le <- length(months1))
hedge.p.and.l.2<- rep(0, le)
month <- 1/12

liab.tmp <- liab
hedge.tmp <- hedge2
a1 <- 0.5

for (i in (1:le)) {
  print(i)
  liab.tmp <- step1(liab.tmp, month)
  hedge.p.and.l.2[i] <- - pv(ECBYieldCurve.months[i,], mat.ECB, liab.tmp) +
    pv(ECBYieldCurve.months[i,], mat.ECB, step1(hedge.tmp, month))
  print(hedge.p.and.l.2[i])
  liab.tmp <- step2(liab.tmp, month)
  liab.pv <- pv(ECBYieldCurve.months[i,], mat.ECB, liab.tmp)
  liab.dur <- dur(ECBYieldCurve.months[i,], mat.ECB, liab.tmp)
  liab.conv <- conv(ECBYieldCurve.months[i,], mat.ECB, liab.tmp)
  T1 <-  (1-a1) * liab.dur
  T2 <-  liab.dur
  T3 <-  (1+6*a1) * liab.dur
  mats <- c(T1, T2, T3)
  hedge.pv <- c(
    pv(ECBYieldCurve.months[i,], mat.ECB, list(time=T1, cashflow=1)),
    pv(ECBYieldCurve.months[i,], mat.ECB, list(time=T2, cashflow=1)),
    pv(ECBYieldCurve.months[i,], mat.ECB, list(time=T3, cashflow=1))
  )
  A <- vandermonde.matrix(mats, 3)
  A.1 <- t(A * hedge.pv)
  liab.vector <- c(1, liab.dur, liab.conv) * liab.pv
  sol <- solve(A.1, liab.vector)
  print(sol)
  hedge.tmp <- list(time=mats, cashflow=sol)
}

hedge.p.and.l.2
summary(hedge.p.and.l.2)
plot(hedge.p.and.l.2, type="l")
(total.result <- cumsum(hedge.p.and.l.2))
last(total.result) / liab.pv # relative hedging error



