## ALM: NAM INSECURITAS - YIELD CURVE MODELLING ##

# Assignment 
# Author(s):      Joost van Geffen, Alexander Johannes, Bas van Kesteren & Wouter Vink
# Created:        14-04-2022
# Last updated:   15-04-2022

# PRELIMINARIES #

# Importing data

data <- read.csv(file = 'Yieldcurve_data.csv')
colnames(data) <- c('Years', 'Yield')
UFR <- 0.0345 # As specified for 2022 in the most recent EIOPA report

# CREATING PRICE FOR EACH MATURITY # 
data$Price <- 100*(1+data$Yield)^(-data$Years)

# MODELLING WITH A LLP OF 20 YEARS # 
LLP.regulatory <- 20
Convergence.period.regulatory <- 40
data.regulatory <- data[data$Years<=20,]

# MODELLING WITH A LLP OF 30 YEARS #
LLP.internal <- 30
Convergence.period.internal <- 40
data.internal <- data

# SMITH WILSON MODEL # 
Wilson.func <- function(t, m, alpha, UFR){
  # t stands for the maturity in years
  # m presents the maturity of observed input instruments
  # alpha is the speed of convergence
  # UFR is the ultimate forward rate
  
  W <- matrix(0, nrow = length(t), ncol = length(m))
  
  for (i in (1:length(t))) {
    for (k in (1:length(m))) {
      W[i,k] <-  exp(-UFR*(t[i]+m[k]))*(alpha*min(t[i], m[k]) - 0.5*exp(-alpha*max(t[i], m[k]))
                                      *(exp(alpha*min(t[i], m[k]))-exp(-alpha*min(t[i], m[k]))))
    }
  }
  return(W)
}

Searchalpha.func <- function(t.cond, m, UFR, prices.obs){
  # t.cond is the maturity for which the condition of alpha should hold (LLP + convergence period)
  # m presents the maturity of observed input instruments
  # UFR is the ultimate forward rate
  # prices.obs is a vector of observed discount prices for maturities in m
  m.URF <- exp(-UFR*m)
  
  for (a in seq(0.05, 0.2, by=0.0001)){
    zetas <- solve(Wilson.func(m, m, a, UFR))%*%(prices.obs-m.URF)
    years <- c(t.cond, t.cond+1)
    
    P1 <- exp(-UFR*years[1]) + Wilson.func(years[1], m, a, UFR)%*%zetas
    P2 <- exp(-UFR*years[2]) + Wilson.func(years[2], m, a, UFR)%*%zetas
    
    s1 <- (1/P1)^(1/years[1])-1
    s2 <- (1/P2)^(1/years[2])-1
    
    f <- (((1+s2)^years[2])/((1+s1)^years[1]))-1
    
    if (abs(f-UFR) < 0.0001){
      break
    }
  }
  
  return(a)
}

spotrate.func <- function(t, m, alpha, UFR, prices.obs){
  # t stands for the maturity in years
  # m presents the maturity of observed input instruments
  # alpha is the speed of convergence
  # UFR is the ultimate forward rate
  # prices.obs is a vector of observed discount prices for maturities in m
  
  m.URF <- exp(-UFR*m)
  zetas <- solve(Wilson.func(m, m, alpha, UFR))%*%(prices.obs-m.URF)
  discount.factors <-exp(-UFR*t)+(Wilson.func(t, m, alpha, UFR) %*% zetas)
  
  spotrates <- (1/discount.factors)^(1/t)-1
  
  return(spotrates)
}

# Obtain the optimal alpha values for regulatory & internal perspective
price.vector.regulatory <-data.regulatory$Price/100
observed.dates.regulatory <- data.regulatory$Years
alpha.optimal.regulatory <- Searchalpha.func(LLP.regulatory+Convergence.period.regulatory, 
                                             observed.dates.regulatory, UFR, price.vector.regulatory)

price.vector.internal <-data.internal$Price/100
observed.dates.internal <- data.internal$Years
alpha.optimal.internal <- Searchalpha.func(LLP.internal+Convergence.period.internal, 
                                           observed.dates.internal, UFR, price.vector.internal)

# Obtain the yield curves for regulatory & internal perspective (for 63 years given  data in Excel file)
yieldcurve.regulatory <- spotrate.func(seq(1,63,by=1), observed.dates.regulatory, 
                                       alpha.optimal.regulatory, UFR, price.vector.regulatory)

yieldcurve.internal <- spotrate.func(seq(1,63,by=1), observed.dates.internal, 
                                       alpha.optimal.internal, UFR, price.vector.internal)

# Plot the resulting yield curves
plot(seq(1,63,by=1) ,yieldcurve.regulatory , main='Smith-Wilson Yield Curve: Regulatory', 
     xlab='Maturity (years)', ylab='Spot rate (%)', type = "l", lwd=1)

plot(seq(1,63,by=1) ,yieldcurve.internal , main='Smith-Wilson Yield Curve: Internal', 
     xlab='Maturity (years)', ylab='Spot rate (%)', type = "l", lwd=1)

# Compare the two yield curves
plot(seq(1,63,by=1) ,yieldcurve.regulatory , main='Smith-Wilson Yield Curve comparison', 
     xlab='Maturity (years)', ylab='Spot rate (%)', type = "l", lwd=1)
lines(seq(1,63,by=1) ,yieldcurve.internal, type = "l", lty = 2, lwd=1)
legend(42,0.005,legend = c("Regulatory", "Internal"), lty=1:2)
