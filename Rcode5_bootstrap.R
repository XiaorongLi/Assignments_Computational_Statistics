# The Bootstrap (The first part of this code is adapted from ISLR Ch 5)

library(ISLR) 
data(Portfolio)
?Portfolio

# estimate alpha based on data$X[index] and data$Y[index]
alpha.fn <- function(data,index){
  X <- data$X[index]
  Y <- data$Y[index]
  return( (var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)) )
}

# estimate alpha based on the original data:
alpha.fn(Portfolio,c(1:100))

# look at the function sample():
?sample
sample(c(1:100),100,replace=F)   # was used for CV
sample(c(1:100),100,replace=T)   # is used for bootstrap

# estimate alpha based on a bootstrap sample:
set.seed(1)
alpha.fn(Portfolio,sample(c(1:100),100,replace=T))

# conduct 1000 bootstrap samples
library(boot)
?boot
res.boot <- boot(Portfolio, alpha.fn, R=1000)

res.boot$t0          # estimate on original data
res.boot$t           # estimates on bootstrap samples

par(mfrow=c(1,1))
hist(res.boot$t)

mean(res.boot$t)                 # mean of estimates on bootstrap samples
mean(res.boot$t)-res.boot$t0     # estimate of bias of \hat alpha
sd(res.boot$t)                   # estimate of standard deviation of \hat alpha
res.boot                         # shows key information



###########################################################
# Some simulations:

# We consider data from a lognormal distribution with mu=0, sigma=1
# See: https://en.wikipedia.org/wiki/Log-normal_distribution

library(boot)

# draw density and cumulative distribution function (CDF)
par(mfrow=c(1,2))
grid <- seq(from=0,to=5,length=200)
plot(grid, dlnorm(grid), type="l", main="Lognormal density")
plot(grid, plnorm(grid), type="l", main="Lognormal CDF")
theta <- 1    # true median
abline(h=.5,lty=2)
abline(v=theta,lty=2)

# function that samples data set of size n from lognormal distribution
#   and computes empirical median
sim.lnorm.median <- function(n){
  data <- rlnorm(n)
  return(median(data))
}

# simulation settings:
n <- c(10,50,250,1250)
reps <- 10000

sim.result <- matrix(numeric(4*reps),ncol=4)
# ith column will contain estimated medians for sample size n[i]

set.seed(123)

par(mfrow=c(2,2))
xlim=c(0,3)
for(i in 1:4){
  sim.result[,i] <- replicate(reps,sim.lnorm.median(n[i]))
  hist(sim.result[,i], freq=F, breaks=20, xlim=xlim, 
       main=paste("Simulated distr., n=",n[i],sep=""),xlab="")
  grid <- seq(from=0,to=4,length=1000)
  lines(grid, dnorm(grid, 1, sd=1/(2*dlnorm(1)*sqrt(n[i]))),col="red")
}
# red curve is density of asymptotic normal distribution of hat theta_n
#   [ standard deviation of hat theta_n is 1/(2*f(theta)*sqrt(n)) ]

# rescaled distribution: sqrt(n)*(hat theta_n - theta)
sim.scaled <- matrix(numeric(4*reps), ncol=4)
for (i in 1:4){
  sim.scaled[,i] <- sqrt(n[i])*(sim.result[,i] - theta)
}

par(mfrow=c(2,2))
xlim=c(-5,5)
for(i in 1:4){
  hist(sim.scaled[,i], freq=F, breaks=20,xlim=xlim, main=paste("Rescaled sim. distr., n=",n[i],sep=""))
  grid <- seq(from=-5,to=5,length=1000)
  lines(grid, dnorm(grid, 0, sd=1/(2*dlnorm(1))),col="red")
}
# red curves are normal approximations of density of sqrt(n)*(hat theta_n - theta)

# plot empirical distribution functions of rescaled quantities:
par(mfrow=c(2,2))
xlim=c(-5,5)
for (i in 1:4){
  plot(ecdf(sim.scaled[,i]), xlim=xlim,
       main=paste("Empirical distribution, n=",n[i],sep=""))
  grid <- seq(from=-5,to=5,length=200)
  lines(grid, pnorm(grid, 0, sd=1/(2*dlnorm(1))), col="red")
}


# function that computes median of data[index]
median.fn <- function(data, index){
  return(median(data[index]))
}

# settings for bootstrap:
set.seed(321)
R <- 20000
n <- 250
#n <- 5000
xlim <- c(theta-8/sqrt(n),theta+8/sqrt(n))

boot.result <- matrix(numeric(4*R),ncol=4)
# ith column will contain bootstrapped estimators for data set i

medians <- numeric(4)
# ith entry will contain estimator based on original data set i

par(mfrow=c(2,2))
for(i in 1:4){
  data <- rlnorm(n)
  medians[i] <- median(data)
  boot.result[,i] <- boot(data, median.fn, R)$t
  hist(boot.result[,i], freq=F, breaks=20, xlim=xlim, 
       main=paste("Bootstrap distr., data set ",i),
       xlab=paste("n=",n,sep=""))
  abline(v=medians[i],col="red",lwd=2)
}

# rescaled distribution: sqrt(n)*(hat theta_n* - hat theta_n)
boot.scaled <- matrix(numeric(4*R), ncol=4)
for (i in 1:4){
  boot.scaled[,i] <- sqrt(n)*(boot.result[,i] - medians[i])
}

par(mfrow=c(2,2))
xlim=c(-5,5)
ylim=c(0,.7)
for(i in 1:4){
  hist(boot.scaled[,i], freq=F, breaks=20, xlim=xlim, ylim=ylim, 
       main=paste("Rescaled bootstr. distr, data set",i),
       xlab=paste("sd=",round(sd(boot.scaled[,i]),2)))
  grid <- seq(from=-5,to=5,length=1000)
  lines(grid, dnorm(grid, sd=1/(2*dlnorm(1))),col="red")
}
# red curve is asymptotic distribution of sqrt(n)*(hat theta_n - theta)

# asymptotic standard deviation of rescaled bootstrap distribution should be:
1/(2*dlnorm(1))

# look at CDFs
par(mfrow=c(2,2))
for (i in 1:4){
  plot(ecdf(boot.scaled[,i]), 
       main=paste("Rescaled bootstr. distr., data set",i),
       verticals = TRUE, do.points = FALSE)
  grid <- seq(from=-5,to=5,length=100)
  lines(grid, pnorm(grid, sd=1/(2*dlnorm(1))), col="red")
}
# red line is theoretical limiting distribution from before


