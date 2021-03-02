# Parametric bootstrap (example adapted from help file of boot())

# We look at the air-conditioning data. 
library(boot)
data(aircondit)
?aircondit
summary(aircondit)

par(mfrow=c(1,1))
plot(aircondit$hours)  # data are sorted

# We conduct parametric bootstrap under exponential model
# Recall if X ~ Exp(eta), then E(X)=1/eta.
# Based on n i.i.d. observations, the MLE of eta is 1/(mean of observations)
# We are interested in estimating the *median* of the distribution (theta). 

# Function to compute the median of the data:
air.fun <- function(data) {
  return(median(data$hours))
}

air.rg <- function(data, mle) {
  # Function to generate random exponential variates.
  # mle will contain the mle of eta
  out <- data
  out$hours <- rexp(nrow(out), mle)
  return(out)
}

# check plots:
par(mfrow=c(3,3))
plot(aircondit$hours,ylim=c(0,600), ylab="hours", col="blue")

replicate(8, plot(sort(air.rg(aircondit, 1/mean(aircondit$hours))$hours),ylim=c(0,600), ylab="hours"))

# compare to nonparametric bootstrap:
par(mfrow=c(3,3))
plot(aircondit$hours,ylim=c(0,600), col="blue", ylab="hours")
replicate(8, plot(sort(aircondit$hours[sample(1:12,12,replace=T)]),ylim=c(0,600),ylab="hours"))

# conduct parametric bootstrap:
air.boot <- boot(aircondit, air.fun, R=1000, sim = "parametric",
                 ran.gen = air.rg, mle = 1/mean(aircondit$hours))

air.boot
par(mfrow=c(1,1))
hist(air.boot$t)
abline(v=median(aircondit$hours), col="red")

boot.ci(air.boot,type=c("norm","basic","perc"))

# note normal CI uses bias correction.
# its midpoint is \hat \theta_n - \hat bias 

################

# Illustration of bootstrap and regression:

library(boot)

data(mtcars)
fit <- lm(mpg ~ wt + disp, data=mtcars)

# random X approach: case resampling

# function to obtain bootstrapped regression estimates 
bootstr <- function(formula, data, indices) {
  d <- data[indices,]
  fit <- lm(formula, data=d)
  return(coef(fit)) 
} 

# bootstrapping with 1000 replications 
results <- boot(data=mtcars, statistic=bootstr, 
                R=1000, formula=mpg~wt+disp)

# view results
results
plot(results, index=1) # intercept 
plot(results, index=2) # wt 
plot(results, index=3) # disp 

# get 95% confidence intervals 
boot.ci(results, type=c("norm", "basic"), index=1) # intercept 
boot.ci(results, type=c("norm", "basic"), index=2) # wt 
boot.ci(results, type=c("norm", "basic"), index=3) # disp

# we can use duality between confidence intervals and tests:
# since zero is not included in any of the confidence intervals, 
# all coefficients are significant. 

#####

# Fixed X approach: bootstrap residuals (resampling or model-based)
# We will use resampling here. -- here the residuals should be i.i.d:
# from the same distribution, therefore has the same variance - homoscedastic,
# otherwise the relationship between the heteroscedastic residuals and X 
# would be destroyed by the random resampling. If this is the case, then 
# can just do case resampling: to resample the x and y pairs.

# function to obtain bootstrapped regression estimates
bootstr.resid <- function(data, indices) {
  resid.new <- fit$residuals[indices]
  y.new <- fit$fitted + resid.new
  data.new <- data.frame(mpg=y.new, wt=data$wt, disp=data$disp)
  fit.new <- lm(mpg ~ wt + disp, data=data.new)
  return(coef(fit.new)) 
} 

fit <- lm(mpg ~ wt + disp, data=mtcars)

results.2 <- boot(data=mtcars, statistic=bootstr.resid, R=1000)

# view results
results.2
plot(results.2, index=1) # intercept 
plot(results.2, index=2) # wt 
plot(results.2, index=3) # disp 

# get 95% confidence intervals 
boot.ci(results.2, type="norm", index=1) # intercept 
boot.ci(results.2, type="norm", index=2) # wt 
boot.ci(results.2, type="norm", index=3) # disp

# for comparison:
boot.ci(results, type="norm", index=3)

# It makes little difference in this case. 
