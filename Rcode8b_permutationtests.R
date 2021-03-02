# Permutation tests

# Example of a one-sample or paired permutation/randomization test:

data(sleep)
?sleep
sleep
attach(sleep)

# Note that we have two observations on each person, one with drug 1, 
#   and one with drug 2. 
# For each person, we take the difference between the extra sleep hours 
#   with the different drugs 
# Note that it is a bit special here that the values of "extra" are 
#   already differences wrt control. This is not the case in general 
#   and is not required for a paired test.

# We suspect that drug 2 increases sleep more 
#   than drug 1. 

(diff.vec <- extra[group==1]-extra[group==2])
stripchart(diff.vec, method = "stack", xlab = "hours",
           main = "Sleep prolongation (n = 10)")
stem(diff.vec)

# wilcoxon rank sum test
wilcox.test(diff.vec, alternative="less")
# we reject H0 in favor of Ha

# In the exercises you will conduct the Wilcoxon signed rank test 
#   "by hand" via permutations. 

# We will now choose a different test statistic and conduct a 
#    permutation test based on mean of differences

# Observed mean difference:
(observed.mean <- mean(diff.vec))

# Let mu_diff = E(extra sleep with drug 1 - extra sleep 
#                 with drug 2)
# H0: drugs affect sleep equally: mu_diff=0
# Ha: drug 2 increases sleep more: mu_diff<0
# Let's set alpha=0.05

# How can we create a null distribution?
# Under H_0, we can permute treatment assignments within 
#    a person. 
# This amounts to multiplying the difference by +1 or -1. 

# Since the sample size is small, we will consider all 
#   possible vectors containing +1's and -1's
# Note that there are 2^10 = 1024 of those. 
n <- 10
res.sleep <- NULL
for (j in 1:n){
  comb <- combn(c(1:10),j) # lists all possible ways of 
                           # taking j elements from c(1:10)
  m <- ncol(comb)          # number of ways
  for (i in 1:m){
    signs <- rep(1, n)     # set all elements to +1
    signs[comb[,i]] <- -1  # set selected elements to -1
    ynew <- diff.vec * signs
    res.sleep <- c(res.sleep, mean(ynew))
  }
}

hist(res.sleep, breaks=30)
abline(v=observed.mean, col="red")
(pval <- (sum(res.sleep<=observed.mean)+1)/(length(res.sleep)+1) )

# We again reject H0 in favor of Ha

# Compare to Monte Carlo version: here duplicates are possible
mean.diff.one.rep <- function(y){
  n <- length(y)
  signs <- sample(c(-1,1), n, replace=T)
  ynew <- y * signs
  return(mean(ynew))
}

res2 <- replicate(100000, mean.diff.one.rep(diff.vec))
hist(res2, breaks=30)
abline(v=observed.mean, col="red")
(pval <- (sum(res2<=observed.mean)+1)/(length(res2)+1) )


###############################################

# permutation test for correlation

n <- 30
x <- runif(n,-3,4)
y <- x^3 + .5*x^2  + rnorm(n, 0, 10)
plot(x,y)

(observed.cor <- cor(x,y, method="spearman"))
# rank correlation

spearman.one.rep <- function(x,y){
  n <- length(y)
  ynew <- sample(y, n, replace=F)
  return(cor(x,ynew, method="spearman"))
}

nrep <- 100000
res.spearman <- replicate(nrep, spearman.one.rep(x,y))

hist(res.spearman)
abline(v=observed.cor)
(pval <- ( sum(abs(res.spearman) >= abs(observed.cor)) + 1) / (nrep+1))

##################################################################

# Finally, let's take a closer look at the permutation distribution 

# simulate data from two normal distributions
set.seed(985)
nrep <- 100000
n1 <- 10   # sample size group 1
n <- 20    # total sample size
n2 <- n-n1 # sample size of group 2

ynull <- c(rnorm(n1, 0, 1), rnorm(n2, 0,1))
yalt <- c(rnorm(n1, 0, 1), rnorm(n2, 15, 1))  
# note I set a large shift to make the effect more clear

# we look at the t-statistic, but we will consider a 
#   permutation distribution rather than the theoretical 
#   t-distribution. 
(obs.null <- t.test(ynull[1:n1], ynull[(n1+1):n])$statistic)
(obs.alt <- t.test(yalt[1:n1], yalt[(n1+1):n])$statistic)

# compute permutation distributions:
t.stat.one.rep <- function(y, n1, n){
  ynew <- sample(y, n, replace=F)
  return(t.test(ynew[1:n1], ynew[(n1+1):n])$statistic)
}

res.t.stat.null <- replicate(nrep, t.stat.one.rep(ynull, n1, n))
res.t.stat.alt <- replicate(nrep, t.stat.one.rep(yalt, n1, n))

hist(res.t.stat.null, freq=F, breaks=100, xlim=c(-5,5),main=paste("Permutation distribution under H0, n=",n,",n1=",n1))
x <- seq(from=-4,to=4,by=.1)
lines(density(res.t.stat.null), col="blue", lwd=2)
lines(x, dt(x,n-2), col="red", lwd=2)
# in this case we know the true null distribution under 
#   the usual sampling framework: 
#   t with n-2 degrees of freedom 
# the permutation distribution equals this distribution
#   (up to discretization)

hist(res.t.stat.alt, freq=F, breaks=150, xlim=c(-5,5), main=paste("Permutation distribution under Ha, n=",n,",n1=",n1))
x <- seq(from=-4,to=4,by=.1)
lines(density(res.t.stat.alt), col="blue", lwd=2)
lines(x, dt(x,n-2), col="red", lwd=2)
# Now the permutation distribution does not equal the null 
#   distribution under the usual sapmling framework.
# This is no problem, as for type I error control we only need 
#   to control what happens under H_0. And this was computed
#   under H_a. 
# Asymptotically, however, we do get the null distribution back.
#   (try increasing the sample sizes)


