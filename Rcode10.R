# Inference after model selection.

# This R-code was partly motivated by http://joshualoftus.com/turing/shorttalk.pdf

set.seed(55)
n <- 80
p <- 25
dat <- data.frame(matrix(rnorm(n*p), nrow=n))
dat$y <- rnorm(n)
# note: all columns are uncorrelated Gaussian noise

# step() is used here instead of regsubsets() of package leaps 
model <- step(lm(y ~ ., data=dat), k = log(n))  
# default is backward regression, showing all steps
# k=log(n) gives BIC penalty

# if you don't want to see all the output, use trace=0
model <- step(lm(y ~ ., data=dat), k = log(n), trace = 0)  
print(summary(model)$coefficients, digits = 2)

# We select 4 out of the 25 variables. 
# All p-values are significant at the 5% level.
# But all variables were random noise!!

######

# What is going on?
# Consider analogy with outlier detection:
set.seed(18)
n <- 10
x <- rnorm(n,0,1)
# find maximum absolute value (this is sometimes also done by eye)
(max.x <- max(abs(x)))
# compute p-value: probability of obtaining this value, or something
#   more extreme, from N(0,1) distribution
(p.val <- 2*pnorm(max.x, lower=FALSE))  # significant at alpha=0.05

# Note:
# We used the data to select this particular observation.
# We need to account for this selection.
# One can see this as a hidden multiple testing problem:
#   testing the most extreme one is like testing every observation

# Compare original p-value to alpha/nr tests:
0.05/n # not significant
# or, equivalently, compute bonferroni adjusted p-value
(p.val.bonf <- min(1,n*p.val))

# What is the distribution of max(abs(x)) under H0: X_i ~ N(0,1)?
set.seed(99)
nrep <- 1000
res <- numeric(nrep)
for (i in 1:nrep){
  x <- rnorm(n,0,1)
  res[i] <- max(abs(x))
}
hist(res, breaks=20, freq=FALSE, xlim=c(-3,max(res)))
grid <- seq(from=-3, to=max(res), length=100)
lines(grid, dnorm(grid), col="red")
# N(0,1) is clearly the wrong reference distribution

# Similar phenomenon is going on for model selection:
# We use the data to find "important" variables.
# The regular p-values do not take this selection process into account.

# This is a problem for all model selection techqniques:
#   forward, backward, all subsets, lasso, cross validation, ... 
#   considering transformations of the data, 
#   considering special subsets of observations, ... 
# plays important role in "reproducibility crisis"

######

# simple solution: fit only full model 

set.seed(55)
n <- 80
p <- 25
dat <- data.frame(matrix(rnorm(n*p), nrow=n))
dat$y <- rnorm(n)

fit <- lm(y~., data=dat)
summary(fit)
# note that the overall F-test is not significant =>
#   we would not look at the individual effects 
#   (=multiple testing correction that provides weak FWER control)
# alternatively, one can look at individual p-values and apply 
#   explicit multiple testing correction 
print(summary(fit)$coefficients, digits=2)

# After fitting the full model, it may be tempting to drop 
#   all variables with large p-values. 
# Let's see what happens if we do do this. 
# X1 and X19 have the smallest p-values, so let's omit the other variables, 
#   and refit the model with X1 and X19
fit2 <- lm(y~X1+X19, data=dat)
print(summary(fit2)$coefficients, digits=2)

# Both variables seem highly significant.
# But we performed model selection and picked those variables 
#   because they had the smallest p-values!
# So then of course they seem significant in this model.
# Same story as in outlier case... 

# Another simple solution: sample splitting

dat1 <- dat[1:(n/2),]
dat2 <- dat[(n/2+1):n,]
model <- step(lm(y ~ ., data=dat1), k = log(n), trace = 0)  
print(summary(model)$coefficients, digits = 2)

fit <- lm(y ~ X1+X2+X11+X17+X18+X19+X25, data=dat2)
print(summary(fit)$coefficients, digits=2)
# These are honest p-values
# One should still be aware of multiple testing issues, but 
#   only considering 7 variables rather than 25 this time.

# Pros of sample splitting:
# - easy to understand and implement
# - provides "honest" p-values

# Disadvantages of sample splitting:
# - loss of power
# - reproducibility issues (random splits)
#   => one can do many random splits and aggregate p-values, 
#      see  https://arxiv.org/pdf/0811.2177.pdf

# Keyword for possible further reading:
# post selection inference (posi)

