# A little detour about regression with 
# orthogonal variables. 

# Key formulas:
# hat beta = (X^T X)^{-1} X^T y
# Var(hat beta) = sigma^2 (X^T X)^{-1}

# Key observation:
# If columns of X are orthogonal, (X^T X)^{-1} is 
# a diagonal matrix. 

set.seed(123)
n <- 20
p <- 5
a <- matrix(rnorm(n*p), ncol=5)
x <- princomp(a)$scores  # use pca to create orthogonal variables
round(cov(x),4)          # check orthogonality

# construct y. active variables are x1, x2 and x3
eps <- rnorm(n)
y <- 2*x[,1] -3*x[,2] + x[,3] + eps

dat <- data.frame(cbind(y,x))
names(dat) <- c("y","x1","x2","x3","x4","x5")

# fit different models
fit0 <- lm(y~1, data=dat)
fit1 <- lm(y~x1, data=dat)
fit2 <- lm(y~x1+x2, data=dat)
fit3 <- lm(y~x1+x2+x3, data=dat)
fit4 <- lm(y~x1+x2+x3+x4, data=dat)
fit5 <- lm(y~x1+x2+x3+x4+x5, data=dat)

# check that hat beta_1 does not depend on other 
# variables in the model:
fit1$coef[2]
fit2$coef[2]
fit3$coef[2]
fit4$coef[2]
fit5$coef[2]
# the same holds for all other beta hat's, and for 
# other subsets of variables in the model.

# Now let's look at the standard errors and p-values:
coef(summary(fit1))
coef(summary(fit1))[2,2]
coef(summary(fit2))[2,2]
coef(summary(fit3))[2,2]
coef(summary(fit4))[2,2]
coef(summary(fit5))[2,2]
# These are not the same, since we have different 
# error variances and different degrees of freedom 
# in the two models:
# Since x2 and x3 explain some of the variance in y, the 
# sigma's in fit2 and fit3 get smaller, leading to 
# smaller standard errors and more significant p-values. 
# In fit4 and fit5, the additional variables do not 
# explain much variance in y, but we lose a degree of 
# freedom, leading to slightly different p-values. 

anova(fit0,fit1,fit2,fit3,fit4,fit5)
# Note that the p-value comparing fit0 and fit1 is 
# exactly the same as the p-value corresponding to 
# x1 in fit5. 
# This is the case, because anova uses the RSS and degrees of 
# freedom from the largest model, in this case fit5.

anova(fit0,fit1)
# Now the p-value comparing fit0 and fit1 is different, 
# and equals the p-value from fit1. 
# This is because now the largest model is fit1, and 
# anova uses the RSS and degrees of freedom from this 
# model. 


