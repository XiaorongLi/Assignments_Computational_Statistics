# Adapted from ISLR Ch 6 

# Chapter 6 Lab 1: Subset Selection Methods

# Best Subset Selection

library(ISLR)
data(Hitters)
?Hitters

head(Hitters)
names(Hitters)
dim(Hitters)

summary(Hitters) # note missing variables in salary
sum(is.na(Hitters$Salary))  # we can also count them like this
Hitters=na.omit(Hitters)  # remove all rows with missing values
dim(Hitters)
summary(Hitters)

library(leaps)
?regsubsets

regfit.full <- regsubsets(Salary~., data=Hitters, nvmax=19) # consider all possible submodels
reg.summary <- summary(regfit.full)
reg.summary
names(reg.summary)
reg.summary$rsq
reg.summary$adjr2
reg.summary$which

# plot criteria versus model size 
#   (model size = number of variables in the model)
par(mfrow=c(2,2))
# rss
plot(reg.summary$rss,xlab="Model size",ylab="RSS",type="l")
(opt <- which.min(reg.summary$rss))
points(opt, reg.summary$rss[opt], col="red",cex=2,pch=20)
# adjr2
plot(reg.summary$adjr2,xlab="Model size",ylab="Adjusted RSq",type="l")
(opt <- which.max(reg.summary$adjr2))
points(opt, reg.summary$adjr2[opt], col="red",cex=2,pch=20)
# Cp
plot(reg.summary$cp,xlab="Model size",ylab="Cp",type='l')
(opt <- which.min(reg.summary$cp))
points(opt, reg.summary$cp[opt], col="red", cex=2, pch=20)
# BIC
plot(reg.summary$bic,xlab="Model size",ylab="BIC",type='l')
(opt <- which.min(reg.summary$bic))
points(opt, reg.summary$bic[opt], col="red", cex=2, pch=20)

# use built in plot function
?plot.regsubsets
par(mfrow=c(1,1))
plot(regfit.full, scale="r2")
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")
# the nbest models of each size are shown, ordered by the given criterion
# note that the y-axis is not linear
# the grey scale simply shows how good the models are according
#   to the given criterion. Darker means better. 

# get the best model with 6 predictors
coef(regfit.full, 6)


#### Forward and Backward Stepwise Selection

regfit.fwd <- regsubsets(Salary~., data=Hitters, nvmax=19, method="forward")
summary(regfit.fwd)

regfit.bwd <- regsubsets(Salary~., data=Hitters, nvmax=19, method="backward")
summary(regfit.bwd)

coef(regfit.full, id=7)
coef(regfit.fwd, id=7)
coef(regfit.bwd, id=7)


### Choosing Among Models using training and test set

# create indices for training and test sets 
#   for validation set approach
set.seed(245)
train=sample(c(TRUE,FALSE), nrow(Hitters), replace=TRUE)
test=(!train)

# redo best subset regression on training data 
#   (why do we have to redo this?)
regfit.best <- regsubsets(Salary~., data=Hitters[train,], nvmax=19)

test.mat <- model.matrix(Salary~., data=Hitters[test,]) # design matrix
head(test.mat)

# how to get SSE errors for best model of given size:
size <- 3
(coefi <- coef(regfit.best, id=size))
test.mat[,names(coefi)]
(pred <- test.mat[,names(coefi)] %*% coefi)
(val.error <- mean((Hitters$Salary[test]-pred)^2))

# now let's do this for every model size:
val.errors <- rep(NA,19)
for(i in 1:19){
  coefi <- coef(regfit.best, id=i)
  pred <- test.mat[,names(coefi)] %*% coefi
  val.errors[i] <- mean((Hitters$Salary[test]-pred)^2)
}
val.errors
plot(c(1:19), val.errors, type="l")
(opt <- which.min(val.errors))
points(opt, val.errors[opt], col="red", pch=20)

# model with 7 variables is best in terms of prediction 
#   error on the validation set

# finally, refit model with 7 variables on full data to obtain our final best model
regfit.best <- regsubsets(Salary~., data=Hitters, nvmax=19)
coef(regfit.best, id=opt)

# Additional info: as there is no predict function for regsubsets, 
#   ISLR provided this:
predict.regsubsets <- function(object,newdata,id,...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form,newdata)
  coefi <- coef(object,id=id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}
# We could have also used this above when computing the 
#   SSE on the test set. 
# 
####################


# Chapter 6 Lab 2: Ridge Regression and the Lasso

library(glmnet)
?glmnet

library(ISLR)
data(Hitters)
Hitters <- na.omit(Hitters)

# define x and y, as we cannot use y~x notation in glmnet
x = model.matrix(Salary~., data=Hitters)
x = x[,-1] # remove first column (intercept)
y = Hitters$Salary
# check dimensions
dim(x)
length(y)

# Ridge Regression
grid <- 10^seq(from = 10, to = -2, length=100)  # grid for lambda values
grid
ridge.mod <- glmnet(x, y, alpha=0, lambda=grid)    
# alpha=0 corresponds to ridge
# alpha=1 corresponds to lasso
# [always check this in help file, as this differs across implementations]

dim(coef(ridge.mod)) # each row corresponds to a predictor, 
                     # each column to value of lambda

ridge.mod$lambda[50] # =grid[50]
round(coef(ridge.mod)[,50],2) # corresponding coefficient estimates
sum(coef(ridge.mod)[-1,50]^2) # corresponding L_2^2 norm of the coefficients

ridge.mod$lambda[60] # =grid[60]
round(coef(ridge.mod)[,60],2)
sum(coef(ridge.mod)[-1,60]^2)

plot(ridge.mod, xvar="lambda")
abline(v=log(705.48), lty=2, col="gray")
abline(h=13.68, lty=2, col="gray")
abline(h=-54.66, lty=2, col="gray")
abline(h=8.61, lty=2, col="gray")

# get coefficient estimates for new value lambda=35:
round(predict(ridge.mod, s=35, type="coefficients"),2)

# create training and test set for validation set approach 
# (on purpose showing you a different method than in the 
#  subset selection part)
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2, replace=FALSE)   
   # indices for training data
test <- (-train)                        
   # indices we *leave out* for test data
y.test <- y[test]
x.test <- x[test,]

# conduct ridge regression on training data
ridge.mod = glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh=1e-12)

# evaluate test MSE for fit corresponding to lambda=4
ridge.pred.4 = predict(ridge.mod, s=4, newx=x.test)
mean((ridge.pred.4 - y.test)^2)

# compare to test MSE for model with only an intercept
fitted.intercept <- mean(y[train])
mean((fitted.intercept - y.test)^2)

# note that we can mimic the above by taking lambda very large in ridge regression:
ridge.pred.inf <- predict(ridge.mod, s=1e10, newx=x.test)
mean((ridge.pred.inf - y.test)^2)

# compare to LS model:
ridge.pred.0 <- predict(ridge.mod, s=0, newx=x[test,])
mean((ridge.pred.0 - y.test)^2)

# choose lambda via cross validation 
?cv.glmnet   # conducts 10 fold cross validation by default
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha=0)
par(mfrow=c(1,1))
plot(cv.out)

# lambda minimizing mean squared error in cross validation
bestlam <- cv.out$lambda.min
bestlam
log(bestlam)

cv.out

# largest value of lambda such that error is within 
# 1 standard error of the minimum.
abline(h=105342+36409, col="blue")
lambda.1se = cv.out$lambda.1se
lambda.1se
log(lambda.1se)
# rationale:  choose the simplest model whose accuracy is 
#   comparable with the best model.

# evaluate test MSE for bestlam
ridge.pred <- predict(ridge.mod, s=bestlam, newx=x[test,])
mean((ridge.pred-y.test)^2)

# evaluate test MSE for lambda.1se
ridge.pred <- predict(ridge.mod, s=lambda.1se, newx=x[test,])
mean((ridge.pred-y.test)^2)

# refit on the full data with lambda=bestlam
out <- glmnet(x, y, alpha=0)   # use default values for lambda
(coef.ridge <- predict(out, type="coefficients", s=bestlam))
  # plug in bestlam


### lasso

# By using alpha=1 we can do similar things for the lasso:
# Conduct lasso regression on training data
lasso.mod <- glmnet(x[train,], y[train], alpha=1, 
                   lambda=grid, thresh=1e-12)

set.seed(1919)
cv.out <- cv.glmnet(x[train,],y[train],alpha=1)

par(mfrow=c(1,1))
plot(cv.out)
cv.out
abline(h=109789+33904, col="blue")

bestlam=cv.out$lambda.min
bestlam  
log(bestlam)

lambda.1se=cv.out$lambda.1se
lambda.1se
log(lambda.1se)

# evaluate test MSE for lambda=bestlam
lasso.pred <- predict(lasso.mod, s=bestlam, newx=x[test,])
mean((lasso.pred-y.test)^2)

# evaluate test MSE for lambda=lambda.1se
lasso.pred <- predict(lasso.mod, s=lambda.1se, newx=x[test,])
mean((lasso.pred-y.test)^2)

# refit on the full data
out <- glmnet(x,y,alpha=1)   # use default values for lambda
(coef1=predict(out,type="coefficients",s=bestlam))  
   # plug in bestlam

(coef2=predict(out,type="coefficients",s=lambda.1se))  
   # plug in lambda.1se

