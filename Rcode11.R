# Based on ISLR Chapter 7 Lab: Non-linear Modeling

library(ISLR)

?Wage
attach(Wage)

# Polynomial Regression 

# default implementation uses orthonogal polynomials:
fit=lm(wage~poly(age,4),data=Wage)

# to understand what is going on internally:
xx <- poly(age,4)  # constructs x-matrix
dim(xx)
par(mfrow=c(2,2))
for (j in 1:4){
   plot(age, xx[,j])
}
round(cov(xx),5)  # columns are uncorrelated

coef(summary(fit))
round(coef(fit),2)

# estimated coefficients are the same in a model 
# that goes up to a different degree:
round(coef(lm(wage~poly(age,2),data=Wage)),2)

# If you want, you can also use x, x^2, x^3 and x^4
# without orthoginalization:
fit2a=lm(wage~poly(age,4,raw=T),data=Wage)
round(coef(fit2a),2)
# or equivalently:
fit2b=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
round(coef(fit2b),2)

# But now estimated coefficients are not identical in 
# models with different degrees:
fit3 = lm(wage~age+I(age^2), data=Wage)
round(coef(fit3),2)

# Why?  
# See Rcode12. 

# Compute fitted values and confidence intervals
#   based on orthogonal polynomials
(agelims=range(age))
(age.grid=seq(from=agelims[1], to=agelims[2], by=1))
preds=predict(fit,newdata=list(age=age.grid),
              se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,
               preds$fit-2*preds$se.fit)

# plot results:
par(mfrow=c(1,1))
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Degree-4 Polynomial")
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

# Fitted values and confidence intervals are the 
# same when using non-orthogonal polynomials:
preds2a=predict(fit2a,newdata=list(age=age.grid),
                se=TRUE)
max(abs(preds$fit-preds2a$fit))
max(abs(preds$se.fit-preds2a$se.fit))

# Test what degree we should use
fit.1=lm(wage~age,data=Wage)
fit.2=lm(wage~poly(age,2),data=Wage)
fit.3=lm(wage~poly(age,3),data=Wage)
fit.4=lm(wage~poly(age,4),data=Wage)
fit.5=lm(wage~poly(age,5),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)
round(coef(summary(fit.5),2),4)
# note identical p-values 
# => this is nice about using orthogonal polynomials (see Rcode12.R)
# degree 3 or 4 seems sufficient

# this does not work for non-orthogonal polynomials:
fit.5b <- lm(wage~poly(age,5,raw=T),data=Wage)
round(coef(summary(fit.5b),2),4)
# now estimated coefficients and t-values and
# p-values depend on other variables in the model
# (except for the last one)

# anova always works:
fit.1b=lm(wage~age,data=Wage)
fit.2b=lm(wage~poly(age,2,raw=T),data=Wage)
fit.3b=lm(wage~poly(age,3,raw=T),data=Wage)
fit.4b=lm(wage~poly(age,4,raw=T),data=Wage)
fit.5b=lm(wage~poly(age,5,raw=T),data=Wage)
anova(fit.1b,fit.2b,fit.3b,fit.4b,fit.5b)

# one can also use anova with other variables in 
# the model
fit.1=lm(wage~education+poly(age,1),data=Wage)
fit.2=lm(wage~education+poly(age,2),data=Wage)
fit.3=lm(wage~education+poly(age,3),data=Wage)
fit.4=lm(wage~education+poly(age,4),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4)
# with education in the model, degree 3 seems sufficient

##############

# Splines

library(splines)  

# we will use bs() to generate a basis
# by default this generates a basis for cubic splines
fit=lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
pred=predict(fit,newdata=list(age=age.grid),se=T)

par(mfrow=c(1,1))
plot(age,wage,col="gray")
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se,lty="dashed")
lines(age.grid,pred$fit-2*pred$se,lty="dashed")

# when specifying degrees of freedom, R places
# knots evenly at percentiles. 
# For K knots, specify df=K+3
dim(bs(age,knots=c(25,40,60)))
dim(bs(age,df=6))
# leads to design matrices of same dimension. 

# locations of knots
attr(bs(age,df=6),"knots")

# bs() also has degree argument. Default is 3 
#  (cubic splines), but we can of course change this.

# use ns() to fit natural splines. 
# for K knots, specify df=K+1
attr(ns(age,df=4),"knots")
fit2=lm(wage~ns(age,df=4),data=Wage)
pred2=predict(fit2,newdata=list(age=age.grid),se=T)
lines(age.grid, pred2$fit,col="red",lwd=2)
lines(age.grid,pred2$fit+2*pred2$se,lty="dashed", col="red")
lines(age.grid,pred2$fit-2*pred2$se,lty="dashed", col="red")

##############
par(mfrow=c(1,1))

# smoothing splines: use function smooth.spline():
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Smoothing Spline")
fit=smooth.spline(age,wage,df=16)
fit2=smooth.spline(age,wage,cv=TRUE)
fit2$df
fit$lambda
fit2$lambda
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

# Local regression (loess):
agelims=range(age)
age.grid=seq(from=agelims[1], to=agelims[2], by=1)
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Local Regression")
fit=loess(wage~age,span=.2,data=Wage)
fit2=loess(wage~age,span=.5,data=Wage)
fit3=loess(wage~age,data=Wage)
lines(age.grid,predict(fit,data.frame(age=age.grid)),col="red",lwd=2)
lines(age.grid,predict(fit2,data.frame(age=age.grid)),col="blue",lwd=2)
lines(age.grid,predict(fit3,data.frame(age=age.grid)),col="orange",lwd=2)
legend("topright",legend=c("Span=0.2","Span=0.5","Span=0.75"),col=c("red","blue","orange"),lty=1,lwd=2,cex=.8)


#######

# GAMs
library(gam)

gam1=lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)
# when using regression splines, we can just put 
#   everything in one big model. 

# we can plot the results convenviently with plot.Gam:
par(mfrow=c(1,3))
plot.Gam(gam1, se=TRUE, col="red")
# discuss interpretation. 

# When using smoothing splines or local regression, 
# we can use backfitting to obtain a solution.
# Options: s() for smoothing splines, lo() for loess
gam.m3=gam(wage~s(year,4)+s(age,5)+education,data=Wage)

par(mfrow=c(1,3))
plot(gam.m3, se=TRUE,col="blue")
# note very similar to fit with natural splines

# consider year: not in the model, linear, or non-linear
gam.m1=gam(wage~s(age,5)+education,data=Wage)
gam.m2=gam(wage~year+s(age,5)+education,data=Wage)
gam.m3=gam(wage~s(year,4)+s(age,5)+education,data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F")
# we should include year, but nonlinear function of year
#  seems not required. 
plot(gam.m2, se=TRUE, col="blue")

# using loess()
gam.lo=gam(wage~year+lo(age,span=0.7)+education,data=Wage)
plot(gam.lo, se=TRUE, col="green")





