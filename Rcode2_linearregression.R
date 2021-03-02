# setwd("~/0 - Teaching/Computational Statistics/Rcode")

##################################################################

# Fitting linear models in R

?lm               # lm stands for linear model

# This uses the same data set as in Rcode1.R:
Advertising <- read.csv("Advertising.csv", header=T)

fit.all <- lm(sales ~ TV + radio + newspaper, data=Advertising)
fit.all           # shows call and estimated beta's

# Interpretation:
#   predicted sales = 2.94 + 0.046*TV + 0.189*Radio - 0.001*Newspaper

summary(fit.all)  # shows more info

# see under "Value" in help file, to see the list of components
#   contained in an object of class "lm". 

# Let's understand all this output and recompute it "by hand"

# Basic things we need:
n <- nrow(Advertising)
names(Advertising)
y <- Advertising[,5]
X <- as.matrix( cbind(1, Advertising[,c(2:4)]) )
XtX.inv <- solve(t(X) %*% X)           # (X^T X)^{-1}

# Compute estimates by hand:
(hatbeta <- XtX.inv %*% t(X) %*% y)      # hatbeta = (X^T X)^{-1} X^T y 
# Compare to summary(fit.all) or coefficients(fit.all)

# Compute fitted values by hand:
y.hat <- X %*% hatbeta                 # yhat = X hatbeta
# Compare to fitted.values(fit.all):
max( abs(y.hat - fitted.values(fit.all)) )

# Re-compute residuals:
res <- y - y.hat 
# Compare to given residuals:
max(abs(res-residuals(fit.all)))

# Re-compute summary statistics of residuals
summary(res, digits=3) # Note some left skewness
# Compare to summary(fit.all)

# Re-compute residual standard error (RSE):
p <- 4                                # intercept and three variables
sum(res^2)/(n-p)                      # sigmahat^2 = RSS/(n-p)
(RSE <- sqrt( sum(res^2)/(n-p) ))     # RSE = sqrt(RSS/(n-p))
# sigmahat^2 is a measure of goodness of fit.
# It is an estimate of sigma^2, the variance of the statistical errors
# The smaller the number, the better the fit (points closer to the line) 
# The RSE is measured in the same units as the dependent variable.

# Plot
plot(residuals(fit.all), ylim=c(-6,6), main="Residuals")
# In case of normally distributed errors, we expect:
#   About 66% of the points are within +/- hat.sigma 
#     from the regression plane (blue dotted lines)
#   Aboute 95% of the points are within +/- 2*hat.sigma 
#     from the regression plane (orange dotted lines)
abline(h=0, lty=2)
abline(h=1.686, lty=3, col="blue", lwd=2)
abline(h=-1.686, lty=3, col="blue", lwd=2)
abline(h=2*1.686, lty=3, col="orange", lwd=2)
abline(h=-2*1.686, lty=3, col="orange", lwd=2)

# Re-compute R^2:
RSS <- sum( (y-y.hat)^2 )
TSS <- sum( (y-mean(y))^2 )
(Rsquared <- 1 - RSS/TSS) 
# Interpretation: proportion of variance explained by regression model

# Re-compute adjusted R^2:
(Rsquared.adj <- 1 - (RSS/(n-p))/(TSS/(n-1)))
# Adjusted for number of variables in the model

# Diagnostic plots
plot(fit.all, which=c(1,2))  # Tukey Anscombe plot and QQ plot of residuals


# See exercise set and exercise session

##################################################################

### Be careful with interpretation of coefficients in multiple regression!

# We illustrate this in a small simulation
n  <- 100000             # use large sample size to get precise estimates
x1 <- rnorm(n,0,1)
x2 <- 1.5*x1 + rnorm(n,0,.8)
y  <- 2*x1 - 3*x2 + rnorm(n,0,.5)
pairs( cbind(x1,x2,y)[1:100,] ) # only first 100 rows

# Fit different regression models:
coef(lm(y~x1))       
coef(lm(y~x2))       
coef(lm(y~x1+x2))    
# Note: The sign of beta1 flipped between the 1st and 3rd model. 
#       The value of beta2 changed between the 2nd and 3rd model. 
# This shows that the interpretation of each beta_k depends on 
#   the other variables in the model! 

# Correct interpretation of beta_k in model Y~x_1+...+x_p:
# Comparing two observations i and j, 
#   where x_{ik} = x_{jk} + 1 (i.e., values for x_k differ by one)
#   and x_{ir} = x_{jr} for all other r in {1,..,p}
#   (i.e., values for all other variables x_r are identical),
#   then it follows by plugging in that E(Y_i) = E(Y_j) + beta_k. 
# So beta_k can be interpreted as the expected difference in y values, 
#   E(Y_i)-E(Y_j), for two observations i and j, when x_{ik} = x_{jk} + 1 
#   and x_{ir} = x_{jr} for all other r in {1,..,p}.
# You also see in the formulation where the other variables come in. 

# You can do the same for the fitted values, using ^y and ^beta:
# Comparing two observations i and j, 
#   with x_{ik} = x_{jk} + 1 (i.e., values for x_k differ by one)
#   and x_{ir} = x_{jr} for all other r in {1,..,p}
#   (i.e., values for all other variables x_r are identical),
#   then by plugging in it follows that ^y_i = ^y_j + ^beta_k.
# So ^beta_k can be interpreted as the difference in fitted y values, 
#   ^y_i - ^y_j, for two observations i and j, when x_{ik} = x_{jk} + 1 
#   and x_{ir} = x_{jr} for all other r in {1,..,p}.

# 3d visualization
library(rgl)
n  <- 100                     # small sample size for plot
x1 <- rnorm(n,0,1)
x2 <- 1.5*x1 + rnorm(n,0,.8)  
y  <- 2*x1 - 3*x2 + rnorm(n,0,.5)

ind <- order(x2)
plot3d(x1[ind],x2[ind],y[ind], col=rainbow(n), xlab="x1", ylab="x2",zlab="y")
fit <- lm(y ~ x1+x2)
planes3d(a=coef(fit)[2], b=coef(fit)[3], c=-1, d=coef(fit)[1], alpha=.5)
# The points are colored according to their x2 value.
# Looking at all points, there is a negative relationship between x1 and y.
#   This corresponds to the negative coefficient of x1 in y~x1.
# Looking at the points of each color separately (i.e., holding x2 fixed), 
#   there is a positive relationship between x1 and y. 
#   This corresponds to the positive coefficient of x1 in y~x1+x2.

# Alternative way to understand ^beta_1 in 
#   regression of y on x1 and x2:
fit.all <- lm(y~x1+x2)
coefficients(fit.all)

# compute ^beta1 in a different way:
res.1.2 <- residuals(lm(x1~x2))  # part of x1 that is *not* linearly explained by x2
res.y.2 <- residuals(lm(y~x2))   # part of y that is *not* linearly explained by x2
fit.beta1 <- lm(res.y.2 ~ res.1.2) 
# linear relationship between y and x1 after taking out 
#   from both of them the part that is linearly explained by x2
coefficients(fit.beta1)

# similarly, we can compute ^beta2:
res.2.1 <- residuals(lm(x2~x1)) # part of x2 that is *not* linearly explained by x1
res.y.1 <- residuals(lm(y~x1))  # part of y that is *not* linearly explaind by x1
fit.beta2 <- lm(res.y.1 ~ res.2.1)
# linear relationship between y and x2 after taking out 
#   from both of them the part that is linearly explained by x1
coefficients(fit.beta2)

# if the design matrix is orthogonal (X^T X = I),
#   then removing or including variables in a model 
#   does not affect the ^beta_k's.


###################################################################

# Understand individual p-values:

# Re-compute standard error for TV:
se.TV <- RSE * sqrt(XtX.inv[2,2])
se.TV

# Reproduce t-value for TV:
tval.TV <- (hatbeta[2]-0) / se.TV
tval.TV

# Compute p-value for TV:
2*pt(abs(tval.TV), df=n-p, lower=FALSE)
# This is so small that we cannot check it.

# Compute p-value for Newspaper:
2*pt(0.177, df=n-p,lower=FALSE)

# We can also reproduce the p-value for Newspaper by comparing two models:
fit.TV.radio <- lm(sales ~ TV + radio, data=Advertising) # leaving out newspaper
# and conducting a partial F-test:
anova(fit.TV.radio, fit.all)    

# The p-value depends on the other variables in the model:
fit.Newsp <- lm(sales ~ newspaper, data=Advertising)
summary(fit.Newsp)     
# Now newspaper is significant.

# How can you explain this?

# Reproduce p-value again by comparing two models:
fit.empty <- lm(sales ~ 1, data=Advertising)
anova(fit.empty,fit.Newsp)


################################################################

# Overall F-test

# P-value at bottom right is from overall F-test
# This compares the full model against the empty model
anova(fit.empty,fit.all)

# Good to look at this first, before looking at individual p-values
#   This avoids multiple testing problems.


######################################################################## 

### Example

football <- read.table("http://www.statsci.org/data/general/punting.txt", header = TRUE)
head(football)
dim(football)
names(football)

pairs(football[,c(1,3,4)])

fit <- lm(Distance ~ R_Strength + L_Strength, data=football)
summary(fit)

# The individual p-values are all insignificant, 
#   whereas the p-value of the overall F-test is highly significant.
# How can you explain this?


########################################################################

# Categorical variables

# Example: Credit data
library(ISLR)
data(Credit)
?Credit
summary(Credit)
pairs(Credit[,c(2:7,12)])
attach(Credit)

# student is a categorical variable with two levels
fit.student <- lm(Balance ~ Income + Student, data=Credit)
summary(fit.student)

# plot results:
coeff <- coefficients(fit.student)
plot(Income, Balance, col=Student, pch=20, main="Credit data (student=red, non-student=black)")
#legend("topleft", c("No student", "Student"), col=c(1,2), pch=20)
abline(a=coeff[1], b=coeff[2], col=1, lwd=2)
abline(a=coeff[1]+coeff[3], b=coeff[2], col=2, lwd=2)

# ethnicity is a categorical variable with more than two levels
fit.ethn <- lm(Balance ~ Income + Ethnicity, data=Credit)
summary(fit.ethn)

# plot results:
coeff <- coefficients(fit.ethn)
plot(Income, Balance, col=Ethnicity, pch=20, main="Credit data (black=Afr-Amer, red=Asian, green=Caucasian")
legend("topleft", c("Afr-Amer", "Asian", "Caucasian"), col=c(1,2,3), pch=20)
abline(a=coeff[1], b=coeff[2], col=1, lwd=2)
abline(a=coeff[1]+coeff[3], b=coeff[2], col=2, lwd=2)
abline(a=coeff[1]+coeff[4], b=coeff[2], col=3, lwd=2)

# It looks like ethnicity is not important for predicting Balance 
#   if we already use income.
# Can we test this? 
# What p-value should we look at?

# Conduct partial F-test again:
fit.income <- lm(Balance ~ Income, data=Credit)
anova(fit.income, fit.ethn)


###################################################################

# Interaction: example R code

fit.interact <- lm(Balance ~ Income + Student + Income*Student, data=Credit)
summary(fit.interact)
coeff <- coef(fit.interact)

plot(Income, Balance, col=Student, pch=20, main="Credit data (red=student, black=non-student")
#legend("topleft", c("No student", "Student"), col=c(1,2), pch=20)
abline(a=coeff[1], b=coeff[2], col=1, lwd=2)
abline(a=coeff[1]+coeff[3], b=coeff[2]+coeff[4], col=2, lwd=2)

# Interaction between Income and Student status means that:
# The "effect" of Income (not meant in a causal sense!) depends on student status
#   in the sense that slopes are different for students and non-students
# The "effect" of Student status depends on Income
#   in the sense that vertical distance between the lines depends on Income level

# One can test if the interaction is significant using a partial F test
# (do this yourself)
