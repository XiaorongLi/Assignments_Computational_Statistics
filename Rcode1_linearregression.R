# set working directory appropriately 
# (adapt this to your own diretory structure):
# setwd("~/0 - Teaching/Computational Statistics 2020/Rcode")

#################################################################

### Advertising data (from ISLR)

# You can download this from: 
#   http://www-bcf.usc.edu/~gareth/ISL/data.html
# You can put it in your working directory (see above),
# and then read it with the command given below.

# use read.csv to read data:
Advertising <- read.csv("Advertising.csv", header=T)

# look at data:
head(Advertising)                 # look at first few rows
?head                             # use ? to look at help file
Advertising <- Advertising[,-1]   # remove first column
head(Advertising)                 # look at first few rows again
dim(Advertising)                  # determine the dimensions; 
                                  # rows are samples, columns are variables  
names(Advertising)                # get the variable names
summary(Advertising)              # get summary statistics per variable

plot(sales~TV, data=Advertising)

# make pairs plot:
# jpeg("Advertising.jpg")
  pairs(Advertising)                # create pairs plot
# dev.off()

####################################################################  
  
#### Fitted lines are random and not equal to "true" regression line

set.seed(543)

# simulate from model y  = 2 + 3*x + eps, with eps~N(mean=0,sd=2)
#    (see ISLR Fig 3.3)
n        <- 100             # sample size
nsim     <- 500             # number of simulations 
hatbeta0 <- rep(NA, nsim)   # vector to store estimated beta_0 values
hatbeta1 <- rep(NA, nsim)   # vector to store estimated beta_1 values
x <- rnorm(n,0,1)           # generate x-values. 
                            # these are held fixed during the simulation

for(i in 1:nsim){
  eps <- rnorm(n,0,2)         # sample epsilon vector
  y <- 2 + 3*x + eps          # compute y vector
  fit <- lm(y~x)              # fit linear regression
  hatbeta0[i] <- fit$coef[1]  # store estimated intercept
  hatbeta1[i] <- fit$coef[2]  # store estimated slope
}

?abline   # help file for function abline()

# create plot:
# jpeg("RandomRegressionLines.jpg")
plot(0, pch='', xlim=c(-2,2), ylim=c(-5,8), xlab="X", ylab="Y") # create empty plot
for(i in 1:10){
   abline(a=hatbeta0[i],b=hatbeta1[i])   # draw line with intercept a and slope b
}
abline(a=2,b=3,col="red", lwd=2)         # "true" line 
abline(h=0,col="gray")               
abline(v=0,col="gray")
# dev.off()

# create histogram:
# jpeg("HistogramBeta0.jpg")
hist(hatbeta0, probability=T, main=expression(paste("Histogram of ", hat(beta)[0])), xlab=expression(hat(beta)[0]))
lines(density(hatbeta0))
abline(v=2, col="red", lwd=2)
# dev.off()

# jpeg("HistogramBeta1.jpg")
hist(hatbeta1, prob=T, main=expression(paste("Histogram of ", hat(beta)[1])), xlab=expression(hat(beta)[1]))
lines(density(hatbeta1))
abline(v=3, col="red", lwd=2)
# dev.off()

