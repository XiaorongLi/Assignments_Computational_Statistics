############################
# Bagging and Random Forests 

# Rcode part 1

library(MASS)
library(randomForest)

dim(Boston) # 13 predictors
n <- dim(Boston)[1]

set.seed(3)

# define two different training/test set combinations, 
# to get an idea of the variance over different splits
train1 <- sample(1:n, n/2, replace=F)
train2 <- sample(1:n, n/2, replace=F)

# randomForest with mtry=13 yields bagged estimate. 
# we consider estimates for the two different training 
# data sets, with their OOB MSEs and test MSEs.

# split 1:
bag.boston1=randomForest(medv~.,data=Boston, subset=train1,
                        mtry=13,importance=TRUE)
bag.boston1 # given MSE is OOB MSE
# check by hand:
# bag.boston1$predicted gives predicted values based on 
#   out of bag samples
mean( (Boston$medv[train1]-bag.boston1$predicted)^2 )
# compute test MSE:
yhat.bag1 = predict(bag.boston1,newdata=Boston[-train1,])
mean( (yhat.bag1-Boston$medv[-train1])^2 ) # test MSE

# split 2:
bag.boston2=randomForest(medv~.,data=Boston, subset=train2,
                         mtry=13,importance=TRUE)
bag.boston2
yhat.bag2 = predict(bag.boston2,newdata=Boston[-train2,])
mean( (yhat.bag2-Boston$medv[-train2])^2 )


# R code part 2:

### bagging for classification ###

# simulation example to show that voting probabilities 
#   are not class probabilities
set.seed(11)
n <- 400
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
y <- rbinom(n, 1, .67)  # probablity of class 1 is 0.67, 
# regardless of x values
data <- data.frame(cbind(x1,x2,x3,y))

# voting does not give reliable estimate of class probs,
# expecially not if tree are not grown very deeply
rf <- randomForest( as.factor(y) ~ ., data=data, mtry=3, 
                    norm.votes=TRUE, maxnodes=5)
rf$vote

par(mfrow=c(1,2))
plot(rf$vote[,2], main="shallow trees, maj. vote", ylim=c(0,1))
abline(h=mean(rf$vote[,2]), col="orange", lwd=2)

# for deep trees, less of a problem:
rf <- randomForest( as.factor(y) ~ ., data=data, mtry=3, 
                    norm.votes=TRUE)
rf$vote
plot(rf$vote[,2], main="deep trees, maj. vote", ylim=c(0,1))
abline(h=mean(rf$vote[,2]), col="orange", lwd=2)

# we can aggregate class probabilities rather than class predictions, 
#   using the package ranger:
library(ranger)

# shallower trees
rf <- ranger( as.factor(y) ~. , data=data, mtry=3,
              probability=TRUE, min.node.size=80)
rf$predictions
plot(rf$predictions[,2], main="shallow trees, prob. method", ylim=c(0,1))
abline(h=mean(rf$predictions[,2]), col="orange", lwd=2)

rf <- ranger( as.factor(y) ~. , data=data, mtry=3, 
              probability=TRUE)
rf$predictions
plot(rf$predictions[,2], main="deep trees, prob. method", ylim=c(0,1))
abline(h=mean(rf$predictions[,2]), col="orange", lwd=2)

# Rcode part 3

# "real" random forest (use default value of mtry, which 
#   is p/3 for regression). We again consider the two 
#   different splits:
set.seed(1)

# split 1:
rf.boston1=randomForest(medv~.,data=Boston,subset=train1,
                       importance=TRUE)
rf.boston1  # OOB MSE
yhat.rf = predict(rf.boston1,newdata=Boston[-train1,])
mean((yhat.rf-Boston$medv[-train1])^2) # test MSE

# split 2:
rf.boston2=randomForest(medv~.,data=Boston,subset=train2,
                       importance=TRUE)
rf.boston2  # OOB MSE 
yhat.rf = predict(rf.boston2,newdata=Boston[-train2,])
mean((yhat.rf-Boston$medv[-train2])^2) # test MSE

# interpretation is more difficult than for trees, 
# but we do have measures of variable importance:
?importance.randomForest
varImpPlot(rf.boston1)
varImpPlot(rf.boston2)




