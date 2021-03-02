##################################################

# Boosting (does not belong to exam material)

library(gbm)
set.seed(1)
boost.boston=gbm(medv~.,data=Boston[train1,],
                 distribution="gaussian",n.trees=5000,
                 interaction.depth=4)
summary(boost.boston)

# partial dependence plots of important variables, 
# showing "effect" of a single variable
# (similar to what we had for gams):
par(mfrow=c(1,2))
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")

# predict on test data:
yhat.boost=predict(boost.boston,newdata=Boston[-train1,],
                   n.trees=5000)
mean((yhat.boost-Boston$medv[-train1])^2)

# use different shrinkage parameter:
boost.boston=gbm(medv~.,data=Boston[train1,],
                 distribution="gaussian", n.trees=5000,
                 interaction.depth=4,shrinkage=0.1,
                 verbose=F)
yhat.boost=predict(boost.boston,newdata=Boston[-train1,],
                   n.trees=5000)
mean((yhat.boost-Boston$medv[-train1])^2)

# or change number of trees
boost.boston=gbm(medv~.,data=Boston[train1,],distribution="gaussian",
                 n.trees=50000,interaction.depth=4,verbose=F)
yhat.boost=predict(boost.boston,newdata=Boston[-train1,],n.trees=50000)
mean((yhat.boost- Boston$medv[-train1])^2)

# be careful that boosting can overfit
#    if the number of trees is too large.

