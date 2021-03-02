# Based on ISLR Chapter 8 Lab: Decision Trees

# Fitting Classification Trees

library(tree)
library(ISLR)

data(Carseats)

# we want to consider classification, so turn 
# sales into a binary variable. 
High=ifelse(Carseats$Sales<=8,"No","Yes")

# check results
cbind(Carseats$Sales, High)

# add new variable "High" to data frame:
Carseats = data.frame(Carseats,High)

# fit tree, using all variables except Sales
tree.carseats = tree(High~.-Sales, data=Carseats)

summary(tree.carseats)

# plot tree
par(mfrow=c(1,1))
plot(tree.carseats)
text(tree.carseats,pretty=0)
# at each split, there is a criterion stated. 
#   if the criterion is true -> left arm
#   if the criterion is false -> right arm
# the No/Yes in the leaf nodes are the predictions for observations 
#   falling in that leaf node. 

# describe tree in text
tree.carseats

# estimate test error (using validation set approach for simplicity)
set.seed(5)
train = sample(1:nrow(Carseats), 200)
Carseats.test = Carseats[-train,]
High.test = High[-train]

# fit on training data
tree.carseats = tree(High~.-Sales, Carseats, subset=train)
# predict for test data
?predict.tree
tree.pred = predict(tree.carseats, Carseats.test, type="class")
table(tree.pred,High.test)
# percentage of correct predictions:
(83+49)/200
# percentage of wrong predictions:
(32+36)/200  # note considerably larger misclassification rate 
             # than before. Overfitting?

# consider pruning
set.seed(142)
?cv.tree
cv.carseats = cv.tree(tree.carseats, FUN=prune.misclass)
names(cv.carseats)  # dev now means nr of misclassifications
                    # k is tuning parameter alpha in ISLR
cv.carseats

par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")

# look at some pruned trees:
?prune.tree
prune.10 <- prune.misclass(tree.carseats, best=10)
plot(prune.10)
text(prune.10)
prune.7 <- prune.misclass(tree.carseats, best=7)
plot(prune.7)
text(prune.7)
prune.4 <- prune.misclass(tree.carseats, best=4)
plot(prune.4)
text(prune.4)
prune.3 <- prune.tree(tree.carseats, best=3)
plot(prune.3)
text(prune.3)


# choose model with size 7:
prune.carseats=prune.misclass(tree.carseats,best=7)
par(mfrow=c(1,1))
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(100+39)/200
# note slightly better accuracy than before

##########################

# Fitting Regression Trees

library(MASS)
set.seed(1)
data(Boston)
?Boston

train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston = tree(medv~.,Boston,subset=train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston,pretty=0)

tree.boston

# consider pruning the tree
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')
# full size seems best in this case, no pruning needed

# if we still want a smaller tree, we can do this:
prune.boston=prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)

# predict using unpruned tree (b/c of cross
# validation results)
yhat=predict(tree.boston, newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)
# predictions are about sqrt(35) ~= 6 off
# medv is measured in $1000s
# => we predict median value of homes up to give or take $6000

