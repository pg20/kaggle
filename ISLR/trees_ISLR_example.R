
library (tree)
#tree library used to construct classification and regression tree
library(ISLR)
attach(Carseats)


High = ifelse(Sales <= 8, "No", "Yes")
Carseats = data.frame(Carseats, High)

#use tree function to fit a  ##########classification tree ##############in order to predict High using all varaibles but Sales
#syntax of tree is similar to km function

tree.carseats=tree(High~.-Sales, Carseats)
#summary fn list the variables that are used as internal nodes in the tree, number of terminal nodes, and the training error rate.
summary(tree.carseats)

#for classification trees, the deviance reported in the output of summary is given by 
#-2 sigma_m(sigma_k(n(mk) log p(mk) ))
#where nmk is the number of observations in the mth terminal node that belong to the kth class. 
#a small deviance indicates a tree that provides a good fit to the training data. 
#residual mean deviance reported is simply the deviance divided by n - |t0|, which is this case (400-27) = 373


#plottin argument pretty = 0 instructs R to include the category names for any qualitative predictors, rather than simply displaying a letter for each category. 
par(mar = rep(2, 4))
plot(tree.carseats)
text(tree.carseats, pretty = 0)

#just print the tree object, R prints output corresponding to each branch of the tree. 
tree.carseats

#inorder to properly evaluate the performance of a classification tree on these data, we must estithe test error rather than simply computing the training error. 
#we split the observations into a training set and test set, build the tree using the training set, evaluate the performance on the test data. 
#predict() fn can be used for this purpose
###in case of classification tree, the argument type = "class" instrcuts R to return the actual class prediction
#This approach lead to correct predictions around 71/5% of the loactions in the test data set.

set.seed(2)
train=sample(1:nrow(Carseats), 200)
Carseats.test = Carseats[-train,]
High.test = High[-train]
tree.carseats = tree(High~.-Sales, Carseats, subset = train)
tree.pred = predict(tree.carseats, Carseats.test, type="class")
table(tree.pred, High.test)

##########################CROSS VALIDATION ##########################################################
#Next we conside whether the pruning of the tree might lead to improved results.
#fn cv.tree performs cross validation in order to determine the optimal level of tree complexity; cost coplexity pruning is used in order to sleect a sequence of tree for considerations. 
#we use argument FUN=prune.misclass in oreder to indicate that we want the classification error rate to guide the cross validation and pruning process, rather than default for the cv.tree fn, which is deviance.
#the cv.tree() fn reports the no of terminal nodes of each tree considered (size) as well as the corresponding error rate and the value of the cost complexity PARAMENTER USED. 

set.seed(3)
cv.carseats = cv.tree(tree.carseats, FUN=prune.misclass)
names(cv.carseats)
cv.carseats




#dev corresponds to cross validation error rate in thhis instance.
#the tree with 9 terminal nodes results in the lowest cross validation error rate, with 50 cross validation errors. 
#WE PLOT ERROR RATE as a FUNCTION OF BOTH SIZE AND K

par(mfrow=c(1,2))
par(mar=rep(2, 4))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

########we now apply the prune.misclass fn in order to prune the tree to obtain the 9 node tree
prune.carseats = prune.misclass(tree.carseats, best = 9)
plot(prune.carseats)
text(prune.carseats, pretty= 0)

#How well does this pruned perform on the test data set. apply predict fn

tree.pred = predict(prune.carseats, Carseats.test, type= "class")
table(tree.pred, High.test)
(107 + 77)/200

#if we increase the value of best, we obtain a larger pruned tree with lower classification accuracy

prune.carseats = prune.misclass(tree.carseats, best = 15)
par (mar = rep(1, 1))
plot(prune.carseats)
text(prune.carseats, pretty = 0)
tree.pred = predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
(100+77)/200

############################FITTING THE REGRESSION TREEEEEE ########################################################

library(MASS)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston = tree(medv~., Boston, subset = train)
summary(tree.boston)

#output of summary indicates that only three of the variables have been used in  constructing tree. 
#In the context of a regression tree, the deviance is simply the sum of squared errors for the tree. 

par (mar = rep(2, 1))
plot(tree.boston)
text(tree.boston, pretty = 0)

#the variable lstat measures the percentage of individuals with lower socioeconomic status. 
#tree indicates the lower the lstat correspond to more expensive houses. 
#tree predicts a median house prices of $46, 400 for larger homes in suburbs in which residents have high socioeconomic status.


########################check if the prunung tree will improve performance ######################
cv.boston = cv.tree(tree.boston)
cv.boston
plot(cv.boston$size, cv.boston$dev, type = 'b')

#in this case the most complex tree is selected by cross validation. However, if we wish to prune the tree, we could do as follows, using the prune.tree() fn 

prune.boston = prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)

#in keeping the cross validation results, we use the unpruned tree to make predictions on the test set. 
yhat = predict(tree.boston, newdata = Boston[-train, ])
boston.test = Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0, 1)
mean((yhat-boston.test)^2)
#in other words, the test MSE associated with the regression tree is 25.05. The square root of the MSE is therefore around 5.005, indicating that this model leads to test predictions that are within around $5, 005 of the true median home value for the suburb.





###################BAGGING AND RANDOM FOREST ####################################################################################################

#bagging is a special case of random forest with m = p
#therefore, randomforest() fn can be useid to preform both random forest and bagging as follows:

library(randomForest)
set.seed(1)
bag.boston = randomForest(medv~., data = Boston, subset= train, mtry = 13, importance = TRUE)
bag.boston

yhat.bag = predict(bag.boston, newdata = Boston[-train, ])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)
#test mSE associated with bagged regression tree is 13.16, almost half that obtained using an optimally pruned single trees. 


#Growing a random Gorest processs in exactly the same way, except with smaller value of mtry argument
#by default, randomforest() uses p/3 variables when building a random forest of regression trees, and sqrt(p) variables when building a randon forest of classification trees. 
#use mtry = 6

set.seed(1)
rf.boston = randomForest(medv~., data = Boston, subset = train, mtry =6, importance = TRUE)
yhat.rf = predict(rf.boston, newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)

importance(rf.boston)
#two measure variable importance are reported. The former is based upon the mean decrease of accuracy in predictions on the out of bag samples when a given variables is excluded from the model. 
#the latter is a measure of the total decrease in node impurity that results from splits over that variable, averaged over all trees. 
#in case of regreesion ntree, the node impurity is measured by the training RSS and for classification tress by the deviance. 

varImpPlot(rf.boston)


########################################### BOOSTING #########################################################################################################

#use gbm package with gbm() fn, to fit boosted regression tree to the boston data set. 
#run gbm with option distribution = "gaussian" snce this is a regression problem; if its were a binary classification proble, use distribution="bernoulli".
#argument n.trees=5000 indicates that we want 5000 trees, and the option interaction.depth=4 limits the depth of each tree. 

library(gbm)
set.seed(1)
boost.boston = gbm(medv~., data = Boston[train, ], distribution = "gaussian", n.trees = 5000, interaction.depth=4)
summary(boost.boston)

#we see that lstat and rm are by far the most imp vairables. We can also produce parital dependance plots for there 2 variables.
#these plots illustrate the marginal effect of the selected variables on the response after integrating out the variables. 
#median house prices are increasing with rm and decreasing with lstat. 


par (mfrow = c(1,2))
plot(boost.boston, i = "rm")
plot(boost.boston, i = "lstat")

#we now boosteed model to predict medv on the test set

yhat.boost = predict(boost.boston, newdata = Boston[-train,], n.trees = 5000)
mean((yhat.boost-boston.test)^2)

#The test MSE for random forests and superior to that for bagging
#if we want we can perform boosting with a diff value of shrinkage parameter lambda
#default valeu is 0.001, but can be modified. 

boost.boston = gbm ( medv~. , data = Boston [ train ,] , distribution = "gaussian " , n.trees =5000 , interaction.depth=4 , shrinkage =0.2 ,verbose = F )
yhat.boost = predict ( boost.boston , newdata = Boston [ - train ,] , n.trees =5000)
mean (( yhat.boost - boston.test ) ^2)














