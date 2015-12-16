library(randomForest)
library(gbm)

#foor Auc
require(verification)
?roc.area

employee = read.csv("/home/pramiti/R_codes/kaggle/amazon_access_problem/train.csv", header=TRUE)
emp_test = read.csv("/home/pramiti/R_codes/kaggle/amazon_access_problem/test.csv", header= TRUE)
View(employee)
View(emp_test)

#for unbalanced datasets, the most "useful" measure is AUC
mean(employee$ACTION)

###########partition of data ###################
#there's a function in plyr that will do this, but it's easy to do your own
#for k-fold CV, you create k different partitions in the data
#I'm assuming that my data are already in a random order
nrow(employee)
k = 10
n = floor(nrow(employee)/k) #n is the size of each fold, rounded down so as to avaoid going out of bounds on the last fold
err.vect = rep(NA, k) #store the error in this vector


###################################################################
#partition the first fold
i = 1
s1 = ((i -1)  * n + 1)
s2 = (i * n)
subset = s1:s2
#because of rounding, the end of the subset may be slightly be out of range

#to do "standard" CV, we could just run the model on the cv.train data
#and test it on the cv.test data
#k-fold CV allows us to use all of the data for the final model
#but still have realistic model performance estimates 

cv.train = train[-subset,]
cv.test = train[subset,]


#to do "standard" CV, we could just run the model on the cv.train data
#and test it on the cv.test data
#k-fold CV allows us to use all of the data for the final model
#but still have realistic model performance estimates 

#next, move to the second fold:
i = 2
#...
##############################################################

########## CV for random forest #######################################

for (i in 1:k) {
  
  s1 = ((i -1) * n + 1)
  s2 = (i * n)
  subset = s1:s2
  
  cv.train = employee[-subset,]
  cv.test = employee[subset,]
  
  #takes all rows and columns except first column
  empfit = randomForest(x = cv.train[,-1], y = as.factor(cv.train[,1]))
  empprediction = predict(empfit, newdata = cv.test[,-1], type = "prob")[,2]
  #calculate the models accuracy for the ith fold
  err.vect[i] = roc.area(cv.test[,1], empprediction)$A
  print(paste("auc for fold", i, ":", err.vect[i]))
  
}
print(paste("average auc:", mean(err.vect)))
#each fold has a different error rate, and that why you do k-fold CV
###################################CV for gbm () ####################

#default is only 100
ntrees = 1000

for (i in 1:k) {
  s1 = ((i - 1) * n + 1)
  s2 = i * n
  subset = s1:s2 
  cv.train = employee[-subset,]
  cv.test = employee[subset,]
  
  gbmfit = gbm.fit(x = cv.train[,-1], y = cv.train[,1], n.trees = ntrees, verbose = FALSE, shrinkage = 0.005, interaction.depth = 20, n.minobsinnode = 5, distribution = "bernoulli")
  #use bernaulli or adaboost for classification problems
  
  gbm.prediction = predict(gbmfit, newdata = cv.test[,-1], n.trees = ntrees)
  err.vect[i] = roc.area(cv.test[,1], gbm.prediction)$A
  print(paste("AUC for fold", i, ":", err.vect[i]))
  
}
print (paste("avaergae auc:", mean(err.vect)))

#conclusion: a random forest is better for this data set! 
#(and for these parameters)
#only needed to change the model fit and prediction lines of the code!