library(ISLR)


#what should be the seed value. Does the seed value effects the result ? 
set.seed(1)
train = sample(392, 196)
train
View(Auto)
lm.fit = lm(mpg ~ horsepower, data = Auto, subset = train)


#-train takes observation which are not in train set

attach(Auto)
mean((mpg-predict(lm.fit, Auto))[-train]^2)


lm.fit2 = lm(mpg~poly(horsepower, 2), data=Auto, subset=train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)

lm.fit3 = lm(mpg~poly(horsepower, 3), data=Auto, subset=train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)


set.seed(2)
train= sample(392, 196)
lm.fit = lm(mpg~horsepower, subset=train)
mean((mpg-predict(lm.fit, Auto))[-train]^2)

lm.fit2 = lm(mpg~poly(horsepower, 2), data = Auto, subset = train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2 )

lm.fit3 = lm(mpg~poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2 )

#shows that quadratic is better than linear function but liitle evidence in favor of cubic fn


####################Leave One Out Cross Validation ##################################
#generalised linear model if glm() fits models without passing family then by default it takes linear regression just like lm()

glm.fit = glm(mpg~horsepower, data=Auto)
coef(glm.fit)

lm.fit = lm(mpg~horsepower, data=Auto)
coef(lm.fit)

#yields idenctical linear regression models
#used glm instead of lm beacuse we can used together with  cv.glm . It is part of boot library

library(boot)
glm.fit=glm(mpg~horsepower, data=Auto)

##cv.glm() fn produces a list with several components. The 2 numbers in the delta vector contain the cross validation results.
#In this case the numbers are indentical (up to two decimal places) and correspong to LOOCV statistic
cv.err = cv.glm(Auto, glm.fit)
cv.err$delta


#repeat procedure for increasingly complex ploynomial fits. 
#To automate the process, we use the for fn to initiate the for loop which iteratively fits the poly reg for poly of order i = 1 to i = 5, computes the associated cross validation error, and stores it in the ith element of the vector cv.error.

cv.error=rep(0, 5)
for(i in 1:5) {
  glm.fit = glm(mpg~poly(horsepower, i), data=Auto)
  cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}
cv.error

#We see a sharp drop in the estimated test MSE between linear and quadratic fits, but no clear improvement in higher order polynomials

#################### K-fold cross Validation ###########################################

#cv.glm() can also be user to implemnt the k-fold CV
#used k = 10

set.seed(17)
cv.error.10=rep(0,10)
for(i in 1:10) {
  glm.fit = glm(mpg~poly(horsepower, i), data=Auto)
  cv.error.10[i]=cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10

#computation time is much shorter than that of LOOCV
#in principle, the compuation time for LOOCV for a least sqaure linear model should be faster than for k-fold CV, due to the availability of the formula for LOOCV; however unfortunately the cv.glm() function doesnot make use of this formula)
#we see little evidence that using cubic or high order poly terms lead to lower test error than simply using a quadratic fit.

#NOTE: the the two numbers associated with delta are essentially the same when LOOCV performed. In k-fold CV, the two numbers with delta differ slightly. The first is the standard k-fold CV estimate. 
#second is a bias corrected version. The two estimates are very similar to each other. 


