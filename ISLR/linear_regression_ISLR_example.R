library(MASS)
library(ISLR)

fix(Boston)
names(Boston)
View(Boston)
lm.fit = lm(medv~lstat, data = Boston)

#if we attach Boston then lm know the data is from Boston
attach(Boston)
lm.fit = lm(medv~lstat)

lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
#to know confidence of the interval
confint(lm.fit)

#predict fn can be used to predict confidence intervals and prediction intervals for the prediction of medv for a given of lstat

predict(lm.fit, data.frame(lstat = c(5, 10, 15)), interval = "confidence")
predict(lm.fit, data.frame(lstat = c(5, 10, 15)), interval = "prediction")
par (mar = rep(0.5, 0.5))
plot(lstat, medv)
#abline fn can be used to draw any line, not just the least square regression line. 
#draw a line with intercept a and slope b
#lwd causes to increase line by width 3

abline(lm.fit, lwd = 3)
abline(lm.fit, lwd = 3, col = "red")
plot(lstat, medv, col = "red")
plot(lstat, medv, pch = 20)
plot(lstat, medv, pch = "+")
plot(1:20, 1:20, pch= 1:20)

#to plot all 4 at one time
#use par fn 
par(mfrow=c(10,2))
plot(lm.fit)

#compute resudulas using fn residuals()
#rstudent will return studentized resudulas
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))


#leverage statisctics can be computed using hatvalues
plot(hatvalues(lm.fit))
#identifies the index of largest element in vector
which.max(hatvalues(lm.fit))

#################multiple LR ############
lm.fit = lm(medv~lstat+age, data = Boston)
summary(lm.fit)$r.sq
summary(lm.fit)$sigma



library(car)
vif(lm.fit)
lm.fit1 = update(lm.fit, ~.-age)

######### interaction terms ###############
summary(lm(medv~lstat*age, data = Boston))


####### non linear transformations of the prediictors #################
lm.fit2 = lm (medv~lstat + I(lstat^2))
summary(lm.fit2)
#using the annova fn u can quantify the extent to which the quadratic fit is superior to the linear fit
annova(lm.fit, lm.fit2)

# to fit 5th order poly use
lm.fit5 = lm(medv~ploy(lstat, 5))
summary(lm.fit5)



############# Quant predictors ##################
fix ( Carseats )
names ( Carseats )

lm.fit = lm ( Sales~.+ Income : Advertising + Price : Age , data = Carseats )
summary ( lm.fit )

#The contrasts() function returns the coding that R uses for the dummy
#covariables.
attach ( Carseats )
contrasts ( ShelveLoc )


















