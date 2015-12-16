install.packages('plyr')
library(plyr)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(RColorBrewer)
library(rpart)
library(randomForest)

employee = read.csv("/home/pramiti/R_codes/kaggle/amazon_access_problem/train.csv", header=TRUE)
emp_test = read.csv("/home/pramiti/R_codes/kaggle/amazon_access_problem/test.csv", header= TRUE)
View(employee)
View(emp_test)
set.seed(415)

#Instead of specifying method=”class” as with rpart, we force the model to predict our classification by temporarily changing our target variable to a factor with only two levels using as.factor(). The importance=TRUE argument allows us to inspect variable importance as we’ll see, and the ntree argument specifies how many trees we want to grow.
fit <- randomForest(as.factor(ACTION) ~ RESOURCE + MGR_ID + ROLE_ROLLUP_1 + ROLE_ROLLUP_2 + ROLE_DEPTNAME + ROLE_TITLE + ROLE_FAMILY_DESC + ROLE_FAMILY + ROLE_CODE , data=employee, importance=TRUE, ntree=2000)
fit1 <- randomForest(as.factor(ACTION) ~ RESOURCE + MGR_ID + ROLE_ROLLUP_1 + ROLE_ROLLUP_2 + ROLE_DEPTNAME + ROLE_TITLE + ROLE_FAMILY_DESC + ROLE_FAMILY , data=employee, importance=TRUE, ntree=2000)
fit2 <- randomForest(as.factor(ACTION) ~ RESOURCE + MGR_ID + ROLE_ROLLUP_1 + ROLE_ROLLUP_2 + ROLE_DEPTNAME + ROLE_CODE + ROLE_FAMILY_DESC + ROLE_FAMILY , data=employee, importance=TRUE, ntree=2000)
#fit3 <- randomForest(as.factor(ACTION) ~ RESOURCE + MGR_ID + ROLE_ROLLUP_1 + ROLE_ROLLUP_2 + ROLE_DEPTNAME + ROLE_CODE + 2 * ROLE_CODE + ROLE_FAMILY_DESC + ROLE_FAMILY , data=employee, importance=TRUE, ntree=2000)

as.factor(employee$RESOURCE) + as.factor(employee$MGR_ID)

#So let’s look at what variables were important:
varImpPlot(fit)
varImpPlot(fit1)
varImpPlot(fit2)
Prediction <- predict(fit, emp_test)
mean((fit-emp_test)^2)
Prediction <- predict(fit1, emp_test)
Prediction <- predict(fit2, emp_test)
submit <- data.frame(ID = emp_test$id, ACTION = Prediction)
View(submit)
write.csv(submit, file = "randomforest_removing_roletitle_amazon_employee_access.csv", row.names = FALSE)



