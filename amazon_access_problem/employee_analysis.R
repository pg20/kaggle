install.packages('plyr')
library(plyr)

employee = read.csv("/home/pramiti/R_codes/kaggle/amazon_access_problem/train.csv", header=TRUE)
emp_test = read.csv("/home/pramiti/R_codes/kaggle/amazon_access_problem/test.csv", header=TRUE)

summary(employee$ROLE_ROLLUP_1)

nrow(prop.table(table(employee$RESOURCE, employee$ACTION)))


unique(employee$RESOURCE)
sapply(employee, FUN = function(x) {length(unique(x))})
length(unique(employee$MGR_ID))

#extract column names on which you want to group by
feature.colnames <- (colnames(employee))[-c(1:2)]
id.num <- 0
#it applies grouby on above colums, and allot a unique pid to that group
employee.pids <- ddply(employee, feature.colnames,
function(r) {
  id.num <<- id.num + 1 
cbind(PID=id.num, r) })
View(employee.pids)

#group by resource and count no of rows 
freq_resource = ddply(employee.pids, .(RESOURCE), nrow)
View(freq_resource)
stem(ddply(employee.pids, .(RESOURCE), nrow)$V1, width=40)

#how many resource have 1 row
sum(ddply(employee.pids, .(RESOURCE), nrow)$V1 ==1)
3766/7518

#The role attribute combinations (profiles)
length(unique(employee.pids$PID))

stem(ddply(employee.pids, .(PID), nrow)$V1, width=40)

sum(ddply(employee.pids, .(PID), nrow)$V1==1)
3336/9561

#check if all the elements group by resource 
all(ddply(employee.pids, .(RESOURCE), function(x) {max(table(x$PID))})$V1==1)


table(employee.pids$ACTION)

#no of resources having "all" action zero(for each resource)
sum(ddply(employee.pids, .(RESOURCE), function(x) {sum(x$ACTION)/nrow(x)})$V1==0)
sum(ddply(employee.pids, .(RESOURCE), function(x) {sum(x$ACTION)/nrow(x)})$V1==1)
6384/7518
#no of set of roles combination having "all" action as 1
sum(ddply(employee.pids, .(PID), function(x) {sum(x$ACTION)/nrow(x)})$V1==1)
8602/9561
sum(ddply(employee.pids, .(PID), function(x) {sum(x$ACTION)/nrow(x)})$V1==0)

#This shows that 95% of the outcomes were approvals, 84% of the resources had only approval outcomes, and 90% of the role profiles had only approval outcomes.


plot(employee.pids$PID, employee.pids$RESOURCE, col = as.factor(employee.pids$ACTION), xlab = "roles", ylab = "resource")
plot()

#############################################
install.packages('ROCR')
library(ROCR)

t<-data.frame(true_label=c(0,0,0,0,1,1,1,1), predict_1 = c(1,2,3,4,5,6,7,8),predict_2=c(1,2,3,6,5,4,7,8), predict_3=c(1,7,6,4,5,3,2,8))
table(t$predict_2 >=6, t$true_label)

pred <- prediction(t$predict_1, t$true_label)
performance(pred, "auc")@y.values[[1]]


require(verification)
roc.area(t$true_label, t$predict_1)$A
###########################################################
#to find unique values 
sapply(employee, function(x) {length(unique(x))})

#unique pair of role_codes
length(unique(paste(employee$ROLE_CODE, employee$ROLE_TITLE)))



#histogram
hist(employee$ROLE_FAMILY_DESC, breaks = 100)
hist(emp_test$ROLE_FAMILY_DESC, breaks = 100)


hist(employee$RESOURCE, breaks = 100)
hist(emp_test$RESOURCE, breaks = 100)

hist(employee$MGR_ID, breaks = 100)
hist(emp_test$MGR_ID, breaks = 100)

#to fill the top 2 frequent values in a column and rest by others
x = employee
for (i in 1:ncol(employee)) {
  the_labels <- names(sort(table(employee[, i]), decreasing = T)[1:2])
  x[!employee[, i] %in% the_labels, i] <- "other"
}
View(x)


#to use pearson chi-square test for vetgorical data
table(employee$ACTION, ifelse(employee$MGR_ID == 770, "mgr_770","mgr_not770" ))

chisq.test(employee$ACTION, ifelse(employee$MGR_ID==770, "mgr_770", "mgr_not_770"))


summary(employee$ROLE_DEPTNAME == 'NA')
summary(employee$ROLE_TITLE == 'NA')
summary(employee$ROLE_FAMILY_DESC == 'NA')
summary(employee$ROLE_FAMILY == 'NA')
summary(employee$ROLE_CODE == 'NA')
summary(employee$ROLE_DEPTNAME == '')
summary(employee$ROLE_TITLE == '')
summary(employee$ROLE_FAMILY_DESC == '')
summary(employee$ROLE_FAMILY == '')
summary(employee$ROLE_CODE == '')





