employee = read.csv("/home/pramiti/R_codes/kaggle/amazon_access_problem/train.csv", header=TRUE)

summary(employee$ROLE_ROLLUP_1)

nrow(prop.table(table(employee$ROLE_ROLLUP_2, employee$ACTION)))