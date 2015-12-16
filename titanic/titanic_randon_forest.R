library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(RColorBrewer)
library(rpart)
library(randomForest)

#read data
tdata <- read.table("/home/pramiti/R_codes/kaggle/titanic/train.csv", header=TRUE, sep=",")
test <- read.table("/home/pramiti/R_codes/kaggle/titanic/test.csv", header= TRUE, sep = ",")
View(tdata)


sample(1:10, replace = TRUE)

#feature enginnering
#The initial suspects for gaining more machine learning mojo from are the three text fields that we never sent into our decision trees last time. While the ticket number, cabin, and name were all unique to each passenger; perhaps parts of those text strings could be extracted to build a new predictive attribute. 
tdata$Name[1]
test$Survived <- NA
combi <- rbind(tdata, test)

#strings are automatically imported as factors in R, even if it doesn’t make sense. So we need to cast this column back into a text string. To do this we use as.character.
combi$Name <- as.character(combi$Name)
combi$Name[1]
#we see that there is a comma right after the person’s last name, and a full stop after their title
strsplit(combi$Name[1], split='[,.]')
strsplit(combi$Name[1], split='[,.]')[[1]]
strsplit(combi$Name[1], split='[,.]')[[1]][2]

#. It runs through the rows of the vector of names, and sends each name to the function. The results of all these string splits are all combined up into a vector as output from the sapply function, which we then store to a new column in our original dataframe, called Title.
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})

#Finally, we may wish to strip off those spaces from the beginning of the titles. Here we can just substitute the first occurrence of a space with nothing. We can use sub for this (gsub would replace all spaces, poor ‘the Countess’ would look strange then though):
combi$Title <- sub(' ', '', combi$Title)
table(combi$Title)


combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

#Our final step is to change the variable type back to a factor, as these are essentially categories that we have created:
combi$Title <- factor(combi$Title)

combi$FamilySize <- combi$SibSp + combi$Parch + 1

#Combining the Surname with the family size though should remedy this concern. No two family-Johnson’s should have the same FamilySize variable on such a small ship. So let’s first extract the passengers’ last names. This should be a pretty simple change from the title extraction code we ran earlier, now we just want the first part of the strsplit output:
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

#We then want to append the FamilySize variable to the front of it, but as we saw with factors, string operations need strings. So let’s convert the FamilySize variable temporarily to a string and combine it with the Surname to get our new FamilyID variable:
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)

famIDs <- data.frame(table(combi$FamilyID))

#Here we see again all those naughty families that didn’t work well with our assumptions, so let’s subset this dataframe to show only those unexpectedly small FamilyID groups.
famIDs <- famIDs[famIDs$Freq <= 2,]

#We then need to overwrite any family IDs in our dataset for groups that were not correctly identified and finally convert it to a factor:
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

tdata <- combi[1:891,]
test <- combi[892:1309,]

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, method="class")


############RANDOM FOREST###################

#o let’s grow a tree on the subset of the data with the age values available, and then replace those that are missing:

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
View(combi)
summary(combi)
summary(combi$Embarked)
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)
summary(combi$Fare)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)
tdata <- combi[1:891,]
test <- combi[892:1309,]
#Because the process has the two sources of randomness that we discussed earlier, it is a good idea to set the random seed in R before you begin. This makes your results reproducible next time you load the code up, otherwise you can get different classifications for each run.
set.seed(415)

#Instead of specifying method=”class” as with rpart, we force the model to predict our classification by temporarily changing our target variable to a factor with only two levels using as.factor(). The importance=TRUE argument allows us to inspect variable importance as we’ll see, and the ntree argument specifies how many trees we want to grow.
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize +
                      FamilyID2, data=tdata, importance=TRUE, ntree=2000)

#So let’s look at what variables were important:
varImpPlot(fit)

Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)


######################### PARTY TREE ####################################

#But let’s not give up yet. There’s more than one ensemble model. Let’s try a forest of conditional inference trees. They make their decisions in slightly different ways, using a statistical test rather than a purity measure, but the basic construction of each tree is fairly similar. So go ahead and install and load the party package.
library(party)


set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = tdata, controls=cforest_unbiased(ntree=2000, mtry=3))

Prediction <- predict(fit, test, OOB=TRUE, type = "response")

