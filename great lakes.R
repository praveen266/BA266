library(dplyr)
data= read.csv(choose.files())
head(data)
str(data)
summary(data)
ncol(is.na(data))
nrow(is.na(data))
labels(data)
sum(is.na(data$Age))
sum(is.na(data))
p= function(x) {sum(is.na(x)/length(x)*100)}
apply(data,2,p)                               #2 is for column, 1 for row
md.pattern(data)

library(mice)

md.pattern(data)
nrow(data)
md.pairs(data)
marginplot(data)
library(VIM)
#Impute
data1= data[,-1]
str(data1)
data1$renewal= as.factor(data1$renewal)
impute= mice(data1, m=3, seed= 123)
print(impute)
impute$imp$HomeOwner
impute$imp$DollarsPerIssue
impute$imp$MonthsSince1stOrder
impute$imp$MonthsSinceLastPayment
impute$imp$MonthsSinceExpire
impute$imp$TotalPaidOrders
impute$imp$HouseholdSize
impute$imp$HomeValue
#complete data:

data2=complete(impute,1)
new_data1=complete(impute,2)
new_data2= complete(impute,3)
sum(is.na(data2))
summary(data2)

value=which(!complete.cases(data2))
data_2= data2[-value,]

sum(is.na(data_2))

#Distribution of observed/impute values:
stripplot(impute$imp$HomeOwner,pch=20, cex=1.2)
xyplot(impute, renewal~ HomeOwner | .imp, pch=20, cex=1.4)
#Data partition
str(data_2)
sum(is.na(data_2))
nrow(is.na(data_2))
set.seed(123)
ind1= sample(2, nrow(data_2), replace = T, prob = c(0.7,0.3))
train1= data_2[ind1==1,] 
test1= data_2[ind1==2,]
str(test1)
str(train1)
sum(is.na(train1))
sum(is.na(test1))
summary(train1)
nrow(is.na(train1))
ncol(is.na(train1))

#random forest:

library(randomForest)
set.seed(222)
rf36= randomForest(renewal~., data=train1)
rf36
attributes(rf38)
rf36$confusion

#Prediction & Confusion matrix- train data
library(caret)
p1= predict(rf36, train1)
head(p1)
head(train1$renewal)
confusionMatrix(p1, train1$renewal)

#Prediction & Confusion matrix- test data
test2= test1[,-37]
str(test2)
p2= predict(rf36, test2)
head(p2)
#Error rate of random forest:

plot(rf36)
#tune mtry

tuneRF(train1[,-37], train1[,37], stepFactor = 0.5,
       plot= T, ntreeTry = 300, trace = T, improve = 0.5)
# Final testing

final_test= read.csv(choose.files())
str(final_test)
final_test1= final_test[,-1]
str(final_test1)
sum(is.na(final_test1))
summary(final_test1)
impute1= mice(final_test1, m=3, seed= 123)
final_test2=complete(impute1,1)
sum(is.na(final_test2))
final_predict= predict(rf36, final_test2)

View(final_predict)
Final= cbind(final_test,final_predict)
str(Final)
Final_sol1= Final[,-(2:37)]
str(Final_sol1)
View(Final_sol1)
write.csv(Final_sol1, file= 'solution.csv')
#Extra

levels(test3$MagazineStatus)= levels(final_test3$MagazineStatus)

data3= data2
data2$MagazineStatus= factor(data2$MagazineStatus, levels = levels(final_test2$MagazineStatus))
levels(data2$MagazineStatus)
levels(final_test2$MagazineStatus)
