library(dplyr)
#Loading the data

data= read.csv(choose.files())
summary(data)
str(data)
#print the summary

data1= data
#Missing data

colSums(is.na(data1))
# Replacing missing data with mean values

data1$Taxi_dist[which(is.na(data1$Taxi_dist))]= mean(data1$Taxi_dist, na.rm = T)
summary(data1$Taxi_dist)
data1$Market_dist[which(is.na(data1$Market_dist))]= mean(data1$Market_dist, na.rm = T)
data1$Hospital_dist[which(is.na(data1$Hospital_dist))]= mean(data1$Hospital_dist, na.rm = T)
data1$Carpet_area[which(is.na(data1$Carpet_area))]= mean(data1$Carpet_area, na.rm = T)
data1$Builtup_area[which(is.na(data1$Builtup_area))]= mean(data1$Builtup_area, na.rm = T)
summary(data1)
#Data visualization for 

scatter.smooth(x= data1$Taxi_dist, y= data1$Price_house, main= "Taxi_dist ~ Price_house", col="red")
scatter.smooth(x= data1$Market_dist, y= data1$Price_house, main= "Market_dist ~ Price_house", col="red")
scatter.smooth(x= data1$Hospital_dist, y= data1$Price_house, main= "Hospital_dist ~ Price_house", col="red")
scatter.smooth(x= data1$Carpet_area, y= data1$Price_house, main= "Carpet_area ~ Price_house", col="red")
scatter.smooth(x= data1$Builtup_area, y= data1$Price_house, main= "Builtup_area ~ Price_house", col="red" )
scatter.smooth(x= data1$Parking_type, y= data1$Price_house, main= "Parking_type ~ Price_house", col="red" )
scatter.smooth(x= data1$City_type, y= data1$Price_house, main= "City_type ~ Price_house", col="red" )
scatter.smooth(x= data1$Rainfall, y= data1$Price_house, main= "Rainfall ~ Price_house", col="red" )
# Finding and removing row of having maximum price value

a=150000000
data2= data1
data2[data2$Price_house==150000000,]
data3= data2[-361,]
summary(data3)
#Again ploting

scatter.smooth(x= data3$Taxi_dist, y= data3$Price_house, main= "Taxi_dist ~ Price_house", col="red")
scatter.smooth(x= data3$Market_dist, y= data3$Price_house, main= "Market_dist ~ Price_house", col="red")
scatter.smooth(x= data3$Hospital_dist, y= data3$Price_house, main= "Hospital_dist ~ Price_house", col="red")
scatter.smooth(x= data3$Carpet_area, y= data3$Price_house, main= "Carpet_area ~ Price_house", col="red")
scatter.smooth(x= data3$Builtup_area, y= data3$Price_house, main= "Builtup_area ~ Price_house", col="red" )
scatter.smooth(x= data3$Parking_type, y= data3$Price_house, main= "Parking_type ~ Price_house", col="red" )
scatter.smooth(x= data3$City_type, y= data3$Price_house, main= "City_type ~ Price_house", col="red" )
scatter.smooth(x= data3$Rainfall, y= data3$Price_house, main= "Rainfall ~ Price_house", col="red" )
# Building the model

model1= lm(Price_house~., data=data3)
summary(model1)
sink("model1.txt")
print(summary(model1))
sink()
# Model after removing insignificant variables:

data4= subset(data3, select = -c(1,2,3,4,5,8))
str(data4)
model2= lm(Price_house~., data=data4)
summary(model2)
plot(model2)

model2$fitted.values
fitted(model2)
plot(data4$Parking_type,model2$fitted.values, xlab= "Parking Type", ylab= "Predicted price")
plot(data4$City_type,model2$fitted.values, xlab= "City Type", ylab= "Predicted price")
sink("model2.txt")
print(summary(model2))
sink()
#Predict the test data

test=read.csv(choose.files())
head(test)
summary(test)
salary.predict= predict(model2,test)
salary.predict
summary(salary.predict)
plot(salary.predict,col="red", pch=16)

Final= cbind(test,salary.predict)
edit(final)
View(final)
scatter.smooth(x= Final$Parking_type, y=Final$salary.predict, cex=1.2,main= "Predicted price", col="red")



