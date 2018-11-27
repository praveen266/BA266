X = c(1.7,1.6,2.8,5.6,1.3,2.2, 1.3,1.1,3.2,1.5,5.2,4.6,5.8,3 )
Y = c(3.7,3.9,6.7,9.5,3.4,5.6,3.7,2.7,5.5,2.9,10.7,7.6,11.8,4.1 )
df1= data.frame(X,Y)
df1
head(df1)
library(gsheet)
area1= "https://docs.google.com/spreadsheets/d/1h7HU0X_Q4T5h5D1Q36qoK40Tplz94x_HZYHOJJC_edU/edit#gid=2023826519"
df2= as.data.frame(gsheet::gsheet2tbl(area1))
str(df2)
head(df2)
df= df2
str(df)
linearmodel= lm(Y ~ X, data=df)
linearmodel
plot(df$X, df$Y)
cor(df$X, df$Y)
cov(df$X, df$Y)
summary(linearmodel)
abline(X,Y)
abline(linearmodel, col='red')
new2= data.frame(X=c(1.5,2,3,4,5)) 
range(df$X)
range(df$Y)
p2sales= predict(linearmodel, newdata = new2)
p2sales
cbind(new2, p2sales)
summary(linearmodel)
head(df)
levels(linearmodel)
#Residuals
Y= 0.96 + 1.66*1.7
r= 3.782-3.7
r
plot(df$X, df$Y)
abline(linearmodel, col= 'red')
abline(v=1.7)
abline(h= 3.7)
abline(v= 1.7, h= c(3.7,3.782))
residuals(linearmodel)
cbind(resid(linearmodel))       
fitted(linearmodel)
fitted(linearmodel)- df$Y
plot(linearmodel)

head(women)
L= lm(weight ~ height, data= women)
L
summary(L)
ndata2 = data.frame(height= c(62.5,66.5))
ndata2
p2wt= predict(L, newdata = ndata2)
p2wt
p2wt= predict(L, newdata = ndata2, level = 0.95, interval = 'confidence')
p2wt
p2wt= predict(level= 0.95, newdata= ndata2, 'confidence', L)
p2wt= predict(level= 0.95, newdata= ndata2, interval= 'confidence',object= L)
p2wt


sales= c(4141,3842,3056,3519,4226, 4630,3507,3754, 5000,5120,4011, 5015,1916,675, 3636,3224,2295, 2730,2618,4421, 4113,3746, 3532, 3825,1096, 761,2088,820,2114, 1882,2159,1602,3354,2927)
price = c(59,59,59,59,59,59,59,59,59,59,59,59, 79,79,79,79,79,79,79,79,79, 79,79,79,99,99, 99,99,99,99,99,99,99,99)
promotion= c(200,200,200,200,400,400,400,400, 600,600,600,600,200,200,200,200, 400,400,400,400,600,600,600,600, 200,200,200,200,400,400,400,400,600,600)
omni1= data.frame(sales, price, promotion)
omni1
head(omni1)
ML= lm(sales ~ price + promotion, data= omni1)
ML
summary(ML)
# without intercept
ML1= lm(sales ~ price + promotion - 1, data= omni1)
ML1
summary(ML1)
range(omni1$price)
range(omni1$sales)
range(omni1$promotion)
range(omni1$sales,  omni1$price , omni1$promotion)
dim(omni1)
sdata= omni1 %>% sample_n(2)
sdata
library(dplyr)
ndata3= data.frame(price, promotion)
ndata3
cbind(sdata, predict(ML, newdata = sdata))
plot(ML)
plot(ML, which = 4)
omni1[14,]
sdata
omni1[14,]
omni1[,3]
ML1= lm(sales ~ price + promotion, data= omni1[-14,])
summary(ML1)
AIC(ML)
AIC(ML1)
ML2= lm(sales ~ price + promotion, data= omni1[-14,-15,])
ML2
summary(ML2)
AIC(ML2)

library(olsrr)
head(mtcars)
model= lm(mpg ~ disp + hp+ wt + qsec, data= mtcars)
summary(model)
k= ols_step_all_possible(model)
k
