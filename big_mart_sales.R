train = read.csv("Train_big_mart.csv",header = T)
test = read.csv("test_big_mart.csv",header = T)

summary(train)
unique(train$Item_Type)
 
#convert factor into character for replacing with NA

train$Outlet_Size= as.character(train$Outlet_Size)
train$Outlet_Size[train$Outlet_Size == ""]= NA
train$Outlet_Size= as.factor(train$Outlet_Size)

test$Outlet_Size= as.character(test$Outlet_Size)
test$Outlet_Size[test$Outlet_Size == ""]= NA
test$Outlet_Size= as.factor(test$Outlet_Size)

  


#giving same name to same category items

unique(train1$Item_Fat_Content)
train$Item_Fat_Content = as.character(train$Item_Fat_Content)
train$Item_Fat_Content[train$Item_Fat_Content=="low fat"]="LF"
train$Item_Fat_Content[train$Item_Fat_Content=="Low Fat"]="LF"
train$Item_Fat_Content[train$Item_Fat_Content=="reg"]="Regular"
train$Item_Fat_Content = as.character(train$Item_Fat_Content)

test$Item_Fat_Content = as.character(test$Item_Fat_Content)
test$Item_Fat_Content[test$Item_Fat_Content=="low fat"]="LF"
test$Item_Fat_Content[test$Item_Fat_Content=="Low Fat"]="LF"
test$Item_Fat_Content[test$Item_Fat_Content=="reg"]="Regular"
test$Item_Fat_Content = as.character(train1$Item_Fat_Content)


#checking visibility data and visibility 0 is of no significance so can deal it as missing data
install.packages("dplyr")
library(dplyr)

train$Item_Visibility[train$Item_Visibility == "0"]= NA
train[train=="NA"]=NA



test$Item_Visibility[test$Item_Visibility == "0"]= NA
test[test=="NA"]=NA



# converting outlet establishment year data to no of years old data

train1 = train %>% mutate(Year_old=max(Outlet_Establishment_Year)-Outlet_Establishment_Year)

test1 = test %>% mutate(Year_old=max(Outlet_Establishment_Year)-Outlet_Establishment_Year)


#Finding missing values
install.packages("mice")
library(mice)
train1$Item_Fat_Content = as.factor(train1$Item_Fat_Content)
test1$Item_Fat_Content = as.factor(test1$Item_Fat_Content)

tempData_train <- mice(train1[,-c(1,7)],m=1,meth="cart" ,maxit=5,seed=500)

tempData_test <- mice(test1[,-c(1,7)],m=1,meth="cart" ,maxit=5,seed=500)


train.r = complete(tempData_train)
test.r = complete(tempData_test)
md.pattern(train.r)
par(mfrow = c(1,1))
stripplot(tempData, pch = 20, cex = 1.2)
xyplot(tempData_test,Item_Visibility ~ Outlet_Establishment_Year+Outlet_Location_Type+Outlet_Type,pch=18,cex=1)

install.packages("dplyr")
library(dplyr)
test.r["Item_Outlet_Sales"] = NA

#Randomforest for Predicting sales

#Fitting model on train data

install.packages("randomForest")
library(randomForest)


#d= sample(2,nrow(train.r),replace = T, prob = c(0.3, 0.7))
#train_test = train.r[d==1 , ]
#train_train = train.r[d==2 ,]
#train.r$Item_Fat_Content = as.factor(train.r$Item_Fat_Content)

fit = randomForest(Item_Outlet_Sales~.,data = train.r, ntree = 300 , importance = T , proximity = T)
pred = predict(fit , newdata = test.r)

answer1 = data.frame(Item_Identifier=test$Item_Identifier , Outlet_Identifier = test$Outlet_Identifier , Item_Outlet_Sales = pred)
write.csv(answer1 ,file = "answer1.csv", row.names = F)
