setwd("/Users/wenxionglu/Documents/GraduateCourse/6340 Stats/proj4")
wine<-read.table('wine.txt',header = TRUE)
db<-read.csv('diabetes.csv',header=TRUE)

library(caret)
library(e1071)
library(leaps)
library(glmnet)
library(bestglm)
set.seed(1)
attach(wine)
Region<-as.factor(Region)
dim(wine) 
sum(is.na(wine))#check missing value
attach(db)
Outcome<-as.factor(Outcome)
dim(db) 
sum(is.na(db))#check missing value

#1(A)
l1 = train(Quality~ .,data=wine, method="lm",trControl=trainControl(method='loocv'))
t1<-(l1$results$RMSE)^2  #mse 
t1

#(b)

regfit.full<-regsubsets(Quality~.,wine,method=c("exhaustive", "backward", "forward", "seqrep"))
regfull.summary<-summary(regfit.full)
regfull.summary
regfull.summary$adjr2 #best subset based on R2  (0.6801276 [4])
l2 = train(Quality~ .-Body-Region,data=wine,method='lm',trControl=trainControl(method='loocv'))
t2<-(l2$results$RMSE)^2 
t2

#(c)
regfit1<-regsubsets(Quality~.,wine,method='forward')
regfit1.summary<-summary(regfit1)
regfit1.summary
regfit1.summary$adjr2   #0.6801276
#test MSE
l3 = train(Quality~ .-Body-Region,data=wine,method='lm',trControl=trainControl(method='loocv'))
t3<-(l3$results$RMSE)^2 
t3
#(d)
regfit2<-regsubsets(Quality~.,wine,method='backward')
regfit2.summary<-summary(regfit1)
regfit2.summary
regfit2.summary$adjr2  #0.6801276
#test MSE
l4 = train(Quality~ .-Body-Region,data=wine,method='lm',trControl=trainControl(method='loocv'))
t4=(l4$results$RMSE)^2 
t4

#(e) use the best model we have found above
set.seed(1)
x=model.matrix(Quality~.-Body-Region,wine)[,-1]
y=wine$Quality

cv1.out=cv.glmnet(x,y,alpha=0,nfolds = length(y))
bestlam1=cv1.out$lambda.min #find min lambda

#Ridge regression
ridge.mod<-glmnet(x,y,alpha=0,lambda=bestlam1)

l5 = train(
  Quality~ .-Body-Region, data = wine, method = "glmnet",
  trControl = trainControl(method="loocv"),
  tuneGrid = expand.grid(alpha = 0, lambda = bestlam1))
t5=(l5$results$RMSE)^2  # test MSE
t5

#(f)
cv2.out=cv.glmnet(x,y,alpha=1,nfolds = length(y))
bestlam2=cv2.out$lambda.min #find min lambda

#Lasso regression
lasso.mod<-glmnet(x,y,alpha=1,lambda=bestlam1)

l6 = train(
  Quality~ .-Body-Region, data = wine, method = "glmnet",
  trControl = trainControl(method="loocv"),
  tuneGrid = expand.grid(alpha = 1, lambda = bestlam2))
t6=(l6$results$RMSE)^2  # test MSE
t6
#(g)
mse_all=as.data.frame(matrix(dat=c(t1,t2,t3,t4,t5,t6),nrow=1,byrow = TRUE))
names(mse_all)<-c('a','b','c','d','e','f')







#2(A)
l21 = train(as.factor(Outcome) ~ .,
                  db, method="glm", family='binomial',metric="Accuracy",
                  trControl = trainControl(method = "LOOCV"))
t21<-1-l21$results$Accuracy  #mse 
t21

#(b)
set.seed(1) #make data frame
x2=model.matrix(Outcome~.,db)[,-1]
y2=db$Outcome
Xy<-as.data.frame(cbind(x2,y2))
names(Xy)<-names(db)

bglm2<-bestglm(Xy,family = binomial, IC='AIC')
bglm2$BestModel


l22 = train(as.factor(Outcome) ~ .-SkinThickness..,
            db, method="glm", family='binomial',metric="Accuracy",
            trControl = trainControl(method = "CV",number=10))
t22<-1-l22$results$Accuracy  #test error rate
t22

#(c)
bglm3<-bestglm(Xy,family = binomial, IC='AIC', method = 'forward')
bglm3$BestModel

#model is the same as above 
l23 = train(as.factor(Outcome) ~ .-SkinThickness..,
            db, method="glm", family='binomial',metric="Accuracy",
            trControl = trainControl(method = "CV",number=10))
t23<-1-l23$results$Accuracy  #mse  
t23

#(d)
bglm4<-bestglm(Xy,family = binomial, IC='AIC', method = 'backward')
bglm4$BestModel

#model is the same as above 
l24 = train(as.factor(Outcome) ~ .-SkinThickness..,
            db, method="glm", family='binomial',metric="Accuracy",
            trControl = trainControl(method = "CV",number=10))
t24<-1-l24$results$Accuracy  #mse  
t24

#(e)

#Ridge regression
set.seed(1)

cv21.out=cv.glmnet(x2,y2,alpha=0,nfolds = length(y))
bestlam21=cv21.out$lambda.min #find min lambda
ridge.mod2<-glmnet(x,y,alpha=0,lambda=bestlam21)

l25 = train(
  Outcome~ .-Body-Region, data = wine, method = "glm",
  trControl = trainControl(method="cv",number=10),
  tuneGrid = expand.grid(alpha = 0, lambda = bestlam21))
t25=(l25$results$RMSE)^2  # test MSE
t25 = 0.23

#(f)
cv22.out=cv.glmnet(x,y,alpha=1,nfolds = length(y))
bestlam2=cv22.out$lambda.min #find min lambda

#Lasso regression
lasso.mod2<-glmnet(x,y,alpha=1,lambda=bestlam1)

l6 = train(
  Quality~ .-Body-Region, data = wine, method = "glm",
  trControl = trainControl(method="cv",number=10),
  tuneGrid = expand.grid(alpha = 1, lambda = bestlam22))
t26=(l6$results$RMSE)^2  # test MSE
t26
t26 = 0.221












