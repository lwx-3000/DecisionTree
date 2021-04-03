setwd('/Users/wenxionglu/Documents/GraduateCourse/6340 Stats/proj3')
db<-read.csv('diabetes.csv',header=TRUE)

db$Outcome<-as.factor(db$Outcome)
attach(db)
is.factor(Outcome)
library(PerformanceAnalytics)
library(MASS)
library(lmtest)
library(car)
library(ISLR)
library(boot)
library(caret)
library(e1071)
set.seed(1)
#1 
#(a)
chart.Correlation(db) #matrix of scatterplot
dev.off()
# (b)
mod.1 = glm(Outcome~., data = db,family = binomial)
summary(mod.1)
#For each predictor, fit a logistic regression model to predict
#the response. 
#(c)
mod.2<-update(mod.1,~.-SkinThickness..)
summary(mod.2)
confint(mod.2)
contrasts(Outcome)
prob<-predict(mod.2,type = 'response')
glm.pred=rep(0,2000)
glm.pred[prob>0.5]=1

sum(glm.pred!=Outcome)/2000 #the training error rate


#2.
#(a)
summary(mod.1)
prob2<-predict(mod.1, type='response',data=db)
glm.pred2<-rep(0,2000)
glm.pred2[prob2>0.5]=1
table(glm.pred2,Outcome)
sum(glm.pred2!=Outcome)/2000 #  the trainig error rate

388/(388+136) #sensitivity
1180/(1180+296) #specificity
#(b)
n<-length(Outcome)
#h <- as.data.frame(hatvalues(mod.1))
Q<-rep(0,n)
tfs<-glm.pred2!=Outcome
Q[tfs==TRUE]=1
cv1<-(sum(Q))/n
cv1
#(c)
#cost <- function(r, pi=0) mean(abs(r-pi) > 0.5)
#cv2<-cv.glm(db,mod.1,cost=cost)
#cv2$delta
#use caret

cv2 = train(as.factor(Outcome) ~ .,
            db, method="glm", family='binomial',metric="Accuracy",
            trControl = trainControl(method = "LOOCV"))
err2<-1-cv2$results$Accuracy

err2
#(d)
#cv3<-cv.glm(db,mod.2,cost=cost)
#cv3$delta
cv3 = train(Outcome ~ .-SkinThickness..,
            data=db, method="glm", family='binomial',metric='Accuracy',
            trControl = trainControl(method = "LOOCV"))
err3<-1-cv3$results$Accuracy
err3
#(e,f)
cv4 = train(as.factor(Outcome) ~ .,
            data=db, method="lda",
                trControl = trainControl(method = "loocv"),prior=c(0.658,0.342))
err4<-1-cv4$results$Accuracy #error rate of loocv
err4
cv5 = train(as.factor(Outcome) ~ .,
            data=db, method="qda",
            trControl = trainControl(method = "loocv"),prior=c(0.658,0.342))
err5<-1-cv5$results$Accuracy #error rate of loocv
err5
#(g)
cv6 = train(as.factor(Outcome) ~ .,
            data=db, method="knn",
            trControl = trainControl(method = "loocv"),prior=c(0.658,0.342))
err6<-1-cv6$results$Accuracy #error rate of loocv
err6 #for k = 5, 6, 9
#(h)
#recommend knn with k=6 in this case

#3.
#(a)
os<-read.table('oxygen_saturation.txt',header=TRUE)
attach(os)
D<-abs(pos-osm)
x<-plot(pos,osm)
abline(a=0,b=1,col='blue')
boxplot(D)


#(b)


#(c)
sD<-sort(D) #sorting and the absolute value of Difference
round(length(sD)/(1/0.9)) #90% is at idx 65
theta<-sD[round(length(sD)*0.9)]
theta
#(d)
#myboot<-function(pos,osm,n,rep){
#  i=1
#  set_thetahat<-c()
#  repeat {
#    if (i==rep){return(set_thetahat)}
#    pos_set<-sort(sample.int(pos,n,replace = TRUE),decreasing = TRUE)
#    osm_set<-sort(sample(osm,n,replace = TRUE),decreasing = TRUE)
#    sd<-sort(abs(pos_set-osm_set))
#    set_thetahat<-c(set_thetahat,sd[65])
#    i=i+1
#    }
#}

set.seed(1)
#my bootfunction for getting mean
mbs<-function(sD,n){
  x<-sD[n]
  return(sort(x))
}

mset<-mbs(sD,sample(72,1000,replace = TRUE))
x<-mset[round(length(mset)*0.9)]
x
bias<-2-x
bias
se<-sqrt(1/(1000-1)*sum((x-mset)^2))
se
upp<-x+(qt(0.025,df=100-1,lower.tail = FALSE)*se) #upper confidence bound
upp

#(e)
mb<-function(sD,n){
  x<-sD[n]
  return(sort(x)[round(length(x)*0.9)])
}
mb(sD,sample(72,1000,replace = TRUE))

boot(sD,mb,R=1000)
#(f)

