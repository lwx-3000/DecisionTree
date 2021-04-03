getwd()
setwd("/Users/wenxionglu/Documents/GraduateCourse/6340 Stats/proj1")
test=read.csv("1-test_data.csv",header=T)
train = read.csv("1-training_data.csv",header=T)
library(class)
#drop unnecessary columns
test.X = test[,-3]
test.Y = test[,3]
train.X = train[,-3]
train.Y = train[,3]
##test.Y<-sapply(test.Y,function(x) if(x=='yes') x<-1 else x<-0)
##train.Y<-sapply(train.Y,function(x) if(x=='yes') x<-1 else x<-0)

Kset=seq(1,200,by=5)
Kset
#Tuning knn k parameter
                          #Training Error
Nset=rep(0,200)                   # declaration to initiate for loop
Nset
set.seed(1)
for (i in Kset){ 
  knn.mod <-  knn(train.X, train.X, cl=train.Y, k=i)
  Nset[i] <- sum(train.Y == knn.mod)/NROW(train.Y)
  k=i  
  cat(k,'=',Nset[k],'\n')       # to print % accuracy 
}
#The optimal K and its Error rate
1-max(Nset)
match(max(Nset[-1]),Nset)
#Plot max Error Rate
plot(Kset,1-Nset[Kset], xlab="K- Value",ylab="Error Rate")  # to plot % accuracy with respect to k-value
table(knn.mod,train.Y)
mean(train.Y!=knn.mod)
                          #Test Error
set.seed(1)
Mset=rep(0,200)
for(i in Kset){
  knn.mod2 <- knn(train.X,test.X,cl=train.Y, k = i)
  Mset[i] <- sum(test.Y == knn.mod2)/NROW(test.Y)
  k=i
  cat(k, '=', Mset[k],'\n')
}
#The optimal K and its Error rate
1-max(Mset)
match(max(Mset[-1]),Mset)
#Plot Test Error Rate
plot(Kset,1-Mset[Kset], xlab="K- Value",ylab="Error Rate")
table(knn.mod2,test.Y)
mean(test.Y!=knn.mod2)


#Plot both error rates
plot(Kset, 1-Nset[Kset], 
     main = 'Training and Test Error Rates',
     ylab = 'Error Rate', xlab= 'K-value',
     type = 'b', col = 'red',pch = 20,ylim = range(c(1-Nset[Kset],1-Mset[Kset])))
lines( Kset,1-Mset[Kset], col= 'blue',pch=20)
legend('bottomright',
      c("Train Error","Test Error"),
      fill=c("red","blue")
      )

#decision boundary
n.grid<-500  #set up grid 
x1.grid<-seq(f=min(train.X[,1]),t=max(train.X[,1]),l=n.grid)
x2.grid<-seq(f=min(train.X[,2]),t=max(train.X[,2]),l=n.grid)
grid<-expand.grid(x1.grid,x2.grid)

k.opt<-match(max(Mset[-1]),Mset)  #k=116
set.seed(1)
mod.opt<-knn(train.X,grid,train.Y,k=k.opt,prob=T)
prob<-attr(mod.opt,'prob')
prob<-ifelse(mod.opt=='yes',prob,1-prob) #assign probability for each grid pixel
prob<-matrix(prob,n.grid,n.grid)

#draw the picture and decision boundary 
plot(train.X, col=ifelse(train.Y == 'yes','blue','red'))
contour(x1.grid,x2.grid,prob,levels=0.5,labels ='', xlab='',ylab='',main='',add=T)




#####
#(2)

library(keras)
cifar <- dataset_cifar10()
str(cifar)
x.train <- cifar$train$x
y.train <- cifar$train$y
x.test <- cifar$test$x
y.test <- cifar$test$y
# reshape the images as vectors (column-wise)
# (aka flatten or convert into wide format)
# (for row-wise reshaping, see ?array_reshape)
dim(x.train) <- c(nrow(x.train), 32*32*3) # 50000 x 3072
dim(x.test) <- c(nrow(x.test), 32*32*3) # 50000 x 3072
# rescale the x to lie between 0 and 1
x.train <- x.train/255
x.test <- x.test/255
# categorize the response
y.train <- as.factor(y.train)
y.test <- as.factor(y.test)
# randomly sample 1/100 of test data to reduce computing time
set.seed(2021)
id.test <- sample(1:10000, 100)
x.test <- x.test[id.test,]
y.test <- y.test[id.test]

#fit knn model
knum <- c(50,100,200,300,400)
knn.pic <- sapply(knum, function (k) knn(x.train, x.test, y.train, k))
test.err.rate<-sapply(c(1:5),function(c) mean(knn.pic[,c]!=y.test)) #calculate the error rate for 5 different K values
test.err.rate

#find best k value
plot(test.err.rate,ylab='error',xlab='k-value',pch=20,type='b',col='purple')
x<-match(min(test.err.rate),test.err.rate)
#the optimal k value
knum[x]
result<-knn.pic[,x]
t<-table(y.test,result)
sum(diag(t))











