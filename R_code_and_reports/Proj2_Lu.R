setwd("/Users/wenxionglu/Documents/GraduateCourse/6340 Stats/proj2")
#1
wine<-read.table('wine.txt',header=TRUE)
wine$Region<-factor(wine$Region) #Tune qualitative data into correct data form
pairs(wine[,1:6],col = wine$Region,lower.panel=NULL,cex=0.5)
---
  title: "MiniProj_2"
author: "Wenxiong Lu"
output: pdf_document
---

setwd("/Users/wenxionglu/Documents/GraduateCourse/6340 Stats/proj2")

##### Q1
#(a) The are 17 observations from region 1, 9 obs. from region 2 and 12 obs. from region 3. It seems obs. from region 3 have highest quality and obs. from region 2 and 1 are mixed.


wine<-read.table('wine.txt',header=TRUE)
wine$Region<-factor(wine$Region) #Tune qualitative data into correct data form
pairs(wine[,1:6],col = wine$Region,lower.panel=NULL,cex=0.5)



##### (b): Yes. Quality is an approriate variable

##### (c):
lm.fit_c <- lm(Quality ~ Clarity, data = wine)
summary(lm.fit_c)
confint(lm.fit_c)
# For predictor Clarity, P-value is sufficiently large(0.865), adjusted R-square is small(-0.02695) and confidence (-5.105130, 6.043584) includes 0. Indicate Clarity is not a good choice for single predictor.

lm.fit_a <- lm(Quality ~ Aroma, data = wine)
summary(lm.fit_a)
confint(lm.fit_a)
# For predictor Aroma, P-vallue  (6.87e-07), Adjusted R-squared: 0.4864, 5% confidence interval (0.8850212 1.787982) does not include 0. May be a good choice for predictor.

lm.fit_b <- lm(Quality ~ Body, data = wine)
summary(lm.fit_b)
confint(lm.fit_b)

# For predictor Body, P-value  (0.000361), Adjusted R-squared: 0.2817, 5% confidence interval (0.8850212 1.787982) does not include 0. May be a good choice for predictor.

lm.fit_f <- lm(Quality ~ Flavor, data = wine)
summary(lm.fit_f)
confint(lm.fit_f)

# For predictor Flavor, P-value  (3.68e-09), Adjusted R-squared: 0.6137, 5% confidence interval (1.159604 1.984177) does not include 0. May be a good choice for predictor.

lm.fit_o <- lm(Quality ~ Oakiness, data = wine)
summary(lm.fit_o)
confint(lm.fit_o)
# For predictor Oakiness, P-value  (0.779), Adjusted R-squared: -0.0255, 5% confidence interval (-1.066083  0.805353) include 0. It is not a good choice for predictor.

lm.fit_r <- lm(Quality ~ Region, data = wine)
summary(lm.fit_r)
confint(lm.fit_r)
attach(wine)
contrasts(Region)
# For predictor Region, P-values, Region 1:(< 2e-16), Region 2: 0.00757, Region 3: 7.01e-06. Overall Adjusted R-squared: 0.5891, 5% confidence interval Region 1: (11.330893 12.6220486) Region 2:(-2.629298 -0.4347544) Region 3:(1.603271  3.6104546). May be a good choice for predictor.

#Comments: As the results shown above, 'Clariy' and 'Oakiness' are not significantly associated to Quality per 95% confidence level. The rest of variables are associated to Quality. 

##### (d): 
#Without considering interaction term, the remaining predictors are 'Flavor' and 'Region'.(The interaction terms will be tested in question e) We can reject H0 for the F test of these parameters. Firstly, fit a full model with all the parameters, then we can find out that Clarity has highest p-value '0.990736'. After removing Clarity, the next highest will be removed. The 'removing sequence' is Clarity(0.990736), Body(0.746249), Aroma(0.70489), Oakiness(0.128060).


lm.all<-lm(Quality ~ Clarity+Aroma+Body+Flavor+Oakiness+Region, data=wine)
summary(lm.all)
lm.a2<-update(lm.all, Quality~.-Clarity)
summary(lm.a2)
lm.a3<-update(lm.a2, Quality~.-Body)
summary(lm.a3)
lm.a4<-update(lm.a3, Quality~.-Aroma)
summary(lm.a4)
lm.a5<-update(lm.a4, Quality~.-Oakiness)
summary(lm.a5)

##### (e) Firstly, fit a full model (including all the posible interaction terms). Then remove terms with highest p-value and make anova for the updated model and find the next highest p-value and remove the predictor. By repeating this process we will remove the predictors in sequence: [Body:Flavor(interaction term between Body and Flavor), Aroma:Region, Flavor:Region, Aroma:Flavor:Region, Body:Flavor:Region, Aroma:Flavor, Aroma:Body:Region, Aroma:Body:Flavor:Region, Aroma:Body:Flavor, Aroma:Body, Body:Region]. The remaing predictors are Aroma, Body, Flavor, Region. Then, check the F test for multiple linear model and we can see that the p-value of Aroma and Body changed from significant to insignificant. This is due to Region is a qualitative data, both Aroma and Body have significant interaction to Region 1 and Region 2. The currently reasonably good model is: #####Quality = x0+Aroma*x1+Body*x2+Flavor*x3+Region*x4.

mlr <- lm(Quality ~ Aroma*Body*Flavor*Region, data=wine)
summary(mlr)
anova(mlr)
mlr1<-update(mlr, ~.-Body:Flavor)# remove the most insignificant interaction term
anova(mlr1)
mlr2<-update(mlr1, ~.-Aroma:Region)# remove the next most insignificant interaction term
anova(mlr2)
mlr3<-update(mlr2, ~.-Flavor:Region)# remove the next most insignificant interaction term
anova(mlr3)
mlr4<-update(mlr3, ~.-Aroma:Flavor:Region)# remove the next most insignificant interaction term
anova(mlr4)
mlr5<-update(mlr4, ~.-Body:Flavor:Region)
anova(mlr5)
mlr6<-update(mlr5, ~.-Aroma:Flavor)
anova(mlr6)
mlr7<-update(mlr6, ~.-Aroma:Body:Region)
anova(mlr7)
mlr8<-update(mlr7, ~.-Aroma:Body:Flavor:Region)
anova(mlr8)
mlr9<-update(mlr8, ~.-Aroma:Body:Flavor)
anova(mlr9)
mlr10<-update(mlr9, ~.-Aroma:Body)
anova(mlr10)
mlr11<-update(mlr10, ~.-Body:Region)
anova(mlr11)
summary(mlr11)# Since the p-value of Aroma and body changed from significant to insignificant in summary, it may indicate that there are some reaction terms between different regions and Aroma and Body. However, overall the Body and Aroma are significant predictors.


mlr11<-lm(Quality ~ Aroma + Body + Flavor + Region, data=wine)
anova(mlr11)
summary(mlr11)


##### (f) By fiting a linear model for Aroma = Region*Body, we can find out the exact interaction term in (e) is of 1.(Region 1, Body, Region2:Body) and fit another model Body = Aroma*Region we can see interaction term is of 2.(Aroma, Region2, Aroma:Region2). This indicates that there are interaction terms between Region1 and Aroma, Region and Body. So we should consider put back the interaction term between Region:Aroma and Region:Body.
##### The final model: Quality = x0 + Aroma x1 + Body x2 + Flavor x3 + Region x4 + Aroma:Region x5 + Body:Region x6


summary(lm(Aroma~Body,wine))
itc <-lm(Aroma~Region*Body,wine)
anova(itc)
summary(itc)
itc2 <- lm(Body~Aroma*Region,wine)
anova(itc2)
summary(itc2)
mlrf1<-update(mlr11, ~.+Region:Aroma+Region:Body)
summary(mlrf1)
summary(mlr11)
anova(mlrf1)
anova(mlr11)



library(MASS)
adm<-read.csv('admission.csv',header=TRUE)
adm$Group<-factor(adm$Group) #Tune qualitative data into correct data form
plot(adm$GPA,adm$GMAT,col=adm$Group) #Plot the figure of GPA vs GMAT vs Group
adm.test<-adm[1:5,] #Split train and test data
adm.train<-adm[6:length(adm$GPA),]

##### 2(a). It is easy to see that group is related to GPA and GMAT. Group might be an important predictor. GPA and GMAT are related to each differently in different group. 



lda.adm=lda(Group~GPA+GMAT, data = adm.train)
lda.adm
#decision boundary
n.grid<-50  #set up grid 
x1.grid<-seq(f=min(adm.train$GPA),t=max(adm.train$GPA),l=n.grid)
x2.grid<-seq(f=min(adm.train$GMAT),t=max(adm.train$GMAT),l=n.grid)
grid<-expand.grid(GPA=x1.grid,GMAT=x2.grid)



set.seed(1)
prd = as.numeric(predict(lda.adm, newdata = grid)$class)

#create LD sequences from min to max values 
p = predict(lda.adm, newdata= grid)
p.x = seq(f = min(p$x[,1]), t = max(p$x[,1]), l = n.grid) 
p.y = seq(f = min(p$x[,2]), t = max(p$x[,2]), l = n.grid)

plot(lda.adm, panel = function(x, y, ...) { points(x, y, ...) },
     xlab='GPA', ylab='GMAT',
     col = adm.train$Group, 
     pch = c(17,19,15)[adm.train$Group],
     ylim=c(-3,3), xlim=c(-5,5))
contour(x = p.x, y = p.y, z = matrix(prd, nrow = n.grid, ncol = n.grid),
        levels = c(1,2,3), add = TRUE, drawlabels = FALSE)
#for training data
train.result <- predict(lda.adm, adm.train[,1:2])
tt<-table(adm.train[,3],train.result$class)
tt
mc_train<-(1-sum(diag(tt))/sum(tt))
print('confusion matrix for training data')
mc_train
#for test data
test.result<- predict(lda.adm, adm.test[,1:2])
t<-table(adm.test[,3],test.result$class)
print('confusion matrix for test data')
t
misclassification <- (1-sum(diag(t))/sum(t))
misclassification

##### (b) The decision boundary seems sensible. The misclassification rate:p(training data) =  0.05, p(test data) = 0.6  There is exist overfiting problem in this model.


qda.adm=qda(Group~GPA+GMAT, data = adm.train)
qda.adm
#decision boundary
n.grid<-50  #set up grid 
x1.grid<-seq(f=min(adm.train$GPA),t=max(adm.train$GPA),l=n.grid)
x2.grid<-seq(f=min(adm.train$GMAT),t=max(adm.train$GMAT),l=n.grid)
grid<-expand.grid(GPA=x1.grid,GMAT=x2.grid)



set.seed(1)
prd2 = as.numeric(predict(qda.adm, newdata = grid)$class)

#create QD sequences from min to max values 
p2 = predict(qda.adm, newdata= grid)
prob1<-p2$posterior[,1]-pmax(p2$posterior[,2],p2$posterior[,3])
prob2<-p2$posterior[,2]-pmax(p2$posterior[,1],p2$posterior[,3])


prob1 <- matrix(prob1, nrow = n.grid, ncol = n.grid, byrow = F)
prob2 <- matrix(prob2, nrow = n.grid, ncol = n.grid, byrow = F)



plot(adm.train$GPA,adm.train$GMAT, col = adm.train$Group)
contour(x = x1.grid, y =x2.grid, prob1,levels = 0.5,labels="",xlab="",ylab="",add=T)
contour(x = x1.grid, y =x2.grid, prob2,levels = 0.5,labels="",xlab="",ylab="",add=T)



#for training data
train.result <- predict(qda.adm, adm.train[,1:2])
tt2<-table(adm.train[,3],train.result$class)
tt2
mc_train<-(1-sum(diag(tt))/sum(tt))
print('confusion matrix for training data')
mc_train
#for test data
test.result<- predict(qda.adm, adm.test[,1:2])
t2<-table(adm.test[,3],test.result$class)
print('confusion matrix for test data')
t2
misclassification <- (1-sum(diag(t2))/sum(t2))
misclassification

##### (c) The decision boundary seems sensible. The misclassification rate:p(training data) =  0.025, p(test data) = 0.2  The model predicts the test data well. 
##### (d) Ingeneral, QDA performs better than LDA in this case.So QDA is recommend.

#3

library(MASS)
dib<-read.csv('diabetes.csv', header=TRUE)
dib$Outcome<-factor(dib$Outcome) #Change qualitative data into correct data form
pairs(dib[,1:8],col = dib$Outcome, lower.panel=NULL,cex=0.5)
attach(dib)
dib.test<-dib[1:300,] #Split train and test data
dib.train<-dib[301:length(dib$Outcome),]
lda.dib<- lda(Outcome~dib.train$Pregnancies+dib.train$Glucose+dib.train$Age+dib.train$BloodPressure+dib.train$Insulin+dib.train$SkinThickness+dib.train$BMI+dib.train$DiabetesPedigreeFunction,data=dib.train)
lda.dib.pred<-predict(lda.dib,dib.train[,1:8])
dib.pred <- ifelse((lda.dib.pred$posterior)>0.5,1,0)# apply cutoff rate = 0.5
library(caret)
confusionMatrix(as.factor(dib.pred), as.factor(lda.dib,dib.train[,9]))

table()

