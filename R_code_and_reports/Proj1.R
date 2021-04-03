getwd()
setwd("/Users/wenxionglu/Documents/GraduateCourse/Stats  6338/data")
data<-read.csv("senic.csv")
library(car)
library(MASS)
attach(data)
region<-factor(region)
is.factor(region)
#(a) use factor effect model
#H0: all mean infection risks for different regions are equal.
#H1: at least one infection risk of different regions is not equal
fm1<-lm(infprob~region)
result1<-anova(fm1)
result1 #so the F statistics is 0.04839 < alpha(0.05)
#conclude H1

#(b)
#library(emmeans)
#means.region <- emmeans(fm1,specs='region')
#pairs(means.region, adjust = "tukey")
#confint(pairs(means.region, adjust = "tukey", level = 0.9))  cannot set level as 0.9  ?
#cf_tukey

fm2 <- aov(infprob ~ region)
tukeyregion <- TukeyHSD(fm2, conf.level=.9)
tukeyregion
plot(tukeyregion)

#(c)
means.region <- emmeans(fm1,specs='region')
LF3 <- contrast(means.region, list('3-4'=c(0,0,1,-1),
                                   '2-4'=c(0,1,0,-1),
                                   '2-3'=c(0,1,-1,0),
                                   '1-4'=c(1,0,0,-1),
                                   '1-3'=c(1,0,-1,0),
                                   '1-2' = c(1,-1,0,0) 
                                   ))
bonf1<-confint(LF3, adjust = "bonferroni")
bonf1
plot(bonf1)

#2
install.packages("sets")
library(sets)
checkit<- function(data,limits,cat){
  i<-0
  while(i<length(limits)){
    i<-i+1
    if(data<limits[i]){data<-cat[i]}
  }
  return(data)
}
threshold<-c(interval(0,49.9,bounds=c("[]")),
             interval(50,54.9,bounds=c("[]")),
             interval(55,59.9,bounds=c("[]")),
             interval(60,200,bounds=c("[]")))
category<-c(1,2,3,4)
i=1
for (val in age){
  age[i]<-checkit(val,threshold,category)
  i=i+1
}
age<-as.factor(age)
fm3<-lm(infprob~age)
anova(fm3) # P=0.6412 > 0.1  conclude H0

#3
lm4<-lm(stay~region)
Anova(lm4)# since F statistics is significant, all four regions are not the same
#(a)
residualPlot(lm4,'region') #there are a few outliers exist in region 1.
#(b)
leveneTest(lm4,center='median') # The BF test shows the constancy of variance is violated
#(c)
st<-summary(lm4)
z<-st$coefficients
z
#s^2/y
z1<-z[,2]**2/z[,1]
#s/y
z2<-z[,2]/z[,1]
#s/y^2
z3<-z[,2]/z[,1]**2
st2<-matrix(c(z1,z2,z3),ncol=3,byrow=FALSE)
st2
var(st2[,1])
var(st2[,2])
var(st2[,3])
#These variance shows that s^2/y is the most stable relationship, hence we should use transformation y'=logy 
#(d)
bx<-boxcox(lm4,lambda = c(seq(-1,1,by=0.1)))
bx
bx$x[match(min(bx$y),bx$y)]# the best boxcox lamda is 1 with SSE = -69.05545
#(e)
reciprol_Y<-1/stay
lm5<-lm(reciprol_Y~region)
Anova(lm5)# Residual=0.025428
#(f)
leveneTest(lm5,center='median')# p =0.41 alpha=0.01, conclude H0

