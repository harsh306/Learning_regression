# Remedial measures in R ( Rcodes for Chapter 11)

#Weighted Least square method
# Machine Speed example (7:11):
# Y-Productivity, X-Machine speed setting
#importing data
data=read.table("R:\\Teaching\\2016\\MA 542\\Class preperation\\Ex.11.7.csv",header = FALSE)
X=data[,2] 

Y=data[,1]

#Fitting a linear regression model

linfit=lm(Y~X)

#ls(linfit)

res=residuals(linfit)

#Residual plot vs X
plot(X, res)

#Residual^2 plot vs X
plot(X, res^2)

#Estimating the variance function.
varf=lm(res^2~X)

#Estimated weights:

wi=1/fitted.values(varf)

newdata=data.frame(Y,X,wi)

#Weighted least square function:
wlsq=lm(Y~X,weights=wi)

summary(linfit)
coef(linfit)[2]
summary(wlsq)



#Ridge Regression:
#Patient Satisfaction Example (11.22 or 6.15):
#importing data
data=read.table("R:\\Teaching\\2016\\MA 542\\Class preperation\\Ex.6.15.csv",header = FALSE)
X1=data[,2] 
X2=data[,3] 
X3=data[,4] 
Y=data[,1]
n=length(Y)

y=1/sqrt(n-1)*(Y-mean(Y))/sd(Y)
x1=1/sqrt(n-1)*(X1-mean(X1))/sd(X1)
x2=1/sqrt(n-1)*(X2-mean(X2))/sd(X2)
x3=1/sqrt(n-1)*(X3-mean(X3))/sd(X3)
data1=data.frame(y,x1,x2,x3)
data2=data.frame(Y,X1,X2,X3)

library(MASS)
library(genridge)
 lmod=lm(Y~X1+X2+X3,data=data2)
 vif(lmod)
 
 X=data.matrix(data2[,c(2:4,1)])
 cvals= seq(0,500,by= 5)
 lridge <- ridge(y, X, lambda=cvals)
 coef(lridge)

 
 vridge <- vif(lridge)
 vridge
 

vif(ridgefits)
plot(range(cvals), range(lridge$coef),type="n")
for(i in 1:ncol(lridge$coef)){ lines(cvals,lridge$coef[,i])}




#ridgefits = lm.ridge(y~.,data=data1,lam=cvals)
#ridgefits = lm.ridge(y~x1+x2+x3+0,lam=cvals)

#ridgefits = lm.ridge(y~.,data=data1,lam=cvals)
#plot(range(cvals), range(ridgefits$coef),type="n")
#for(i in 1:nrow(ridgefits$coef)){ lines(cvals,ridgefits$coef[i,])}


#Nonparametric Regression
#_______________________________________________________________________

#Lowess Method:
#Cars example 
data(cars) # this is inbuild in R, Y-speed, X-distance.
head(cars)
Speed=cars$speed
Distance=cars$dist

#scatter plot:
plot(Distance,Speed)


#Fitting the lowess curve:
lowess(Speed~Distance) #This will give the optimal fit.

#we also can change the value of q (here it is f in R)
lowess(Speed~Distance,f=2/3)


#To see the effect of "f" value, see following plots.

plot(Distance,Speed)
lines(lowess(Speed~Distance,f=2/3),col="red")
lines(lowess(Speed~Distance,f=.9),col="green")
lines(lowess(Speed~Distance,f=1/3),col="orange")
lines(lowess(Speed~Distance,f=0.1),col="black")



#Regression Trees:
library(rpart)
library(MASS)

# Patient Satisfaction Example (11.22 or 6.15):

# grow tree (dividing data into groups)
fit <- rpart(Y~X1+X2+X3, method="anova", data=data2)


#To display the results
print(fit)


#visualize cross-validation results 
plotcp(fit)

summary(fit)


# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) # visualize cross-validation results  	





#Boostrapping.
#The following package needed to be instaled and uploaded.
library(boot)

#Data frame
speed=data.frame(Y,X)

#To calculate and return value of b1. A function like this need to be written
# for the estimate (estimates)
b1=function(data,indices){
  d = data[indices, ]
  lfit=lm(Y~X,data=d)
  res=residuals(lfit)
  sdf=lm(abs(res)~X)
  wi=1/fitted.values(sdf)
  wls=lm(Y~X,weights=wi)
  b_1=coef(wls)[2]
return(b_1)
}


results = boot(data = speed, statistic = b1, R=800)
print(results)
plot(results)

#Confidence intervals
boot.ci(results,conf=0.95, type="all")


#Confidence interval for the Least Square Fit.
confint(linfit)

#Confidence interval for the Weighted Least Square Fit.
confint(wlsq)



qt(.9999,75)
pt(0.9999,75)



