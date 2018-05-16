#Regression with qualitative predictors:
#Insurance Innovation Example:
#Y- # of months elapsed, X1- size of firm, X2- type of firm.

#Data:


data=read.table("R:\\Teaching\\2016\\MA 542\\Class preperation\\Insurance.csv",header = FALSE)
colnames(data)<-c("Y","X1","X2")
attach(data)

#fitting the model:
#______________________________________________________________
fit<-lm(Y~X1 +X2)
fit

summary(fit)


#ANOVA table:
#_____________________________________________________________

anova(fit)



#Fitting a model with interaction term:
#_____________________________________________________________

intfit=lm(Y~X1+X2+X1*X2)
intfit

summary(intfit)

