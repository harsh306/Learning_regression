
#Polynomial Regression
#Power Cells Example: Y=# of cycles, X1=charge rate, X2=Temperature.

# Data:
Y=c(150,86,49,288,157,131,184,109,279,235,224)
X1=c(.6,1,1.4,.6,1,1,1,1.4,.6,1,1.4)
X2=c(10,10,10,20,20,20,20,20,30,30,30)



Xbar1=mean(X1)
Xbar2=mean(X2)


#Transformation (Scaling the original variables):

x1=(X1-Xbar1)/.4
x2=(X2-Xbar2)/10

#Correlations:
#___________________________________________________

cor(X1,X1^2)
cor(x1,x1^2)

cor(X2,X2^2)
cor(x2,x2^2)

#Fitting the model(Second order polynomial Regression Model):
#___________________________________________________


#First define square and product terms.
x1_2=x1^2
x2_2=x2^2
x12=x1*x2

#Model fitting.
prm=lm(Y~x1+x2+x1_2+x2_2+x12)
prm

#Summary output:
summary(prm)


#Lack of fit test:
#_________________________________________________________

### Method 1
fit<-lm(Y~x1+x2+x1_2+x2_2+x12)
exfactor = factor( c(seq(-4,-1), rep(0,3),seq(1,4)) )
#fit full model
anova( fit, lm(Y ~ exfactor))

### Method 2
library(alr3) # for lack of fit
pureErrorAnova(fit)



#Extended ANOVA table
#_________________________________________________________

anova(prm)




#Partial F test (to test whether first order model is sufficient).
#_________________________________________________________

# here note that full model is prm.

#fitting the reduced model:
redm=lm(Y~x1+x2)
redm
#ANOVA table:
anova(redm,prm)


#First order model with original variables:
#_________________________________________________________

firom=lm(Y~X1+X2)
firom


#Estimation of Regression Coefficients:
#_________________________________________________________
# 90% family confidence coefficients by the Bonferroni method

g = length(coef(lm(Y ~ X1+X2)) ) - 1
alpha=0.1  #for 90% confidence intervals.
confint( firom, level = 1-(alpha/g) )


