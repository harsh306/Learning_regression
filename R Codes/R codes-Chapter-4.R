# Codes for Cahpter-4
# This is a countinuation of Codes for Chapter -1 , 2 and 3.
# You may have to run some of the codes in the previous handouts 


#Simultaneous Confidence Intervals.


#Bonferroni CIs for Beta0 and Beta1
alpha=0.05
confint(SLR,level=1-alpha/2)

#Using a function.
BonCI<-function(model, alpha)
{
  X=confint(SLR,level=1-alpha/2)
  return(X)
}
BonCI(SLR,alpha=0.1)
BonCI(SLR, alpha=0.05)


#Simultaneous Estimation of Mean Responses
#_____________________________________________________________
#1. The Working-Hotelling Procedure

df=df.residual(SLR)
alpha=0.05
W=sqrt( 2 * qf(1 - alpha, 2, df) ) 
newx=data.frame(Time = c(18, 20, 22)) # Creating a data frame with the new values of the predictor.
pre    <- predict(SLR, newx, se.fit = TRUE)
lwr = pre$fit - W * pre$se.fit
upr = pre$fit + W * pre$se.fit

CI=cbind('X'=newx,'Fit Val'=pre$fit,lwr,upr)
CI


#Using a function

WH_CI <- function(model, newdata, alpha )
{
  df    <- nrow(model.frame(model)) - length(coef(model))  # 23
  W     <- sqrt( 2 * qf(1 - alpha, 2, df) )                # 2.2580
  ci    <- predict(model, newdata, se.fit = TRUE)   
  x <- cbind(
    'x'   = newdata,
    'SE'   = ci$se.fit,
    'fit' = ci$fit,
    'lwr' = ci$fit - W * ci$se.fit,
    'upr' = ci$fit + W * ci$se.fit)
  
  return(x)
}

new <- data.frame(Time = c(18, 20, 22))
WH_CI(SLR, new,0.05)

WH_CI(SLR, data.frame(Time = c(19, 29, 23)),0.1)


#2.The Bonferroni Procedure

alpha=0.05
newx=data.frame(Time = c(18, 20, 22))
g=nrow(newx) #number of parameters

predict(SLR, newx, int = "confidence", level = (1 - alpha/g), se.fit = TRUE)

#Using a function

BCIMR=function(model, newdata, alpha )
{
  
  g=nrow(newdata) 
  CI=predict(model, newdata, int="c",level =(1-alpha/g), se.fit = TRUE)
  return(CI)
}

BCIMR(SLR, data.frame(Time = c(19, 29, 23)),0.1)

BCIMR(SLR, data.frame(Time = c(19, 29, 23,25)),0.01)


#Simultaneous Prediction Intervals for New Observations
#_____________________________________________________________
#The Scheffe and Bonferroni Procedures


PI_SoB <- function(model, newdata, type = c("B", "S"), alpha)
{
  g  <- nrow(newdata)
  CI <- predict(model, newdata, se.fit = TRUE)
  if(match.arg(type) == "B"){
    M=  qt(1 - alpha / (2*g), model$df)} # B = (4.9a)
  else{              
    M= sqrt( g * qf( 1 - alpha, g, model$df))} # S = (4.8a)
  
  spred <- sqrt( CI$residual.scale^2 + (CI$se.fit)^2 )  # (2.38) 
  x <- data.frame(
    "x"     = newdata,
    "spred" = spred,
    "fit"   = CI$fit,
    "lower" = CI$fit - M * spred,
    "upper" = CI$fit + M * spred)
  
  return(x)
}


PI_SoB(SLR, data.frame(Time = c(19, 29, 23)), type = "S",0.05)

PI_SoB(SLR, data.frame(Time = c(19, 29, 23,25)), type = "B",0.01)





#Regression through Origin
#==============================================================================


#Importing Warehouse data

WHdata<-read.table("http://www.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%204%20Data%20Sets/CH04TA02.txt ", header= FALSE , sep="")
WHdata

X=WHdata[,1]
Y=WHdata[,2]

#Fitting a SLR model through the origin

fit <- lm(Y ~ 0 + X)
fit

# An alternative way

fitA = lm(Y~X-1)
fitA


summary(fit)


#ANOVA
anova(fit)


#Confidence intervals 
confint(fit,level=0.99)
