#Codes for Cahpter-1


#Checking the current  R working directory:
getwd ()

#Changing R working directory:
setwd ("R:/Teaching/2018/MA 542 S/R Codes")


##Importing data from internet
#Plastic hardnes example:
data<-read.table("http://www.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%201%20Data%20Sets/CH01PR22.txt ", header= FALSE , sep="")
data

#Importing data:
datart=read.table("R:\\Teaching\\2017\\MA 542 F\\R Codes\\Plastic Hardnes.csv",header = FALSE)
datart


#Simple Linear Regression Model.
Hardness=data[,1]
Time=data[,2]

#Creating the darta frame:
dataf=data.frame(Hardness, Time)
dataf

#model
SLR=lm(Hardness~Time,data=dataf)
SLR

summary(SLR)
#Extracting Estimators:
b0=summary(SLR)$coefficients[1,1]
b0

b1=summary(SLR)$coefficients[2,1]
b1 


sigmahat=summary(SLR)$sigma #Least square estimator.
sigmahat

#Calculating MLE of Sigma
DoFR=df.residual(SLR) #Extracting error degrees of freedom:
DoFR

mle_sigmahat=sqrt(summary(SLR)$sigma^2*DoFR/(length(Hardness)))
mle_sigmahat

#Fitted Values:
Fitvals=fitted.values(SLR)
Fitvals

#Calculating Residuals:
Res=residuals(SLR)
Res

#MLE of sigma in a different way
mles=sqrt(sum(Res*Res)/(length(Hardness)))
mles

#Properties of residuals:
sumei=sum(Res)
sumei

sumXiei=sum(Time*Res)
sumXiei

sumyihatei=sum(Fitvals*Res)
sumyihatei

sumyi_sumyihat=sum(Hardness)-sum(Fitvals)
sumyi_sumyihat

XbarYbar=mean(Hardness)-(b0+b1*mean(Time))
XbarYbar
