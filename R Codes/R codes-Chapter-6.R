#Multiple Linear Regression in R

##Importing data from internet
#Problem 6.9, "Grocery Retailer.":  Y=Retailer,  X1 = Cases, X2 = Costs, and X3 = Holiday.:
data<-read.table("http://www.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%206%20Data%20Sets/CH06PR09.txt", header= FALSE , sep="")
data


Cases=data[,2] 
Costs=data[,3]
Holiday=data[,4]
Retailer=data[,1]
data=data.frame(Retailer,Holiday,Costs,Cases)

#Scatter-plot Matrix
plot(data)

# Corelation Matrix
cor(data)


#Fitting the Model
MLR=lm(Retailer~Cases+Costs+Holiday)
MLR

summary(MLR)


#Confidence and Prediction Intervals:
#__________________________________________
#Confidence intervals for model parameters:
confint(MLR)  # 95% (default significance level) confidence intervals for beeta0 and beet1


#Confidence intervals for model parameters with other significance levels:
confint(MLR,level=0.99) # 99% confidence intervals for beeta0 and beet1

#Confidence interval for mean response:
newx=data.frame(Cases=300000,Costs=6,Holiday=0) # Creating a data frame with the new value of predictors.

predict.lm(MLR,newx, interval="confidence", level=0.95) #95% Confidence interval at the new X level.


#Prediction interval for a new observation:
newpx=data.frame(Cases=310000,Costs=7,Holiday=1) # Creating a data frame with the new value of predictors.

predict.lm(MLR,newpx, interval="prediction", level=0.99) #99% Confidence interval at the new X level.

#The General Linear Regression Model
GLRM=glm(Retailer~Cases+Costs+Holiday, family=gaussian)
GLRM

summary(GLRM)








