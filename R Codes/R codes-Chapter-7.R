#Codes FOr chapter 7
#Multiple Regression in R (Extra Sum of Squares)
#Problem 6.9, “Grocery Retailer.”:  Y=Retailer,  X1 = Cases, X2 = Costs, and X3 = Holiday.

#Data:

data=read.table("http://www.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%206%20Data%20Sets/CH06PR09.txt", header= FALSE , sep="")
Cases=data[,2] 
Costs=data[,3]
Holiday=data[,4]
Retailer=data[,1]

data=data.frame(Retailer,Holiday,Costs,Cases)


lrm=lm(Retailer~Cases+Costs+Holiday) # Fitting the regression Model and assigning to variable "lrm".
lrm

#ANOVA table (The extended ANOVA table)

anova(lrm)

#To calculate and store SSR,MSR, SSE, MSE:

SSR = sum( anova(lrm)[1:3,2] ) 
SSR
MSR = SSR / 3
MSR
SSE = anova(lrm)[4,2]
SSE
MSE = anova(lrm)[4,3]
MSE

#modael with different order of predictors.
Model2 <- lm( Retailer ~ Holiday+Cases+Costs)
anova(Model2)


#General linear test:

Reduced <- lm( Retailer ~ Holiday+Cases) #fitting the reduced model
Reduced
anova(Reduced, lrm) #to get the ANOVA comparison:


#The other tests:
#_____________________________________________________________
#Test H0: ß2 = 0, ß3 = 600 against its alternative.

RetailerN=Retailer-600*Holiday

Reduced2 <- lm(RetailerN ~ Cases)
Reduced2

anova(Reduced2)

#Extracting sum of squares and degrees of freedom:
SSE_R=anova(Reduced2)[2,2]
#SSE_R
DF_R=anova(Reduced2)[2,1]
#DF_R

SSE_F = anova(lrm)[4,2]
#SSE_F
DF_F=anova(lrm)[4,1]
#DF_R

#Test atatistics
F=((SSE_R-SSE_F)/(DF_R-DF_F))/(SSE_F/DF_F)
F

#P-value
Pvalue=1-pf(F,DF_R-DF_F,DF_F)
Pvalue



#Coefficients of Partial Determination

M1=lm(Retailer~ Costs+Holiday)
SSEX_2X_3=anova(M1)[3,2]
SSEX_2X_3

M2=lm(Retailer~ Cases+Costs+Holiday)
SSEX_1X_2X_3=anova(M2)[4,2]
SSEX_1X_2X_3

RSQ_Y1_23=(SSEX_2X_3-SSEX_1X_2X_3)/SSEX_2X_3
RSQ_Y1_23
