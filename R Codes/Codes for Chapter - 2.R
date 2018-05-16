# Codes for Cahpter-2
# This is a countinuation of Codes for Chapter -1
# You may have to run some of the codes in Codes for chapter -1 to use these commands.

#Scatter Plots 
plot(Time,Hardness) # Plot(X,Y)


#Confidence Intervals(C : Is):
#C:Is for model parameters
confint(SLR) # 95% (default significance level) confidence intervals for beeta0 and beet1


#Other significance levels:
confint(SLR,level=0.99) # 99% confidence intervals for beeta0 and beet1

#Confidence interval for mean response:
newx=data.frame(Time=20) # Creating a data frame with the new value of the predictor.
newx
predict.lm(SLR,newx,interval="confidence",level=0.95) #95% Confidence interval at the new X level.


#Prediction interval for a new observation:
newpx=data.frame(Time=22)
newpx
predict.lm(SLR,newpx,interval="prediction",level=0.99) #99% Confidence interval at the new X level.


summary(SLR)

#ANOVA in R
anova(SLR)