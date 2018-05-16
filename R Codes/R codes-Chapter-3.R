# Codes for Cahpter-3
# This is a countinuation of Codes for Chapter -1 and Codes for cahpter -2
# You may have to run some of the codes in Codes for chapter -1 (or chapter -2) to use these commands.


#Residual plots:
#_______________________________________________________________________________________________
#First the SLR model should be fitted using lm command.
res=residuals(SLR)         #residuals
res
fitvals=fitted.values(SLR)  #fitted values
fitvals
sstres=res/sigmahat           #Semistudentized residuels
sstres
abres=abs(res)              #absolute residuals
abres

plot(X,res,main="Residual vs X")                             #plot of residuals Vs X
plot(fitvals,res,main="Residual vs Fitted values")            #plot of residuals Vs fitted values
plot(X,sstres,main="Semi Residual vs X")                     #plot of semi residuals Vs X
plot(fitvals,sstres,main="Semi Residual vs Fitted values")    #plot of semi residuals Vs fitted values
plot(X,abres,main="Absolute Residual vs X")                  #plot of absolute residuals Vs X
plot(fitvals,abres,main="Absolute Residual vs Fitted values") #plot of semi residuals Vs fitted values

boxplot(res,main="Box-plot of Residuals") #Box-plot of the residals.

#Normal probability plot:
rres=rank(res) #Ranking residuals
rres
smse=summary(SLR)$sigma #extracting mse.
eres=smse*(qnorm((rres-0.375)/(length(Time)+.25))) # Expected values of residuals.
eres


par(mfrow=c(1,1))
plot(eres,res,main="Normal Probability Plot", xlab="Expected residuals",ylab="Residuals")

#An alternative Way to Normal Probabilty plot
qqnorm(res) #plot pf the Theoritical quantiles and the sample quantiles
qqline(res) #adding a line to the plot


#Correlation coefficient between residuals and their expected values under normality.
TLF=cor(eres,rres) #here use ranked residuals.
TLF



#F-test for Lack of Fit:

#  Fittting the means model (this is also calld anova model)

Means=aov(Hardness~factor(Time)) #Full model # factor(X) consider only the levels of X.
SLR=lm(Hardness~Time)  #Reduced model (Simple Linear Regression model)

# Generating the additional two lines in general ANOVA table.
anova(SLR,Means)

#Main ANOVA table
anova(SLR)


#BOX-COX (Estimating lambda)
library(MASS)
BC=boxcox(Hardness~Time, lambda = seq(-2, 3, 1/10), xlab = expression(lambda), ylab = "log-Likelihood")

lambda=BC$x[which.max(BC$y)]
lambda
