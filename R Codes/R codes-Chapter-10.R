#Model Diagnostic in R (CHapter -10)
#Problem 6.9, “Grocery Retailer.”:  Y=Hours,  X1 = Cases, X2 = Costs (Only first two predictors are used)

install.packages("car") #to install the package "car".
library(car)
#Data:
# We can import data from a excel sheet (.CSV)

data=read.table("R:\\Teaching\\2016\\MA 542\\Class preperation\\Ex.6.9.csv",header = FALSE)
X1=data[,2] 
X2=data[,3]
Y=data[,1]

Hoursd=data.frame(Y,X1,X2)

fit<-lm(Y~X1+X2, data=Hoursd)
#Residual plot
plot(X1,resid(fit),pch=16) # Residual vs X1 plot plot 
abline(0,0,lty=2,col="red")

# To draw Added Variable Plots (1st method)
plot(resid(lm(Y~X2)) ~ resid(lm(X1~X2)),col="blue",pch=16,
     xlab="e(X_1|X_2)", ylab="e(Y|X_2)")
abline(lm(resid(lm(Y~X2))~resid(lm(X1~X2))),col="red")

# To draw Added Variable Plots (second method)

avPlot( model=fit, variable=X1 )

avPlot( model=fit, variable=X2 )


PRESS <- function(linear.model) {
  #' calculate the predictive residuals
  pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
  #' calculate the PRESS
  PRESS <- sum(pr^2)
  
  return(PRESS)
}

PRESS(fit)

#We can compute and plot the leverage of each point using the following commands:

lev = hat(model.matrix(fit))
lev



#2nd method
hatvalues(fit)
plot(lev)

# To identify cases with large leverage values:
  
Hoursd[lev>0.2,] # do not forget  the "," .


# Studentized residuals:
  
r = rstudent(fit)
r

#To see a summary of  influence measures :
influence.measures(fit)


#To calculate influence measures seperately:

#Cook's distance
cook = cooks.distance(fit)
cook

#DFBETAS values
dfbetas(fit)

# DFFITS values
dffits(fit)

#variance inflation factors
vif(fit)




