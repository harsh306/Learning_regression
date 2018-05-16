# Variable Selection in R (Chapter -9)
# Commercial Properties Example.
# X1-operating expenses, X2-taxes, X3- vacancy rates, x4- total square footage, Y- rental rates.

#importing data
data<-read.table("http://www.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%206%20Data%20Sets/CH06PR18.txt ", header= FALSE , sep="")

colnames(data)<-c("rentalrates","operexp","taxes","vaca_rates","tot_sq_footage")
attach(data)
#head(data)



library(leaps)
leaps=regsubsets(rentalrates~operexp+taxes+vaca_rates+tot_sq_footage,data=data,  nbest=15)

#To view the ranked models according to the adjusted R-squared criteria and BIC, respectively:

plot(leaps, scale="adjr2")
plot(leaps, scale="bic")

plot(leaps, scale="r2")
plot(leaps, scale="Cp")




#To fit the null model (The model with only the intercept)
null=lm(rentalrates~1, data=data)
null

#To fit the Full model( The model with all the possible predictors)
full=lm(rentalrates~operexp+taxes+vaca_rates+tot_sq_footage,data=data)
full

#We can perform forward selection using the command:
step(null, scope=list(lower=null, upper=full), direction="forward",level="0.01")

#We can perform backward elimination on the same data set using the command:
step(full, data=Housing, direction="backward",level="0.05")

#Stepwise regression can be performed using the command:
step(null, scope = list(upper=full), data=Housing, direction="both",levels="(0.05,0.01)")

