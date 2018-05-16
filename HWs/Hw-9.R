#HW -9 



#Q1 (9-10)
data<-read.table("http://www.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%209%20Data%20Sets/CH09PR10.txt", header= FALSE , sep="")
data

#b)
Y=data[,1]
X1=data[,2]
X2=data[,3]
X3=data[,4]
X4=data[,5]



data=data.frame(Y,X1,X2,X3,X4)
X=data.frame(X1,X2,X3,X4)

plot(data)
cor(X)

fit1=lm(Y~X1+X2+X3+X4,data=data)
summary(fit1)


#Q2 (9-11)
#a)
library(leaps)
leaps=regsubsets(Y~X1+X2+X3+X4,data=data, nbest=15)
plot(leaps, scale="adjr2")



#b)
#summary(leaps)
plot(leaps, scale="Cp")
plot(leaps, scale="r2")
plot(leaps, scale="bic")




#Q3 (9-18)
#a)

null=lm(Y~1, data=data)

full=lm(Y~X1+X2+X3+X4,data=data)

step(null, scope = list(upper=full), data=data, direction="both",levels='(0.05,0.01)')



#Q4 (9-21)


PRESS=0
for (i in 1 : dim(data)[1]){
  newdata=data[-i,] #data frame without the ith row.
  newmodel=lm(Y~X1+X3+X4,data=newdata)
  newx=data.frame(X1=X1[i],X3=X3[i],X4=X4[i])
  pred=predict.lm(newmodel,newx)[1]
  PRESS=PRESS+(pred-data$Y[i])^2
}
PRESS

fit2=lm(Y~X1+X3+X4,data=data)
summary(fit2)
smse=summary(fit2)$sigma
mse=smse^2
anova(fit2)


#Q5 (9-22)

Tdata<-read.table("http://www.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%209%20Data%20Sets/CH09PR22.txt", header= FALSE , sep="")
Tdata

TY=Tdata[,1]
TX1=Tdata[,2]
TX2=Tdata[,3]
TX3=Tdata[,4]
TX4=Tdata[,5]

#a)
Vdata=data.frame(Y=TY,X1=TX1,X2=TX2,X3=TX3,X4=TX4)
Valid_X=data.frame(TX1,TX2,TX3,TX4)
cor(Valid_X)


cor(X)


#b) 
VFIT=lm(TY~TX1+TX3+TX4,data=Vdata)

summary (VFIT)
Summary (fit2)
smse2=summary(VFIT)$sigma
mse2=smse2^2

#c)
xnewN=data.frame(X1=TX1,X2=TX2,X3=TX3,x4=TX4)
fit=lm(Y~X1+X3+X4,data=data)
Vpred=predict.lm(fit,xnewN,interval='prediction')

sum=0
for(i in 1:25){
  dif=TY[i]-Vpred[i,1]
  sum=sum+dif^2
}
MSRP=sum/25
MSRP



xnewN=data.frame(X1=TX1,X2=TX2,X3=TX3,x4=TX4)

#fit=lm(Y~X1+X3+X4,data=data)
Vpred=predict.lm(fit2,xnewN,interval='prediction')

MSPR=sum((Vpred[,1]-as.matrix(TY))^2)/25
MSPR


MSE=summary(fit)$sigma
pred=predict(newmodel,data=newX)


#d)
total <- rbind(data , Vdata)

Cmodel=lm(Y~X1+X3+X4,data=total)
summary(Cmodel)
