# Chapter 9 Lab: Support Vector Machines

# Support Vector Classifier

set.seed(1)
x=matrix(rnorm(20*2), ncol=2) # 20  observations in 2 classes - normally distributed 
y=c(rep(-1,10), rep(1,10)) # y variable with 10 in each class 
x[y==1,]=x[y==1,] + 1 
plot(x, col= y+3 , pch=19) 

# Loading library        
library(e1071)    
# converting data into data frmae for SVM 
dat=data.frame(x=x, y=as.factor(y))

# create a model 
svmfit=svm(y~., data=dat, kernel="linear", cost=10,scale=FALSE)
print(svmfit)
plot(svmfit, dat)
svmfit$index
summary(svmfit)
# making own plot 
make.grid = function(x,n=75){
  grange = apply(x, 2, range)
  x1 = seq(from = grange[1,1] , to = grange[2,1] , length=n )
  x2 = seq(from = grange[1,2] , to = grange[2,2] , length=n )
  expand.grid(X1=x1,X2=x2)
}

xgrid = make.grid(x)
ygrid = predict(svmfit,xgrid)


# Tune SVM 
set.seed(1)
tune.out=tune(svm,y~.,data=dat,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)
xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))
ypred=predict(bestmod,testdat)
table(predict=ypred, truth=testdat$y)
svmfit=svm(y~., data=dat, kernel="linear", cost=.01,scale=FALSE)
ypred=predict(svmfit,testdat)
table(predict=ypred, truth=testdat$y)
x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch=19)
dat=data.frame(x=x,y=as.factor(y))
svmfit=svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svmfit)
plot(svmfit, dat)
svmfit=svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit,dat)

# Non linear SVM 
names(ESL.mixture)
attach(ESL.mixture)
plot(x,col=y+1)
dat = data.frame(y= factor(y) , x)
fit = svm(factor(y)~., data = dat , scale= FALSE , kernel = "radial" , cost = 5)
xgrid = expand.grid(X1=px1 , X2 = px2)
ygrid = predict(fit , xgrid)
plot(xgrid, col=as.numeric(ygrid), pch=20 , cex=.2)
points(x, col=y+1,pch=19)
# improving fit

func = predict(fit , xgrid , decision.values = TRUE)
func = attributes(func)$decision
xgrid = expand.grid(X1=px1 , X2 = px2)
ygrid = predict(fit , xgrid)
plot(xgrid, col=as.numeric(ygrid), pch=20 , cex=.2)
points(x, col=y+1,pch=19)
contour(px1,px2 , matrix(func,69,99), level = 0 , add=TRUE)
contour(px1,px2 , matrix(func,69,99), level = 0.5 , add=TRUE , col = "blue" , lwd = 2)


# Support Vector Machine

set.seed(1)
x=matrix(rnorm(200*2), ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat=data.frame(x=x,y=as.factor(y))
plot(x, col=y)
train=sample(200,100)
svmfit=svm(y~., data=dat[train,], kernel="radial",  gamma=1, cost=1)
plot(svmfit, dat[train,])
summary(svmfit)
svmfit=svm(y~., data=dat[train,], kernel="radial",gamma=1,cost=1e5)
plot(svmfit,dat[train,])
set.seed(1)
tune.out=tune(svm, y~., data=dat[train,], kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)
table(true=dat[-train,"y"], pred=predict(tune.out$best.model,newx=dat[-train,]))

# ROC Curves

library(ROCR)
rocplot=function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)}
svmfit.opt=svm(y~., data=dat[train,], kernel="radial",gamma=2, cost=1,decision.values=T)
fitted=attributes(predict(svmfit.opt,dat[train,],decision.values=TRUE))$decision.values
par(mfrow=c(1,2))
rocplot(fitted,dat[train,"y"],main="Training Data")
svmfit.flex=svm(y~., data=dat[train,], kernel="radial",gamma=50, cost=1, decision.values=T)
fitted=attributes(predict(svmfit.flex,dat[train,],decision.values=T))$decision.values
rocplot(fitted,dat[train,"y"],add=T,col="red")
fitted=attributes(predict(svmfit.opt,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],main="Test Data")
fitted=attributes(predict(svmfit.flex,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],add=T,col="red")

# SVM with Multiple Classes

set.seed(1)
x=rbind(x, matrix(rnorm(50*2), ncol=2))
y=c(y, rep(0,50))
x[y==0,2]=x[y==0,2]+2
dat=data.frame(x=x, y=as.factor(y))
par(mfrow=c(1,1))
plot(x,col=(y+1))
svmfit=svm(y~., data=dat, kernel="radial", cost=10, gamma=1)
plot(svmfit, dat)

# Application to Gene Expression Data

library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)
table(Khan$ytrain)
table(Khan$ytest)
dat=data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out=svm(y~., data=dat, kernel="linear",cost=10)
summary(out)
table(out$fitted, dat$y)
dat.te=data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te=predict(out, newdata=dat.te)
table(pred.te, dat.te$y)
