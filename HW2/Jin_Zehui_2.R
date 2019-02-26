setwd("/Users/kathy/Desktop/Columbia University/2019Spring/Business Analytics/HW2")

# Prob1 *****************************************************************************
gedata = read.csv("GE.csv")

l = length(gedata$Date)
gedata$oneday_return[2:l] = (gedata$Adj.Close[2:l] - gedata$Adj.Close[1:l-1])/gedata$Adj.Close[1:l-1]
gedata$fiveday_return[6:l] = (gedata$Adj.Close[6:l] - gedata$Adj.Close[1:(l-5)])/gedata$Adj.Close[1:(l-5)]
gedata$target = gedata$oneday_return
gedata$target[1:l-1] = gedata$oneday_return[2:l]
 
attach(gedata) 

mean(oneday_return, na.rm=TRUE)
mean(fiveday_return, na.rm=TRUE)

trainRows = format(as.Date(Date),"%Y") ==2016
train = gedata[trainRows,]
testRows = format(as.Date(Date),"%Y") ==2017
test = gedata[testRows,]

fit1 = lm(target ~ oneday_return + fiveday_return, data=train)
summary(fit1)

pred_return = predict(fit1,newdata = test)
pred_return = pred_return[-1]
avg_return = mean(pred_return)
final_return = prod(pred_return+1)

detach(gedata)



# Prob2 *****************************************************************************
clgdata = read.csv("CollegeData.csv")

clgdata = na.omit(clgdata)
l = length(clgdata$INSTNM)

clgdata$sqrtCOSTT4_A = sqrt(clgdata$COSTT4_A)
clgdata$sqrtTUITIONFEE_OUT = sqrt(clgdata$TUITIONFEE_OUT)
clgdata$sqrtTUITFTE = sqrt(clgdata$TUITFTE)
clgdata$sqrtAVGFACSAL = sqrt(clgdata$AVGFACSAL)

clgdata$intCTO = as.double(clgdata$COSTT4_A) * as.double(clgdata$TUITIONFEE_OUT)
clgdata$intCT = as.double(clgdata$COSTT4_A) * as.double(clgdata$TUITFTE)
clgdata$intCA = as.double(clgdata$COSTT4_A) * as.double(clgdata$AVGFACSAL)
clgdata$intTOT = as.double(clgdata$TUITIONFEE_OUT) * as.double(clgdata$TUITFTE)
clgdata$intTOA = as.double(clgdata$TUITIONFEE_OUT) * as.double(clgdata$AVGFACSAL)
clgdata$intTA = as.double(clgdata$TUITFTE) * as.double(clgdata$AVGFACSAL)

colMeans(clgdata[-1])

attach(clgdata)

library(leaps) # for regsubsets
library(glmnet) # for LASSO, ridge regression, mtcars data

set.seed(4574)
train = sample(1:nrow(clgdata),0.75*nrow(clgdata))
test = -train
clgdata = clgdata[-1]
clgdata.train = clgdata[train,]
clgdata.test = clgdata[test,]

mean(clgdata.train$SAT_AVG)
mean(clgdata.test$SAT_AVG)

# 5-fold cross-validation
k = 5 #number of folds
p = 8 #number of predictors in dataset 
cv.errors=array(NA,dim=c(k,p)) 
folds=sample(1:k,p,replace=TRUE)

predict.regsubsets=function(regfit.full,newdata,t){
  form=as.formula(regfit.full$call[[2]])
  mat=model.matrix(form,newdata) #mat = model.matrix(mpg~., newdata)
  coefi=coef(regfit.full,id=t) #obtain the coefficients of the model corresponding to t
  xvars=names(coefi)
  pred = mat[,xvars]%*%coefi
  return(pred)
}

for(j in 1:k){
  best.fit = regsubsets(SAT_AVG~.,data=clgdata.train[folds!=j,],nvmax=p)
  for(t in 1:p){
    pred = predict.regsubsets(best.fit,clgdata.train[folds==j,],t)
    actual = clgdata.train$SAT_AVG[folds==j]
    cv.errors[j,t] = mean((actual-pred)^2)
  }
}

# Best subset selection: subset 1-8
regfit.full=regsubsets(SAT_AVG~.,data=clgdata.train, nvmax=p)
mean.cv.errors=apply(cv.errors,2,mean)
best.model = which.min(mean.cv.errors)
coef(regfit.full, best.model)


# Use 5-fold CV to choose the best value of lambda for lasso regression
x = model.matrix(SAT_AVG~.,clgdata)
y = clgdata$SAT_AVG
grid=c(10^(-3:3),0) 

cv.out = cv.glmnet(x[train,], y[train], alpha=1, lambda=grid, nfolds=k) 
bestlam = cv.out$lambda.min

lasso.mod = glmnet(x[train,], y[train], alpha=1, lambda=bestlam)

# Evaluate
pred = predict(lasso.mod,x[test,])
actual = y[test]
mean((actual-pred)^2) 
