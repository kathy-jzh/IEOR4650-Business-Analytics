setwd("/Users/kathy/Desktop/Columbia University/2019Spring/Business Analytics/HW3")

ojdata = read.csv("OrangeJuice.csv")

set.seed(1337)
ojdata = ojdata[-1]

# train = sample(1:nrow(ojdata),0.5*nrow(ojdata))
# val = sample(1:nrow(ojdata),0.25*nrow(ojdata))
# test = sample(1:nrow(ojdata),0.25*nrow(ojdata))

# idx =  sample(seq(1, 3), size = nrow(ojdata), replace = TRUE, prob = c(.5, .25, .25))
# train = (idx == 1)
# val = (idx == 2)
# test = (idx == 3)

train = sample(nrow(ojdata),as.integer(nrow(ojdata)*0.5))
idxNotTrain <- which(! 1:nrow(ojdata) %in% train )
val = sample(idxNotTrain,as.integer(length(idxNotTrain)*0.5))
test = idxNotTrain[which(! idxNotTrain %in% val)]

ojdata$StoreID = as.factor(ojdata$StoreID)
ojdata$Purchase = as.factor(ojdata$Purchase)
ojdata$SpecialCH = as.factor(ojdata$SpecialCH)
ojdata$SpecialMM = as.factor(ojdata$SpecialMM)

summary(ojdata[train,])

dummies = model.matrix(~ojdata$StoreID)[,c(2:5)]
ojdata = cbind(ojdata, dummies)[-3]

attach(ojdata)

# ojdata = ojdata[-c(11:13)]

# ********************* Logistic Regression **********************************

glm.fit=glm(Purchase~., data=ojdata[train,], family =binomial)
summary (glm.fit)

library(glmnet) # for LASSO, ridge regression, mtcars data

# logistic regression with LASSO
x = model.matrix(Purchase~.,ojdata)
y = as.factor(ojdata$Purchase)

grid=10^(-3:3) #set sequence of lambdas we want to test
cv.out=cv.glmnet(x[train,],y[train],alpha=1,lambda=grid, family="binomial",nfolds=10)
bestlam=cv.out$lambda.min
bestlam

#Train model with best value of lambda on the training set
lasso.mod=glmnet(x[train,], y[train], alpha=1, lambda=bestlam, family="binomial")
coef(lasso.mod)

# *********************** LDA ************************************************

library(MASS)
lda.fit = lda(Purchase~.,data = ojdata,subset=train)
lda.fit

lda.pred=predict(lda.fit, ojdata[-1][train,])
lda.class = lda.pred$class
mean(lda.class!=y[train])


# *********************** KNN ************************************************

library(ISLR)
library(class)

predQuality = vector("numeric",10)
for (nn in 1:10){
  KNNpred = knn(x[train,], x[val,], y[train], k = nn)
  predQuality[nn] = mean(KNNpred != y[val])
}
predQuality

KNNpred = knn(x[train,], x[train,], y[train], k = 4)
mean(KNNpred != y[train])

# *********************** Choose Model ************************************************
KNNpred = knn(x[train,], x[val,], y[train], k = 4)
mean(KNNpred != y[val])

lda.pred=predict(lda.fit, ojdata[-1][val,])
lda.class = lda.pred$class
mean(lda.class!=y[val])

pred = predict(lasso.mod, x[val,],type="class");
mean(pred!=y[val])

lasso.mod=glmnet(x[c(train,val),], y[c(train,val)], alpha=1, lambda=bestlam, family="binomial")
pred = predict(lasso.mod, x[test,],type="class")
mean(pred!=y[test])

# *********************** Confusion Matrix ************************************************

# table to see error rates
classficationTable = table(truth=y[test], 
      predict= pred)
classficationTable

#calculate costs
payoff =  c(c(3.5,0),c(-0.5,0))
sum(classficationTable * payoff)

# find the best threshold
predprob = predict(lasso.mod, x[test,],type="response")
payoffPerThreshold = vector("numeric",100)

for (i in 1:100)
{
  p = 0.01*i
  lgDecision = ifelse(predprob > p,'CH','MM')
  lgDecision
  classficationTable = table(truth=y[test], predict=lgDecision)
  payoffPerThreshold[i] = ifelse(i<=97,sum(classficationTable * payoff), 0)
}

plot(payoffPerThreshold,pch = 15, xlab = "Threshold")
payoffPerThreshold[1]
