rm(list = ls())
ourdata= read.csv("GroupFinalDataSet.csv", header = T, na.strings = c("NA","?"))
sum(is.na(ourdata)) #Count the NAs in our data set
sapply(ourdata, function(y)sum(length(which(is.na(y))))) #See where the NAs occur

#Data frame with murdperpop, rapesperpop, robbperpop, assaultperpop, burgperpop, larcperpop,
#autotheftperpop, arsonsperpop, and nonviolentcrimeperpop
dfbig <- ourdata[,c(6:15, 17:24, 26:32, 34:50, 52:54, 56, 62, 66:68, 70:84, 89:91, 
                    93:103, 121:123, 131, 133, 135, 137, 139, 141, 143, 145, 146, 147)]

#Data frame without murdperpop, rapesperpop, robbperpop, assaultperpop, burgperpop, larcperpop,
#autotheftperpop, arsonsperpop, and nonviolentcrimeperpop
dfsmall <- ourdata[,c(6:15, 17:24, 26:32, 34:50, 52:54, 56, 62, 66:68, 70:84, 89:91, 93:103, 121:123, 147)]

dfsmall <- na.omit(dfsmall)

#We had 2,215 rows and by omitting all NAs we only decrease to 1,993 rows, which is acceptable
x <- dfsmall[, -c(83)]
y <- dfsmall[,c(83)]

#standarlize x, not y
scaledx=scale(x,center=TRUE, scale=TRUE)

rm(ourdata)

ViolentCrimeCat <- rep(0, length(y))
ViolentCrimeCat[y > median(y)] <- 1
ViolentCrimeCat[y <= median(y)] <- 0
df <- data.frame(scaledx, ViolentCrimeCat)


###########################
#feature selection by lasso
###########################

library(glmnet)
set.seed(1)
lassox=model.matrix(df$ViolentCrimeCat~.,df)[,-1]
lassoy=as.factor(df$ViolentCrimeCat)

grid <- 10^seq(10,-2,length=100)
lasso.mod <- glmnet(lassox, lassoy, alpha=1, lambda=grid, family="binomial")
plot(lasso.mod)

cv.out <- cv.glmnet(lassox, lassoy, alpha=1, family="binomial")
plot(cv.out)
bestlam <- cv.out$lambda.min

out <- glmnet(lassox, lassoy, alpha=1, lambda = grid, family="binomial")

lasso.coef <- predict(out, type="coefficients", s = bestlam)[1:82,]
lasso.coef
names(lasso.coef[lasso.coef!=0])[-1]

#data with selected variables
x<-df[,names(lasso.coef[lasso.coef!=0])[-1]]
y<-as.factor(ViolentCrimeCat)
data<-cbind(x,y)

####################
#Logistic Model#####
####################
trainErrorRate<-NULL
OverallErrorRate<-NULL
type1FalsePositiveErrorRate<-NULL
type2FalseNegativeErrorRate<-NULL
sensitivityPowerRecall<-NULL
specificityTrueNegative<-NULL
precision<-NULL

set.seed(1)
n <- nrow(data)
mydf <- data[sample(1:n, n),]
numfolds <- 10
fold.indices <- cut(1:n, breaks=numfolds, labels=FALSE)

for(i in 1:numfolds){
  
  test.indices <- which(fold.indices == i)
  test.data <- mydf[test.indices, ]
  train.data <- mydf[-test.indices, ]
  
  fit.glm.train<-glm(y~.,data=train.data,family=binomial)
  probs.train<-predict(fit.glm.train,train.data,type="response")
  pred.glm.train<-rep(0,length(probs.train))
  pred.glm.train[probs.train>0.5]<-1
  
  fit.glm<-glm(y~.,data=train.data,family=binomial)
  probs<-predict(fit.glm,test.data,type="response")
  pred.glm<-rep(0,length(probs))
  pred.glm[probs>0.5]<-1
  
  confusion.table.train=table(train.data$y,pred.glm.train)
  trainErrorRate[i] <- (confusion.table.train[2] + confusion.table.train[3]) / sum(confusion.table.train)
  confusion.table=table(test.data$y,pred.glm)
  OverallErrorRate[i] <- (confusion.table[2] + confusion.table[3]) / sum(confusion.table)
  type1FalsePositiveErrorRate[i] <- confusion.table[1, 2] / sum(confusion.table[1, ])
  type2FalseNegativeErrorRate[i] <- confusion.table[2, 1] / sum(confusion.table[2, ])
  sensitivityPowerRecall[i] <- confusion.table[2, 2] / sum(confusion.table[2, ])
  specificityTrueNegative[i] <- confusion.table[1, 1] / sum(confusion.table[1, ])
  precision[i] <- confusion.table[2,2] / sum(confusion.table[, 2])  
}


hist(OverallErrorRate, col="light blue", main="10-fold CV Overall Error Rate of Logistic Model")

mean(OverallErrorRate)
mean(trainErrorRate)
mean(type1FalsePositiveErrorRate)
mean(type2FalseNegativeErrorRate)
mean(sensitivityPowerRecall)
mean(specificityTrueNegative)
mean(precision)

###################
#KNN with various k
###################

library(FNN)

ls1<-NULL
ls2<-NULL
t1e<-NULL
t2e<-NULL
powerls<-NULL
specif<-NULL
precisionls<-NULL

set.seed(1)
for (j in 1:30){
  trainErrorRate<-NULL
  OverallErrorRate<-NULL
  type1FalsePositiveErrorRate<-NULL
  type2FalseNegativeErrorRate<-NULL
  sensitivityPowerRecall<-NULL
  specificityTrueNegative<-NULL
  power<-NULL
  precision<-NULL
  
  
  n <- nrow(data)
  mydf <- data[sample(1:n, n),]
  numfolds <- 10
  fold.indices <- cut(1:n, breaks=numfolds, labels=FALSE)
  
  for(i in 1:numfolds){ 
    test.indices <- which(fold.indices == i)
    test.data <- mydf[test.indices, ]
    train.data <- mydf[-test.indices, ]
    train.x<-train.data[,1:ncol(mydf)-1]
    train.y<-train.data[,ncol(mydf)]
    test.x<-test.data[,1:ncol(mydf)-1]
    test.y<-test.data[,ncol(mydf)]
    
    pred.knn.train <- knn(train.x, train.x, train.y, k = j)
    pred.knn <- knn(train.x, test.x, train.y, k = j)
    
    confusion.table.train=table(train.y,pred.knn.train)
    trainErrorRate[i] <- (confusion.table.train[2] + confusion.table.train[3]) / sum(confusion.table.train)
    confusion.table=table(test.y,pred.knn)
    OverallErrorRate[i] <- (confusion.table[2] + confusion.table[3]) / sum(confusion.table)
    type1FalsePositiveErrorRate[i] <- confusion.table[1, 2] / sum(confusion.table[1, ])
    type2FalseNegativeErrorRate[i] <- confusion.table[2, 1] / sum(confusion.table[2, ])
    sensitivityPowerRecall[i] <- confusion.table[2, 2] / sum(confusion.table[2, ])
    specificityTrueNegative[i] <- confusion.table[1, 1] / sum(confusion.table[1, ])
    precision[i] <- confusion.table[2,2] / sum(confusion.table[, 2])  
  }
  
  ls1[j]=mean(OverallErrorRate)
  ls2[j]=mean(trainErrorRate)
  t1e[j]=mean(type1FalsePositiveErrorRate)
  t2e[j]=mean(type2FalseNegativeErrorRate)
  powerls[j]=mean(sensitivityPowerRecall)
  specif[j]=mean(specificityTrueNegative)
  precisionls[j]=mean(precision)
}



which.min(ls1)
ls1[which.min(ls1)]
ls2[which.min(ls1)]
t1e[which.min(ls1)]
t2e[which.min(ls1)]
powerls[which.min(ls1)]
specif[which.min(ls1)]
precisionls[which.min(ls1)]


plot(NULL, NULL, type='n', xlim=c(1, 29), ylim=c(0,max(c(ls1, ls2))), xlab='Increasing Flexibility (Decreasing k)', ylab='Overall Error Rate', main='Overall Error Rate as a Function of \n Flexibility for KNN Classficiation')
lines(1:30, ls1, type='b', col=2, pch=16)
lines(1:30, ls2, type='b', col=1, pch=16)
legend("topright", legend = c("Test Error Rate", "Train Error Rate"), col=c(2, 1), cex=.75, pch=16)


##########
#LDA Model
##########

library(MASS)

trainErrorRate<-NULL
OverallErrorRate<-NULL
type1FalsePositiveErrorRate<-NULL
type2FalseNegativeErrorRate<-NULL
sensitivityPowerRecall<-NULL
specificityTrueNegative<-NULL
power<-NULL
precision<-NULL


set.seed(1)
mydf <- data
n <- nrow(mydf)
mydf <- mydf[sample(1:n, n),]
numfolds <- 10
fold.indices <- cut(1:n, breaks=numfolds, labels=FALSE)

for(i in 1:numfolds){
  
  test.indices <- which(fold.indices == i)
  test.data <- mydf[test.indices, ]
  train.data <- mydf[-test.indices, ]
  
  fit.lda <- lda(y ~., data = train.data)
  pred.lda <- predict(fit.lda, test.data)
  pred.lda.train<-predict(fit.lda, train.data)
  
  confusion.table.train=table(train.data$y,pred.lda.train$class)
  trainErrorRate[i] <- (confusion.table.train[2] + confusion.table.train[3]) / sum(confusion.table.train)
  confusion.table=table(test.data$y,pred.lda$class)
  OverallErrorRate[i]=(confusion.table[2] + confusion.table[3]) / sum(confusion.table)
  type1FalsePositiveErrorRate[i] <- confusion.table[1, 2] / sum(confusion.table[1, ])
  type2FalseNegativeErrorRate[i] <- confusion.table[2, 1] / sum(confusion.table[2, ])
  sensitivityPowerRecall[i] <- confusion.table[2, 2] / sum(confusion.table[2, ])
  specificityTrueNegative[i] <- confusion.table[1, 1] / sum(confusion.table[1, ])
  precision[i] <- confusion.table[2,2] / sum(confusion.table[, 2]) 
}

hist(OverallErrorRate,col="light blue",main="10-fold CV Overall Error Rate of LDA")

mean(OverallErrorRate)
mean(trainErrorRate)
mean(type1FalsePositiveErrorRate)
mean(type2FalseNegativeErrorRate)
mean(sensitivityPowerRecall)
mean(specificityTrueNegative)
mean(precision)

##########
#QDA Model
##########

trainErrorRate<-NULL
OverallErrorRate<-NULL
type1FalsePositiveErrorRate<-NULL
type2FalseNegativeErrorRate<-NULL
sensitivityPowerRecall<-NULL
specificityTrueNegative<-NULL
power<-NULL
precision<-NULL

set.seed(1)
mydf <- data
n <- nrow(mydf)
mydf <- mydf[sample(1:n, n),]
numfolds <- 10
fold.indices <- cut(1:n, breaks=numfolds, labels=FALSE)

for(i in 1:numfolds){
  test.indices <- which(fold.indices == i)
  test.data <- mydf[test.indices, ]
  train.data <- mydf[-test.indices, ]
  
  fit.qda <- qda(y ~., data = train.data)
  pred.qda <- predict(fit.qda, test.data)
  pred.qda.train<-predict(fit.qda, train.data)
  
  confusion.table.train=table(train.data$y,pred.qda.train$class)
  trainErrorRate[i] <- (confusion.table.train[2] + confusion.table.train[3]) / sum(confusion.table.train)
  confusion.table=table(test.data$y,pred.qda$class)
  OverallErrorRate[i]=(confusion.table[2] + confusion.table[3]) / sum(confusion.table)
  type1FalsePositiveErrorRate[i] <- confusion.table[1, 2] / sum(confusion.table[1, ])
  type2FalseNegativeErrorRate[i] <- confusion.table[2, 1] / sum(confusion.table[2, ])
  sensitivityPowerRecall[i] <- confusion.table[2, 2] / sum(confusion.table[2, ])
  specificityTrueNegative[i] <- confusion.table[1, 1] / sum(confusion.table[1, ])
  precision[i] <- confusion.table[2,2] / sum(confusion.table[, 2]) 
}


hist(OverallErrorRate,col="light blue",main="10-fold CV Overall Error Rate of QDA")

mean(OverallErrorRate)
mean(trainErrorRate)
mean(type1FalsePositiveErrorRate)
mean(type2FalseNegativeErrorRate)
mean(sensitivityPowerRecall)
mean(specificityTrueNegative)
mean(precision)

####################
#PCA to get new data
####################

pr.out<-prcomp(dfsmall[, -c(83)], center = TRUE)
summary(pr.out)

plot(summary(pr.out)$importance[2,], type = "b", main="Proportion of Variance Explained by Each Principal Components", 
     xlab = "Principal Components", ylab = "Proportion")
plot(summary(pr.out)$importance[3,], type = "b", main="Cumulative Proportion Explained", 
     xlab = "Principal Components", ylab = "Proportion")

##################
#Logistic with PCA
##################

lsforerr=NULL
lsfortrainerr=NULL
t1ls=NULL
t2ls=NULL
powerls=NULL
specifls=NULL
precisionls=NULL


library(MASS)
set.seed(1)

for (j in 1:82){
  newdata=data.frame(cbind(pr.out$x[,1:j],ViolentCrimeCat))
  
  trainErrorRate<-NULL
  OverallErrorRate<-NULL
  type1FalsePositiveErrorRate<-NULL
  type2FalseNegativeErrorRate<-NULL
  sensitivityPowerRecall<-NULL
  specificityTrueNegative<-NULL
  power<-NULL
  precision<-NULL
  
  mydf <- newdata
  n <- nrow(newdata)
  mydf <- mydf[sample(1:n, n),]
  numfolds <- 10
  fold.indices <- cut(1:n, breaks=numfolds, labels=FALSE)
  for(i in 1:numfolds){
    test.indices <- which(fold.indices == i)
    test.data <- mydf[test.indices, ]
    train.data <- mydf[-test.indices, ]
    
    fit.glm.train<-glm(ViolentCrimeCat~.,data=train.data,family=binomial)
    probs.train<-predict(fit.glm.train,train.data,type="response")
    pred.glm.train<-rep(0,length(probs.train))
    pred.glm.train[probs.train>0.5]<-1
    
    fit.glm<-glm(ViolentCrimeCat~.,data=train.data,family=binomial)
    probs<-predict(fit.glm,test.data,type="response")
    pred.glm<-rep(0,length(probs))
    pred.glm[probs>0.5]<-1
    
    confusion.table.train=table(train.data$ViolentCrimeCat,pred.glm.train)
    trainErrorRate[i] <- (confusion.table.train[2] + confusion.table.train[3]) / sum(confusion.table.train)
    confusion.table=table(test.data$ViolentCrimeCat,pred.glm)
    OverallErrorRate[i]=(confusion.table[2] + confusion.table[3]) / sum(confusion.table)
    type1FalsePositiveErrorRate[i] <- confusion.table[1, 2] / sum(confusion.table[1, ])
    type2FalseNegativeErrorRate[i] <- confusion.table[2, 1] / sum(confusion.table[2, ])
    sensitivityPowerRecall[i] <- confusion.table[2, 2] / sum(confusion.table[2, ])
    specificityTrueNegative[i] <- confusion.table[1, 1] / sum(confusion.table[1, ])
    precision[i] <- confusion.table[2,2] / sum(confusion.table[, 2]) 
  }
  lsforerr[j]=mean(OverallErrorRate)
  lsfortrainerr[j]=mean(trainErrorRate)
  t1ls[j]=mean(type1FalsePositiveErrorRate)
  t2ls[j]=mean(type2FalseNegativeErrorRate)
  powerls[j]=mean(sensitivityPowerRecall)
  specifls[j]=mean(specificityTrueNegative)
  precisionls[j]=mean(precision)
}


lsforerr[20]
lsfortrainerr[20]
t1ls[20]
t2ls[20]
powerls[20]
specifls[20]
precisionls[20]


plot(lsforerr, type = "b", main="Overall Error rate of Logistic Model by various Principal Components", 
     xlab = "Principal Components", ylab = "Overall Error rate")
plot(lsfortrainerr, type = "b", main="Overall Train Error rate of Logistic Model by various Principal Components", 
     xlab = "Principal Components", ylab = "Overall Train Error rate")



#############
#LDA with PCA
#############

lsforerr=NULL
lsfortrainerr=NULL
t1ls=NULL
t2ls=NULL
powerls=NULL
specifls=NULL
precisionls=NULL


library(MASS)
set.seed(1)

for (j in 1:82){
  newdata=data.frame(cbind(pr.out$x[,1:j],ViolentCrimeCat))

  trainErrorRate<-NULL
  OverallErrorRate<-NULL
  type1FalsePositiveErrorRate<-NULL
  type2FalseNegativeErrorRate<-NULL
  sensitivityPowerRecall<-NULL
  specificityTrueNegative<-NULL
  power<-NULL
  precision<-NULL

  mydf <- newdata
  n <- nrow(newdata)
  mydf <- mydf[sample(1:n, n),]
  numfolds <- 10
  fold.indices <- cut(1:n, breaks=numfolds, labels=FALSE)

  for(i in 1:numfolds){
  
    test.indices <- which(fold.indices == i)
    test.data <- mydf[test.indices, ]
    train.data <- mydf[-test.indices, ]
  
    fit.lda <- lda(ViolentCrimeCat ~., data = train.data)
    pred.lda <- predict(fit.lda, test.data)
    pred.lda.train<-predict(fit.lda, train.data)
  
    confusion.table.train=table(train.data$ViolentCrimeCat,pred.lda.train$class)
    trainErrorRate[i] <- (confusion.table.train[2] + confusion.table.train[3]) / sum(confusion.table.train)
    confusion.table=table(test.data$ViolentCrimeCat,pred.lda$class)
    OverallErrorRate[i]=(confusion.table[2] + confusion.table[3]) / sum(confusion.table)
    type1FalsePositiveErrorRate[i] <- confusion.table[1, 2] / sum(confusion.table[1, ])
    type2FalseNegativeErrorRate[i] <- confusion.table[2, 1] / sum(confusion.table[2, ])
    sensitivityPowerRecall[i] <- confusion.table[2, 2] / sum(confusion.table[2, ])
    specificityTrueNegative[i] <- confusion.table[1, 1] / sum(confusion.table[1, ])
    precision[i] <- confusion.table[2,2] / sum(confusion.table[, 2]) 
  }
  lsforerr[j]=mean(OverallErrorRate)
  lsfortrainerr[j]=mean(trainErrorRate)
  t1ls[j]=mean(type1FalsePositiveErrorRate)
  t2ls[j]=mean(type2FalseNegativeErrorRate)
  powerls[j]=mean(sensitivityPowerRecall)
  specifls[j]=mean(specificityTrueNegative)
  precisionls[j]=mean(precision)
}

plot(lsforerr, type = "b", main="Overall Error rate of LDA by various Principal Components", 
     xlab = "Principal Components", ylab = "Overall Error rate")
plot(lsfortrainerr, type = "b", main="Overall Train Error rate of LDA by various Principal Components", 
     xlab = "Principal Components", ylab = "Overall Train Error rate")


lsforerr[20]
lsfortrainerr[20]
t1ls[20]
t2ls[20]
powerls[20]
specifls[20]
precisionls[20]

#############
#QDA with PCA 
#############

library(MASS)
lsforerr=NULL
lsfortrainerr=NULL
t1ls=NULL
t2ls=NULL
powerls=NULL
specifls=NULL
precisionls=NULL

set.seed(1)
for (j in 1:82){
newdata=data.frame(cbind(pr.out$x[,1:j],ViolentCrimeCat))

trainErrorRate<-NULL
OverallErrorRate<-NULL
type1FalsePositiveErrorRate<-NULL
type2FalseNegativeErrorRate<-NULL
sensitivityPowerRecall<-NULL
specificityTrueNegative<-NULL
power<-NULL
precision<-NULL

n <- nrow(newdata)
mydf <- newdata[sample(1:n, n),]
numfolds <- 10
fold.indices <- cut(1:n, breaks=numfolds, labels=FALSE)
for(i in 1:numfolds){
  test.indices <- which(fold.indices == i)
  test.data <- mydf[test.indices, ]
  train.data <- mydf[-test.indices, ]
  
  fit.qda <- qda(ViolentCrimeCat~., data = train.data)
  pred.qda <- predict(fit.qda, test.data)
  pred.qda.train<-predict(fit.qda, train.data)
  
  confusion.table.train=table(train.data$ViolentCrimeCat,pred.qda.train$class)
  trainErrorRate[i] <- (confusion.table.train[2] + confusion.table.train[3]) / sum(confusion.table.train)
  confusion.table=table(test.data$ViolentCrimeCat,pred.qda$class)
  OverallErrorRate[i]=(confusion.table[2] + confusion.table[3]) / sum(confusion.table)
  type1FalsePositiveErrorRate[i] <- confusion.table[1, 2] / sum(confusion.table[1, ])
  type2FalseNegativeErrorRate[i] <- confusion.table[2, 1] / sum(confusion.table[2, ])
  sensitivityPowerRecall[i] <- confusion.table[2, 2] / sum(confusion.table[2, ])
  specificityTrueNegative[i] <- confusion.table[1, 1] / sum(confusion.table[1, ])
  precision[i] <- confusion.table[2,2] / sum(confusion.table[, 2]) 
}
lsforerr[j]=mean(OverallErrorRate)
lsfortrainerr[j]=mean(trainErrorRate)
t1ls[j]=mean(type1FalsePositiveErrorRate)
t2ls[j]=mean(type2FalseNegativeErrorRate)
powerls[j]=mean(sensitivityPowerRecall)
specifls[j]=mean(specificityTrueNegative)
precisionls[j]=mean(precision)
}

lsforerr[20]
lsfortrainerr[20]
t1ls[20]
t2ls[20]
powerls[20]
specifls[20]
precisionls[20]


plot(lsforerr, type = "b", main="Overall Error rate of QDA by various Principal Components", 
     xlab = "Principal Components", ylab = "Overall Error rate")
plot(lsfortrainerr, type = "b", main="Overall Train Error rate of LDA by various Principal Components", 
     xlab = "Principal Components", ylab = "Overall Train Error rate")


############################
#KNN with best k=26 with PCA
############################

library(FNN)

ls1<-NULL
ls2<-NULL
t1e<-NULL
t2e<-NULL
powerls<-NULL
specif<-NULL
precisionls<-NULL

set.seed(1)
for (j in 1:82){
  trainErrorRate<-NULL
  OverallErrorRate<-NULL
  type1FalsePositiveErrorRate<-NULL
  type2FalseNegativeErrorRate<-NULL
  sensitivityPowerRecall<-NULL
  specificityTrueNegative<-NULL
  power<-NULL
  precision<-NULL
  
  newdata=data.frame(cbind(pr.out$x[,1:82],ViolentCrimeCat))
  n <- nrow(newdata)
  mydf <- newdata[sample(1:n, n),]
  numfolds <- 10
  fold.indices <- cut(1:n, breaks=numfolds, labels=FALSE)
  
  for(i in 1:numfolds){ 
    test.indices <- which(fold.indices == i)
    test.data <- mydf[test.indices, ]
    train.data <- mydf[-test.indices, ]
    train.x<-train.data[,1:ncol(mydf)-1]
    train.y<-train.data[,ncol(mydf)]
    test.x<-test.data[,1:ncol(mydf)-1]
    test.y<-test.data[,ncol(mydf)]
    
    pred.knn.train <- knn(train.x, train.x, train.y, k = 26)
    pred.knn <- knn(train.x, test.x, train.y, k = 26)
    
    confusion.table.train=table(train.y,pred.knn.train)
    trainErrorRate[i] <- (confusion.table.train[2] + confusion.table.train[3]) / sum(confusion.table.train)
    confusion.table=table(test.y,pred.knn)
    OverallErrorRate[i] <- (confusion.table[2] + confusion.table[3]) / sum(confusion.table)
    type1FalsePositiveErrorRate[i] <- confusion.table[1, 2] / sum(confusion.table[1, ])
    type2FalseNegativeErrorRate[i] <- confusion.table[2, 1] / sum(confusion.table[2, ])
    sensitivityPowerRecall[i] <- confusion.table[2, 2] / sum(confusion.table[2, ])
    specificityTrueNegative[i] <- confusion.table[1, 1] / sum(confusion.table[1, ])
    precision[i] <- confusion.table[2,2] / sum(confusion.table[, 2])  
  }
  
  ls1[j]=mean(OverallErrorRate)
  ls2[j]=mean(trainErrorRate)
  t1e[j]=mean(type1FalsePositiveErrorRate)
  t2e[j]=mean(type2FalseNegativeErrorRate)
  powerls[j]=mean(sensitivityPowerRecall)
  specif[j]=mean(specificityTrueNegative)
  precisionls[j]=mean(precision)
}

ls1[20]
ls2[20]
t1e[20]
t2e[20]
powerls[20]
specif[20]
precisionls[20]


plot(NULL, NULL, type='n', xlim=c(1,81), ylim=c(0,max(c(ls1, ls2))), xlab='Principal Components', ylab='Overall Error Rate', main='Overall Error rate of KNN(k=26) by various Principal Components')
lines(1:82, ls1, type='b', col=2, pch=16)
lines(1:82, ls2, type='b', col=1, pch=16)
legend("bottomright", legend = c("Test Error Rate", "Train Error Rate"), col=c(2, 1), cex=.75, pch=16)

