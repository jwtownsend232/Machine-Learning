rm(list=ls())


installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}

needed  <-  c("ISLR", "class", "FNN", "MASS", "boot")
installIfAbsentAndLoad(needed)


##################### 
#### QUESTION 1 ##### 
#####################

set.seed(5072)
dim(Weekly)
dimnames(Weekly)
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial)
summary(glm.fit)
# The intercept and Lag2 are the only significant predictors according to this model

glm.pred <- rep('Down', nrow(Weekly))
glm.pred[predict(glm.fit,type='response')>.5] <- 'Up'
Weekly.table <- table(Weekly$Direction, glm.pred)
Weekly.table


correct <- (Weekly.table[1,1] + Weekly.table[2,2])/sum(Weekly.table)
error <- 1 - correct
t1 <- Weekly.table[1,2]/sum(Weekly.table[1,]) 
t2 <- Weekly.table[2,1]/sum(Weekly.table[2,]) 
power <- 1 - t2
precision <- Weekly.table[2,2]/sum(Weekly.table[,2])

print(paste('The fraction of correct responses is ', correct))
print(paste('The overall error rate is', error))
print(paste('Type I error rate: ', t1))
print(paste('Type II error rate: ', t2))
print(paste('The power of the model is: ', power))
print(paste('The precision of the model: ', precision))

#logistic regression

train <- Weekly$Year < 2009
Weekly.2009 <- Weekly[!train,]
dim(Weekly.2009)
Direction.2009 <- Weekly$Direction[!train]

glm.log <- glm(Direction~Lag2, data = Weekly, family = 'binomial', subset = train)
glm.log.probs <- predict(glm.log, Weekly.2009, type = 'response')

glm.pred.2 <- rep('Down', 104)
glm.pred.2[glm.log.probs>.5] <- 'Up'
Weekly.table.log <- table(Direction.2009,glm.pred.2)
Weekly.table.log

correct.log <- (Weekly.table.log[1,1] + Weekly.table.log[2,2])/sum(Weekly.table.log)
error.log <- 1 - correct.log
t1.log <- Weekly.table.log[1,2]/sum(Weekly.table.log[1,]) 
t2.log <- Weekly.table.log[2,1]/sum(Weekly.table.log[2,]) 
power.log <- 1 - t2
precision.log <- Weekly.table.log[2,2]/sum(Weekly.table.log[,2])

print(paste('The fraction of correct responses is ', correct.log))
print(paste('The overall error rate is', error.log))
print(paste('Type I error rate: ', t1.log))
print(paste('Type II error rate: ', t2.log))
print(paste('The power of the model is: ', power.log))
print(paste('The precision of the model: ', precision.log))

#LDA
lda.fit <-lda(Direction~Lag2, data = Weekly, subset = train) 
lda.pred <- predict(lda.fit, Weekly.2009)
Weekly.table.lda <- table(Direction.2009, lda.pred$class)
Weekly.table.lda

correct.lda <- (Weekly.table.lda[1,1] + Weekly.table.lda[2,2])/sum(Weekly.table.lda)
error.lda <- 1 - correct.lda
t1.lda <- Weekly.table.lda[1,2]/sum(Weekly.table.lda[1,]) 
t2.lda <- Weekly.table.lda[2,1]/sum(Weekly.table.lda[2,]) 
power.lda <- 1 - t2
precision.lda <- Weekly.table.lda[2,2]/sum(Weekly.table.lda[,2])

print(paste('The fraction of correct responses is ', correct.lda))
print(paste('The overall error rate is', error.lda))
print(paste('Type I error rate: ', t1.lda))
print(paste('Type II error rate: ', t2.lda))
print(paste('The power of the model is: ', power.lda))
print(paste('The precision of the model: ', precision.lda))

#QDA
qda.fit <-qda(Direction~Lag2, data = Weekly, subset = train) 
qda.pred <- predict(qda.fit, Weekly.2009)
Weekly.table.qda <- table(Direction.2009, qda.pred$class)
Weekly.table.qda

correct.qda <- (Weekly.table.qda[1,1] + Weekly.table.qda[2,2])/sum(Weekly.table.qda)
error.qda <- 1 - correct.qda
t1.qda <- Weekly.table.qda[1,2]/sum(Weekly.table.qda[1,]) 
t2.qda <- Weekly.table.qda[2,1]/sum(Weekly.table.qda[2,]) 
power.qda <- 1 - t2
precision.qda <- Weekly.table.qda[2,2]/sum(Weekly.table.qda[,2])

print(paste('The fraction of correct responses is ', correct.qda))
print(paste('The overall error rate is', error.qda))
print(paste('Type I error rate: ', t1.qda))
print(paste('Type II error rate: ', t2.qda))
print(paste('The power of the model is: ', power.qda))
print(paste('The precision of the model: ', precision.qda))

#KNN k = 1
knn.pred.1 <- knn(matrix(Weekly$Lag2[train]), matrix(Weekly$Lag2[!train]), matrix(Weekly$Direction[train]), k = 1)
Weekly.table.knn.1 <- table(Direction.2009, knn.pred.1)
Weekly.table.knn.1

correct.k.1 <- (Weekly.table.knn.1[1,1] + Weekly.table.knn.1[2,2])/sum(Weekly.table.knn.1)
error.k.1 <- 1 - correct.k.1
t1.k.1 <- Weekly.table.knn.1[1,2]/sum(Weekly.table.knn.1[1,]) 
t2.k.1 <- Weekly.table.knn.1[2,1]/sum(Weekly.table.knn.1[2,]) 
power.k.1 <- 1 - t2
precision.k.1 <- Weekly.table.knn.1[2,2]/sum(Weekly.table.knn.1[,2])

print(paste('The fraction of correct responses is ', correct.k.1))
print(paste('The overall error rate is', error.k.1))
print(paste('Type I error rate: ', t1.k.1))
print(paste('Type II error rate: ', t2.k.1))
print(paste('The power of the model is: ', power.k.1))
print(paste('The precision of the model: ', precision.k.1))

#KNN k = 5
knn.pred.5 <- knn(matrix(Weekly$Lag2[train]), matrix(Weekly$Lag2[!train]), matrix(Weekly$Direction[train]), k = 5)
Weekly.table.knn.5 <- table(Direction.2009, knn.pred.5)
Weekly.table.knn.5

correct.k.5 <- (Weekly.table.knn.5[1,1] + Weekly.table.knn.5[2,2])/sum(Weekly.table.knn.5)
error.k.5 <- 1 - correct.k.5
t1.k.5 <- Weekly.table.knn.5[1,2]/sum(Weekly.table.knn.5[1,]) 
t2.k.5 <- Weekly.table.knn.5[2,1]/sum(Weekly.table.knn.5[2,]) 
power.k.5 <- 1 - t2
precision.k.5 <- Weekly.table.knn.5[2,2]/sum(Weekly.table.knn.5[,2])

print(paste('The fraction of correct responses is ', correct.k.5))
print(paste('The overall error rate is', error.k.5))
print(paste('Type I error rate: ', t1.k.5))
print(paste('Type II error rate: ', t2.k.5))
print(paste('The power of the model is: ', power.k.5))
print(paste('The precision of the model: ', precision.k.5))

names <- c('Log', 'LDA', 'QDA', 'K = 1', 'K = 5')
Fraction.Correct <- c(correct.log, correct.lda, correct.qda, correct.k.1, correct.k.5)
as.data.frame(Fraction.Correct, names)
# As you can see, both logistic regression and LDA had the highest percentage of correct responses, so I would say they are the best models generally.
# But if you want to use a conservative model, then the KNN k = 1 is best because it has the lowest type 1 error rate.

##################### 
#### QUESTION 2 ##### 
#####################

set.seed(5072)

median <- median(Auto$mpg)
mpg01 <- rep(0, nrow(Auto))
mpg01[Auto$mpg > median] <- 1

Auto <- data.frame(Auto[,-1], mpg01 = mpg01)
Auto <- Auto[,c(1:2,4,9)]

train <- sample(nrow(Auto), .8*nrow(Auto))
test <- setdiff(1:nrow(Auto), train)
trainset <- Auto[train,]
testset <- Auto[-train,]

# Logistic regression
glm.log <- glm(as.factor(mpg01)~cylinders+displacement+weight, family = 'binomial', data = trainset)
glm.log.probs <- predict(glm.log, testset, type = 'response')

glm.pred <- rep('0', length(glm.log.probs))
glm.pred[glm.log.probs > .5] <- '1'
Auto.table.log <- table(testset$mpg01,glm.pred)
Auto.table.log

correct.log <- (Auto.table.log[1,1] + Auto.table.log[2,2])/sum(Auto.table.log)
error.log <- 1 - correct.log
t1.log <- Auto.table.log[1,2]/sum(Auto.table.log[1,]) 
t2.log <- Auto.table.log[2,1]/sum(Auto.table.log[2,]) 
power.log <- 1 - t2
precision.log <- Auto.table.log[2,2]/sum(Auto.table.log[,2])

print(paste('The fraction of correct responses is ', correct.log))
print(paste('The overall error rate is', error.log))
print(paste('Type I error rate: ', t1.log))
print(paste('Type II error rate: ', t2.log))
print(paste('The power of the model is: ', power.log))
print(paste('The precision of the model: ', precision.log))

# LDA
lda.fit <-lda(as.factor(mpg01)~cylinders+displacement+weight, data = trainset) 
lda.pred <- predict(lda.fit, testset)
Auto.table.lda <- table(testset$mpg01, lda.pred$class)
Auto.table.lda

correct.lda <- (Auto.table.lda[1,1] + Auto.table.lda[2,2])/sum(Auto.table.lda)
error.lda <- 1 - correct.lda
t1.lda <- Auto.table.lda[1,2]/sum(Auto.table.lda[1,]) 
t2.lda <- Auto.table.lda[2,1]/sum(Auto.table.lda[2,]) 
power.lda <- 1 - t2
precision.lda <- Auto.table.lda[2,2]/sum(Auto.table.lda[,2])

print(paste('The fraction of correct responses is ', correct.lda))
print(paste('The overall error rate is', error.lda))
print(paste('Type I error rate: ', t1.lda))
print(paste('Type II error rate: ', t2.lda))
print(paste('The power of the model is: ', power.lda))
print(paste('The precision of the model: ', precision.lda))

#QDA
qda.fit <-qda(as.factor(mpg01)~cylinders+displacement+weight, data = trainset) 
qda.pred <- predict(qda.fit, testset)
Auto.table.qda <- table(testset$mpg01, qda.pred$class)
Auto.table.qda

correct.qda <- (Auto.table.qda[1,1] + Auto.table.qda[2,2])/sum(Auto.table.qda)
error.qda <- 1 - correct.qda
t1.qda <- Auto.table.qda[1,2]/sum(Auto.table.qda[1,]) 
t2.qda <- Auto.table.qda[2,1]/sum(Auto.table.qda[2,]) 
power.qda <- 1 - t2
precision.qda <- Auto.table.qda[2,2]/sum(Auto.table.qda[,2])

print(paste('The fraction of correct responses is ', correct.qda))
print(paste('The overall error rate is', error.qda))
print(paste('Type I error rate: ', t1.qda))
print(paste('Type II error rate: ', t2.qda))
print(paste('The power of the model is: ', power.qda))
print(paste('The precision of the model: ', precision.qda))

# k = 1
knn.pred.1 <- knn(trainset[ ,-4], testset[ ,-4], matrix(trainset[ ,4]), k = 1)
Auto.table.knn.1 <- table(testset$mpg01, knn.pred.1)
Auto.table.knn.1

correct.k.1 <- (Auto.table.knn.1[1,1] + Auto.table.knn.1[2,2])/sum(Auto.table.knn.1)
error.k.1 <- 1 - correct.k.1
t1.k.1 <- Auto.table.knn.1[1,2]/sum(Auto.table.knn.1[1,]) 
t2.k.1 <- Auto.table.knn.1[2,1]/sum(Auto.table.knn.1[2,]) 
power.k.1 <- 1 - t2
precision.k.1 <- Auto.table.knn.1[2,2]/sum(Auto.table.knn.1[,2])

print(paste('The fraction of correct responses is ', correct.k.1))
print(paste('The overall error rate is', error.k.1))
print(paste('Type I error rate: ', t1.k.1))
print(paste('Type II error rate: ', t2.k.1))
print(paste('The power of the model is: ', power.k.1))
print(paste('The precision of the model: ', precision.k.1))

# multiple k's
correct.k <- c()
for (k in 2:10) {
  knn.pred <- knn(trainset[ ,-4], testset[ ,-4], trainset$mpg01, k = k)
  print(paste('K =', k))
  k.table <- table(testset$mpg01, knn.pred)
  print(k.table)
  correct.k[k] <- (k.table[1,1] + k.table[2,2])/sum(k.table)
  print(paste('Percent Correct =', correct.k[k]))
}
print(paste('The highest rate of True Positives occurs when k =', which.max(correct.k)))

##################### 
#### QUESTION 3 ##### 
#####################

set.seed(5072)

median <- median(Boston$crim)
crim01 <- rep(0, nrow(Boston))
crim01[Boston$crim > median] <- 1

Boston <- data.frame(Boston[,-1], crim01 = crim01)
Boston <- Boston[,c(4,7:8,14)]

train <- sample(nrow(Boston), .8*nrow(Boston))
test <- setdiff(1:nrow(Boston), train)
trainset <- Boston[train,]
testset <- Boston[-train,]

# Logistic regression
glm.log <- glm(crim01~.,family = 'binomial', data = trainset)
glm.log.probs <- predict(glm.log, testset, type = 'response')

glm.pred <- rep('0', length(glm.log.probs))
glm.pred[glm.log.probs > .5] <- '1'
Boston.table.log <- table(testset$crim01,glm.pred)
Boston.table.log

correct.log <- (Boston.table.log[1,1] + Boston.table.log[2,2])/sum(Boston.table.log)
error.log <- 1 - correct.log
t1.log <- Boston.table.log[1,2]/sum(Boston.table.log[1,]) 
t2.log <- Boston.table.log[2,1]/sum(Boston.table.log[2,]) 
power.log <- 1 - t2
precision.log <- Boston.table.log[2,2]/sum(Boston.table.log[,2])

print(paste('The fraction of correct responses is ', correct.log))
print(paste('The overall error rate is', error.log))
print(paste('Type I error rate: ', t1.log))
print(paste('Type II error rate: ', t2.log))
print(paste('The power of the model is: ', power.log))
print(paste('The precision of the model: ', precision.log))

# LDA
lda.fit <-lda(crim01~., data = trainset) 
lda.pred <- predict(lda.fit, testset)
Boston.table.lda <- table(testset$crim01, lda.pred$class)
Boston.table.lda

correct.lda <- (Boston.table.lda[1,1] + Boston.table.lda[2,2])/sum(Boston.table.lda)
error.lda <- 1 - correct.lda
t1.lda <- Boston.table.lda[1,2]/sum(Boston.table.lda[1,]) 
t2.lda <- Boston.table.lda[2,1]/sum(Boston.table.lda[2,]) 
power.lda <- 1 - t2
precision.lda <- Boston.table.lda[2,2]/sum(Boston.table.lda[,2])

print(paste('The fraction of correct responses is ', correct.lda))
print(paste('The overall error rate is', error.lda))
print(paste('Type I error rate: ', t1.lda))
print(paste('Type II error rate: ', t2.lda))
print(paste('The power of the model is: ', power.lda))
print(paste('The precision of the model: ', precision.lda))

# k = 1
knn.pred.1 <- knn(trainset[ ,-4], testset[ ,-4], matrix(trainset[ ,4]), k = 1)
Boston.table.knn.1 <- table(testset$crim01, knn.pred.1)
Boston.table.knn.1

correct.k.1 <- (Boston.table.knn.1[1,1] + Boston.table.knn.1[2,2])/sum(Boston.table.knn.1)
error.k.1 <- 1 - correct.k.1
t1.k.1 <- Boston.table.knn.1[1,2]/sum(Boston.table.knn.1[1,]) 
t2.k.1 <- Boston.table.knn.1[2,1]/sum(Boston.table.knn.1[2,]) 
power.k.1 <- 1 - t2
precision.k.1 <- Boston.table.knn.1[2,2]/sum(Boston.table.knn.1[,2])

print(paste('The fraction of correct responses is ', correct.k.1))
print(paste('The overall error rate is', error.k.1))
print(paste('Type I error rate: ', t1.k.1))
print(paste('Type II error rate: ', t2.k.1))
print(paste('The power of the model is: ', power.k.1))
print(paste('The precision of the model: ', precision.k.1))

# multiple k's
correct.k <- c()
for (k in 2:10) {
  knn.pred <- knn(trainset[ ,-4], testset[ ,-4], trainset$crim01, k = k)
  print(paste('K =', k))
  k.table <- table(testset$crim01, knn.pred)
  print(k.table)
  correct.k[k] <- (k.table[1,1] + k.table[2,2])/sum(k.table)
  print(paste('Percent Correct =', correct.k[k]))
}
print(paste('The highest rate of True Positives occurs when k =', which.max(correct.k)))

names <- c('Log', 'LDA', 'QDA', 'K = 1', 'K = 2')
Fraction.Correct <- c(correct.log, correct.lda, correct.qda, correct.k.1, correct.k[2])
as.data.frame(Fraction.Correct, names)

# In this case, the model with the highest percent correct responses was the KNN when k = 1. The log, and lda models
# both appeared to be particularly worse than KNN, whereas in the previous questions the models did not differ nearly as much.

##################### 
#### QUESTION 4 ##### 
#####################
set.seed(5072)
x <- rnorm(100)
y <- x - 2 * x^2 + rnorm(100)
XY <- data.frame(X = x, Y = y)
plot(XY$X, XY$Y)

set.seed(123)

cv.error <- rep(0,4)

for (i in 1:4) {
  glm.fit <- glm(Y ~ poly(X,i), data = XY)
  print(summary(glm.fit)$coefficients[,4])
  cv.error[i] <- cv.glm(XY, glm.fit)$delta[1]
}
cv.error

set.seed(456)

cv.error.2 <- rep(0,4)

for (j in 1:4) {
  glm.fit <- glm(Y ~ poly(X,j), data = XY)
  print(summary(glm.fit)$coefficients[,4])
  cv.error.2[j] <- cv.glm(XY, glm.fit)$delta[1]
}
cv.error.2

# We get the same results regardless because in LOOCV, every single row is used as a validation set once, so
# no matter the seed, the end result is the same.

# As the data was created using a quadratic function, the model with the lowest LOOCV is the quadratic model.

# Looking at the printed out P-values, we can see that the intercept, linear, and squared term had the best, whereas
# the cubic and  quartic terms are not remotely significant because the population model is a quadratic function.
# Thus, this agrees with the LOOCV results.


