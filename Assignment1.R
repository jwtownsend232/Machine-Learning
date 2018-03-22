rm(list=ls())

installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}

needed <- c('class', 'FNN')
installIfAbsentAndLoad(needed)

#install.packages('FNN')
####################
#### Question 1 ####
####################

# reading in data, scaling
home.prices <- read.table('HomePrices.txt', sep = '\t', header = T, stringsAsFactors = F)
(MSE <- mean((home.prices$medv - (mean(home.prices$medv)))**2))
(var(home.prices$medv) * (nrow(home.prices) - 1)/nrow(home.prices))
scaled.home.prices <- scale(home.prices[, -c(13)])
scaled.home.prices <- cbind(scaled.home.prices, medv = home.prices$medv)
head(scaled.home.prices, 6)

# setting up for KNN
set.seed(5072)

n <- nrow(scaled.home.prices)
trainprop <- 0.75
validateprop <- 0.15

train  <-  sample(n, trainprop * n)
validate  <-  sample(setdiff(1:n, train), validateprop * n) 
test <- setdiff(setdiff(1:n, train), validate)

head(train,1)
head (validate, 1)
head(test, 1)

trainset <- data.frame(scaled.home.prices[train,])
validateset <- data.frame(scaled.home.prices[validate,])
testset <- data.frame(scaled.home.prices[test,])

train.x <- trainset[-13]
validate.x <- validateset[-13]
test.x <- testset[-13]
train.y <- trainset[13] 
validate.y <- validateset[13]
test.y <- testset[13]

#KNN
library('FNN')
k.range <- seq(19, 1, -2) 
validate.mse <- rep(0, 10)
train.mse <- rep(0, 10)
for (i in 1:10) {
  knn.pred <- knn.reg(train.x, validate.x, train.y, k = k.range[i])
  validate.mse[i] <- mean((validate.y - knn.pred$pred)**2)
  
  knn.pred <- knn.reg(train.x, train.x,  train.y, k = k.range[i])
  train.mse[i] <- mean((train.y - knn.pred$pred)**2)
}

print(paste("My 'best' training MSE occurred with k =", k.range[which.min(train.mse)], "and produced a train MSE of", min(train.mse)))
print(paste("My 'best' validate MSE occurred with k =", k.range[which.min(validate.mse)], "and produced a validate MSE of", min(validate.mse)))

plot(NULL, NULL, type='n', xlim = c(19,1), ylim=c(0,max(c(validate.mse, train.mse))), xlab='Increasing Flexibility (Decreasing k)', ylab='MSE', main='MSE as a Function of \n Flexibility for KNN Regression')
lines(k.range, validate.mse, type='b', col=2, pch=16)
lines(k.range, train.mse, type='b', col=1, pch=16)
legend("topright", legend = c("Validation MSE", "Train MSE"), col=c(2, 1), cex=.75, pch=16)

knn.pred <- knn.reg(train.x, test.x, train.y, k = k.range[which.min(validate.mse)])
test.mse <- mean((test.y - knn.pred$pred)**2)
print(paste("Predicted test MSE occurred with k =", k.range[which.min(validate.mse)], "with a test MSE of", test.mse))

####################
#### Question 2 ####
####################

loan.data <- read.csv('LoanData.csv', sep = ',')
scaled.loan.data <- data.frame(scale(loan.data[1:7]))
scaled.loan.data$loan.repaid <- loan.data$loan.repaid
head(scaled.loan.data, 6)

set.seed(5072)

n <- nrow(scaled.loan.data)
trainprop <- 0.75
validateprop <- 0.15

train  <-  sample(n, trainprop * n)
validate  <-  sample(setdiff(1:n, train), validateprop * n) 
test <- setdiff(setdiff(1:n, train), validate)

head(train,1)
head (validate, 1)
head(test, 1)

trainset <- data.frame(scaled.loan.data[train,])
validateset <- data.frame(scaled.loan.data[validate,])
testset <- data.frame(scaled.loan.data[test,])

train.x <- trainset[-8]
validate.x <- validateset[-8]
test.x <- testset[-8]
train.y <- trainset$loan.repaid
validate.y <- validateset$loan.repaid
test.y <- testset$loan.repaid

#KNN
k.range <- seq(1, 19, 2) 
validate.error <- rep(0, 10)
train.error <- rep(0, 10)
for (i in 1:10) {
  knn.pred <- knn(train.x, validate.x, train.y, k = k.range[i])
  validate.error[i] <- mean(validate.y != knn.pred)
  
  knn.pred <- knn(train.x, train.x, train.y, k = k.range[i])
  train.error[i] <- mean(train.y != knn.pred)
}

print(paste("My 'best' training error rate occurred with k =", k.range[which.min(train.error)], "and produced a training error rate of", min(train.error)))
print(paste("My 'best' validate error rate occurred with k =", k.range[which.min(validate.error)], "and produced a validate error rate of of", min(validate.error)))

plot(NULL, NULL, type='n', xlim = c(19,1), ylim=c(0,max(c(validate.error, train.error))), xlab='Increasing Flexibility (Decreasing k)', ylab='Error', main='Error Rate as a Function of \n Flexibility for KNN Classification')
lines(k.range, validate.error, type='b', col=2, pch=16)
lines(k.range, train.error, type='b', col=1, pch=16)
legend("topleft", legend = c("Validation Error Rate", "Train Error Rate"), col=c(2, 1), cex=.75, pch=16)

knn.pred <- knn(train.x, test.x, train.y, k = k.range[which.min(validate.error)])
test.error <- mean(test.y != knn.pred)
print(paste("Predicted test error rate occurred with k =", k.range[which.min(validate.error)], "with a test error rate of", test.error))


####################
#### Question 3 ####
####################

home.prices <- read.table('HomePrices.txt', sep = '\t', header = T, stringsAsFactors = F)
scaled.home.prices <- scale(home.prices[, -c(13)])
scaled.home.prices <- cbind(scaled.home.prices, medv = home.prices$medv)
head(scaled.home.prices, 6)

# setting up for KNN
set.seed(5072)

n <- nrow(scaled.home.prices)
trainprop <- 0.75
validateprop <- 0.15

#KNN
library('FNN')
k.range <- seq(19, 1, -2) 
validate.mse <- rep(0, 10)
test.mse <- rep(0, 50)
min.validate.mse <- rep(0, 50)
for (i in 1:50) {
  train  <-  sample(n, trainprop * n)
  validate  <-  sample(setdiff(1:n, train), validateprop * n) 
  test <- setdiff(setdiff(1:n, train), validate)
  
  trainset <- data.frame(scaled.home.prices[train,])
  validateset <- data.frame(scaled.home.prices[validate,])
  testset <- data.frame(scaled.home.prices[test,])
  
  train.x <- trainset[-13]
  validate.x <- validateset[-13]
  test.x <- testset[-13]
  train.y <- trainset[13] 
  validate.y <- validateset[13]
  test.y <- testset[13]
  
  
  for (j in 1:10) {
    knn.pred <- knn.reg(train.x, validate.x, train.y, k = k.range[j])
    validate.mse[j] <- mean((validate.y - knn.pred$pred)**2)
  }
  min.validate.mse[i] <- min(validate.mse)
  knn.pred <- knn.reg(train.x, test.x, train.y, k = k.range[which.min(validate.mse)])
  test.mse[i] <- mean((test.y - knn.pred$pred)**2)
}
  
print(paste("My numerical results were as follows: mean.validate.errors =", mean(min.validate.mse)))
print(paste("sd.validate.errors =", sd(min.validate.mse)))
print(paste("mean.test.errors =", mean(test.mse)))
print(paste("sd.test.errors =", sd(test.mse)))

# The validate and test MSEs differ because there is a large difference in the standard deviation of the validate and test sets.
# The test standard deviation is essentially twice that of the validate set, so we would expect it to have a higher MSE.


ylim <- c(0, max(test.mse))
title <- 'Test and Best Validation MSEs for Many Partitionings of the Data'

plot(1:50, min.validate.mse, col = 2, pch = 20, ylim = ylim, 
     main = title, ylab = 'MSEs', xlab = 'Replication')
points(1:50, test.mse, col = 3, pch = 20)
abline(a = mean(min.validate.mse), b = 0, col = 2, lty = 5, lwd = 2)
abline(a = mean(test.mse), b = 0, col = 3, lty = 5, lwd = 2)
lines(1:50, min.validate.mse, col = 2, lty = 2)
lines(1:50, test.mse, col = 3, lty =2)
legend('topright', c('Validation MSEs', 'Validation MSE Mean', 'Test MSEs', 'Test MSE Mean'), 
       col = c(2, 2, 3, 3), pch = c(20, NA, 20, NA), lty = c(2, 5, 2, 5), lwd = c(1, 2, 1, 2))


####################
#### Question 4 ####
####################

capp.data <- read.csv('applications.train.csv', sep = ',')
#Run a MLR on data to see which columns have the most significant effect on applications
fit <- lm(Applications ~ ., data = capp.data)
summary(fit)
# I will use the same approach as #3 to find the best k, using 50 trials with only the
# columns significant to 0.000 as determined by the regression and the initial applications column
capp.data <-capp.data[ , c(1, 2, 4, 6, 10, 15, 17)] 

set.seed(5072)

n <- nrow(capp.data)
trainprop <- 0.75
validateprop <- 0.15

#KNN
library('FNN')
k.range <- seq(19, 1, -2) 
validate.mse <- rep(0, 10)
test.mse <- rep(0, 50)
min.validate.mse <- rep(0, 50)
for (i in 1:50) {
  train  <-  sample(n, trainprop * n)
  validate  <-  sample(setdiff(1:n, train), validateprop * n) 
  test <- setdiff(setdiff(1:n, train), validate)
  
  trainset <- data.frame(capp.data[train,])
  validateset <- data.frame(capp.data[validate,])
  testset <- data.frame(capp.data[test,])
  
  train.x <- trainset[-1]
  validate.x <- validateset[-1]
  test.x <- testset[-1]
  train.y <- trainset[1] 
  validate.y <- validateset[1]
  test.y <- testset[1]
  
  
  for (j in 1:10) {
    knn.pred <- knn.reg(train.x, validate.x, train.y, k = k.range[j])
    validate.mse[j] <- mean((validate.y - knn.pred$pred)**2)
  }
  min.validate.mse[i] <- min(validate.mse)
  knn.pred <- knn.reg(train.x, test.x, train.y, k = k.range[which.min(validate.mse)])
  test.mse[i] <- mean((test.y - knn.pred$pred)**2)
}
k = k.range[which.min(validate.mse)]
print(paste('The optimal k found using this model occurs when k =', k))

