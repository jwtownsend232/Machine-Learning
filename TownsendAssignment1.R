rm(list = ls())

library('ISLR')
library('e1071')
library('MASS')
library('gam')
library('boot')

#################################
# PART 1 ########################
#################################
##############
### Part 1a ##
##############

set.seed(5082)
n <- dim(OJ)[1]
train_inds <- sample(1:n,800)
test_inds <- (1:n)[-train_inds]
train <- OJ[train_inds,]
test <- OJ[test_inds,]
 
##############
### Part 1b ##
##############

svmfit <- svm(Purchase ~., data = train, kernel = "linear", cost = .01, scale = FALSE)
summary(svmfit)
# here we can see the cost of .01 was used, and the model resulted in 614 support vectors. 309 were in one class
# and 305 were in the other. This is expected to be high because the margin is going to be quite big considering
# how small the cost is

##############
### Part 1c ##
##############

pred.train <- predict(svmfit, train)
train.table <- table(truth = train$Purchase, predict = pred.train)
print(train.table)
print(paste('The train error rate is', sum(train.table[2,1], train.table[1,2]) / sum(train.table)))

pred.test <- predict(svmfit, test)
test.table <- table(truth = test$Purchase, predict = pred.test)
print(test.table)
print(paste('The test error rate is', sum(test.table[2,1], test.table[1,2]) / sum(test.table)))

##############
### Part 1d ##
##############

costs <- c(0.01,0.05,.1,.5,1,5,10)
tune.out <- tune(svm, Purchase ~ ., data=train, kernel="linear", ranges=costs)
best.mod <- tune.out$best.model
summary(best.mod) # best cost is 1

##############
### Part 1e ##
##############

pred.b.train <- predict(best.mod, train)
train.b.table <- table(truth = train$Purchase, predict = pred.b.train)
print(train.b.table)
print(paste('The train error rate is', sum(train.b.table[2,1], train.b.table[1,2]) / sum(train.b.table)))

pred.b.test <- predict(best.mod, test)
test.b.table <- table(truth = test$Purchase, predict = pred.b.test)
print(test.b.table)
print(paste('The test error rate is', sum(test.b.table[2,1], test.b.table[1,2]) / sum(test.b.table)))
test.errors <- sum(test.b.table[2,1], test.b.table[1,2]) / sum(test.b.table)
  
##############
### Part 1f ##
##############

svmfit <- svm(Purchase ~., data = train, kernel = "radial", cost = .01, scale = FALSE)
summary(svmfit)
# THe SVM model resulted in 626 support vectors of two classes using the radial kernel. 
# 321 were in one class and 305 were in the other. 

pred.train <- predict(svmfit, train)
train.table <- table(truth = train$Purchase, predict = pred.train)
print(train.table)
print(paste('The train error rate is', sum(train.table[2,1], train.table[1,2]) / sum(train.table)))

pred.test <- predict(svmfit, test)
test.table <- table(truth = test$Purchase, predict = pred.test)
print(test.table)
print(paste('The test error rate is', sum(test.table[2,1], test.table[1,2]) / sum(test.table)))

costs <- c(0.01,0.05,.1,.5,1,5,10)
tune.out <- tune.out <- tune(svm, Purchase ~ ., data=train, kernel="radial", ranges=costs)
best.mod <- tune.out$best.model
summary(best.mod)

pred.b.train <- predict(best.mod, train)
train.b.table <- table(truth = train$Purchase, predict = pred.b.train)
print(train.b.table)
print(paste('The train error rate is', sum(train.b.table[2,1], train.b.table[1,2]) / sum(train.b.table)))

pred.b.test <- predict(best.mod, test)
test.b.table <- table(truth = test$Purchase, predict = pred.b.test)
print(test.b.table)
print(paste('The test error rate is', sum(test.b.table[2,1], test.b.table[1,2]) / sum(test.b.table)))
test.errors <- c(test.errors, sum(test.b.table[2,1], test.b.table[1,2]) / sum(test.b.table))

##############
### Part 1g ##
##############

svmfit <- svm(Purchase ~., data = train, kernel = "polynomial", degree = 2, cost = .01, scale = FALSE)
summary(svmfit)
# THe SVM model resulted in 346 support vectors of two classes using the polynomial kernel of degree 2. 
# 172 were in one class and 174 were in the other. 

pred.train <- predict(svmfit, train)
train.table <- table(truth = train$Purchase, predict = pred.train)
print(train.table)
print(paste('The train error rate is', sum(train.table[2,1], train.table[1,2]) / sum(train.table)))

pred.test <- predict(svmfit, test)
test.table <- table(truth = test$Purchase, predict = pred.test)
print(test.table)
print(paste('The test error rate is', sum(test.table[2,1], test.table[1,2]) / sum(test.table)))

costs <- c(0.01,0.05,.1,.5,1,5,10)
tune.out <- tune.out <- tune(svm, Purchase ~ ., data=train, kernel="polynomial", degree = 2, ranges=costs)
best.mod <- tune.out$best.model
summary(best.mod)

pred.b.train <- predict(best.mod, train)
train.b.table <- table(truth = train$Purchase, predict = pred.b.train)
print(train.b.table)
print(paste('The train error rate is', sum(train.b.table[2,1], train.b.table[1,2]) / sum(train.b.table)))

pred.b.test <- predict(best.mod, test)
test.b.table <- table(truth = test$Purchase, predict = pred.b.test)
print(test.b.table)
print(paste('The test error rate is', sum(test.b.table[2,1], test.b.table[1,2]) / sum(test.b.table)))
test.errors <- c(test.errors, sum(test.b.table[2,1], test.b.table[1,2]) / sum(test.b.table))

##############
### Part 1h ##
##############
print(test.errors)

# It's interesting that the best cost for the margin for all three approaches was 1. When using the minimum
# cost of .01, the polynomial model performed the best in terms of having the lowest train and test error rate,
# far better than the classifier and the radial SVM, which had the highest errors. Using the best cost of 1, 
# the best model turns out to be the radial kernel, and the polynomial model is the worst. However, the
# differences between the models is not extreme, with a 3% difference between radial and polynomial degree 2
# using a cost of 1. 

#################################
# PART 2 ########################
#################################
##############
### Part 2b ##
##############

set.seed(5082)
cv.error <- rep(0,10)
for (i in 1:10){
  wage.fit <- glm(wage ~ poly(age,i), data = Wage)
  cv.error[i] <- cv.glm(Wage, wage.fit)$delta[1]
}
cv.error
which.min(cv.error)

##############
### Part 2c ##
##############

plot(1:10, cv.error, xlab = "Polynomial Degree", ylab = "Error Rate", type = "l")
(bestDegree <- which.min(cv.error))

##############
### Part 2d ##
##############

ageLim <- range(Wage$age)
age.grid <- seq(from = ageLim[1], to = ageLim[2])
plot(wage ~ age, data = Wage, xlim = ageLim, cex = .5)
title("Polynomial Fit with Degree 9 Chosen by C.V.", outer = TRUE)
wage.fit <- lm(wage ~ poly(age, 9), data = Wage)
preds <- predict(wage.fit, newdata = list(age = age.grid))
lines(age.grid, preds, lwd = 2, col = "red")

##############
### Part 2e ##
##############

set.seed(5082)
cv.error <- rep(NA, 12)
for (i in 2:12){
  Wage$mycuts <- cut(Wage$age, i) 
  wage.glm.fit <- glm(wage ~ mycuts, data = Wage)
  cv.error[i] <- cv.glm(Wage, wage.glm.fit,  K = 10)$delta[1]
}
cv.error
which.min(cv.error)

##############
### Part 2f ##
##############

plot(2:12, cv.error[-1], xlab = "Number of Cuts", ylab = "Test Error Rate", type = "l")
print("Optimum number of cuts is:")
(optimumCuts <- which.min(cv.error))

##############
### Part 2g ##
##############

ageLim <- range(Wage$age)
age.grid <- seq(from = ageLim[1], to = ageLim[2])
plot(wage ~ age, data = Wage, xlim = ageLim, cex = .5)
title("Step Function Using Number of Cuts (7) Chosen with C.V.", outer = TRUE)
wage.fit <- glm(wage ~ cut(age, 8), data = Wage)
preds <- predict(wage.fit, data.frame(age = age.grid))
lines(age.grid, preds, lwd = 2, col = "red")

#################################
# PART 3 ########################
#################################
##############
### Part 3a ##
##############

disrange <- range(Boston$dis)
dissamples <- seq(from = disrange[1], to = disrange[2], length.out = 100)

##############
### Part 3b ##
##############

train.RSS <- c()
plot(nox ~ dis, data = Boston, col = "black", main = "Polynomial Fit With Various Degrees of Freedom")
degrees <- c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9","D10")
colors <- c("black", "red","green", "orange", "blue", "cyan", "purple", "yellow", "darkgrey", "darkblue")
for (i in 1:10){
  lm.fit <- lm(nox ~ poly(dis, i), data = Boston)
  preds <- predict(lm.fit, list(dis = dissamples))
  train.RSS <- c(train.RSS, (preds - dissamples)^2)
  lines(dissamples, preds, col = colors[i], lwd = 2)
}
legend('topright', legend = degrees, col = colors, xjust = 1, lty = 1, lwd = 3, cex = 0.5)
View(error.table <- cbind(degrees, train.RSS))

##############
### Part 3c ##
##############

set.seed(5082)

k <- 10 
errors <- rep(NA, k)

for (i in 1:k) {
  glm.fit <- glm(nox ~ poly(dis, i), data = Boston)
  errors[i] <- cv.glm(Boston, glm.fit, K = k)$delta[1]
}

(error.min <- which.min(errors))

plot(nox ~ dis, data = Boston, col = "black", main = "Polynomial fit with min cv error at polynomial with degree of 3")
fit <- glm(nox ~ poly(dis, error.min), data = Boston)
preds <- predict(fit, data.frame(dis = dissamples))
lines(dissamples, preds, col = "red", lwd = 2)

##############
### Part 3d ##
##############

fit = lm(nox ~ bs(dis,df=4),data=Boston)
summary(fit)
attr(bs(Boston$dis,df=4), "knots")
# i. the knots are chosen by default as the basis of polynomial regression. Since we have 4 df, it will cut
#     the data in half
# ii. the knot here is placed at the 50th percentile, 3.20745

preds = predict(fit, newdata = list(dis=dissamples), se=TRUE)
plot(Boston$dis, Boston$nox, col="black", main = "Regression Spline with df = 4")
lines(dissamples, preds$fit ,lwd =2, col =" red")

##############
### Part 3e ##
##############

train.RSS <- c()
plot(nox ~ dis, data = Boston, col = "black", main = "Regression Splines with Various Degrees of Freedom")
degrees <- c("D3", "D4", "D5", "D6", "D7", "D8", "D9","D10")
colors <- c("green", "orange", "blue", "cyan", "purple", "yellow", "darkgrey", "darkblue")
for (i in 3:10){
  lm.fit <- lm(nox ~ bs(dis, i), data = Boston)
  preds <- predict(lm.fit, list(dis = dissamples))
  train.RSS <- c(train.RSS, (preds - dissamples)^2)
  lines(dissamples, preds, col = colors[i], lwd = 2)
}
legend('topright', legend = degrees, col = colors, xjust = 1, lty = 1, lwd = 3, cex = 0.5)
View(error.table <- cbind(degrees, train.RSS))

##############
### Part 3f ##
##############

set.seed(5082)

k <- 10 
errors <- rep(NA, k)

for (i in 3:k) {
  glm.fit <- glm(nox ~ bs(dis, i), data = Boston)
  errors[i] <- cv.glm(Boston, glm.fit, K = k)$delta[1]
}

(error.min <- which.min(errors))

plot(nox ~ dis, data = Boston, col = "black", main = "Regression Splines with best df = 10 chosen by C.V.")
fit <- glm(nox ~ bs(dis, error.min), data = Boston)
preds <- predict(fit, data.frame(dis = dissamples))
lines(dissamples, preds, col = "red", lwd = 2)

##############
### Part 3g ##
##############

set.seed(5082)
fit <- smooth.spline(jitter(Boston$dis), Boston$nox, cv = T)
print('Best Lambda')
(fit$lambda)
plot(nox ~ dis, data = Boston, main = "Smoothing spline with best lamda 6.943592e-05 choosen by c.v.")
preds=predict(fit, newdata =list(dis=dis.grid), se=TRUE)
lines(preds$x, preds$y ,lwd =2, col =" red")
