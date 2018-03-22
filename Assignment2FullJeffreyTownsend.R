rm(list = ls())

##########################################################################################
### Functions
##########################################################################################
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}

calc.cost <- function(predictions, results){
  ret.cost <- sum(predictions == 'Yes') * ret.offer
  table <- table(predictions, results)
  retained <- trunc(prob.success * table[2,2])
  att.cost <- (trunc(table[2,2] - retained) + table[1,2]) * lost.cust 
  total.cost <- ret.cost + att.cost
  error.rate <- (table[1,2] + table[2,1]) / sum(table)
  FP <- (table[2,1]/sum(table[,1]))
  FN <- (table[1,2]/sum(table[,2]))
  TP <- (table[2,2]/sum(table[,2]))
  TN <- (table[1,1]/sum(table[,1]))
  return(list(FP, FN, TP, TN, error.rate = error.rate, total.cost = total.cost, per.cust = total.cost/1006))
}

##############################
### Load required packages ###
##############################
needed  <-  c("ISLR","e1071","boot","rpart","randomForest")      
installIfAbsentAndLoad(needed)

library(rpart)
library(tree)
library(randomForest)
library(ada)
library(gbm)

at.risk <- .26448
ret.offer <- 1600 # one time
lost.cust <- 11500 # one time
prob.success <- .45
prob.fail <- 1 - prob.success

# pre-treat data in Excel, set "No xxx xxx" to No

data <- read.csv('Assignment2TrainingData.csv')
data <- data[complete.cases(data),]
data$SeniorCitizen <- as.factor(data$SeniorCitizen)
data <- data[,-19]
data <- data[,-1]

set.seed(1)
train <- sample(1:nrow(data), (5/6) * nrow(data))
test <- setdiff(1:nrow(data), train)
trainset <- data[train,]
testset <- data[-train,]

cut <- seq(0.01, .99, .01)
decisiontree.results = c()
randomforest.results = c()
boosting.results = c()
results = data.frame()

#################
# decision tree #
#################


# dec.tree <- rpart(Churn ~ ., data = trainset)
# tree.pred <- predict(dec.tree, testset, type = 'prob')
# tree.pred <- tree.pred[,2]
# for(i in 1:length(cut)){
#   dt <- rep("Yes", length(tree.pred))
#   dt[tree.pred < cut[i]] <- "No"
#   dt.res <- calc.cost(factor(dt, levels = c("No", "Yes")), factor(testset$Churn, levels = c("No", "Yes")))
#   entry <- c(cut[i], dt.res$per.cust)
#   randomforest.results <- c(randomforest.results, entry)
# }
# cost.per <- randomforest.results[randomforest.results>1]
# cuts.per <- randomforest.results[randomforest.results<1]
# results <- rbind(results, data.frame(model = 'Decision Tree', cut = cuts.per[which.min(cost.per)], cost = cost.per[which.min(cost.per)]))


#################
# random forest #
#################

# rf <- randomForest(Churn ~ ., data = trainset, importance = TRUE)
# rf.pred <- predict(rf, testset, type = 'prob')
# rf.pred <- rf.pred[,2]
# for(i in 1:length(cut)){
#   dt <- rep("Yes", length(rf.pred))
#   dt[rf.pred < cut[i]] <- "No"
#   dt.res <- calc.cost(factor(dt, levels = c("No", "Yes")), factor(testset$Churn, levels = c("No", "Yes")))
#   entry <- c(cut[i], dt.res$per.cust)
#   decisiontree.results <- c(decisiontree.results, entry)
# }
# cost.per <- decisiontree.results[decisiontree.results>1]
# cuts.per <- decisiontree.results[decisiontree.results<1]
# results <- rbind(results, data.frame(model = 'Random Forest', cut = cuts.per[which.min(cost.per)], cost = cost.per[which.min(cost.per)]))


############
# Boosting #
############

boost <- ada(Churn ~ ., data = trainset)
boost.pred <- predict(boost, testset, type = 'prob')
boost.pred <- boost.pred[,2]
errors <- data.frame()
for(i in 1:length(cut)){
  dt <- rep("Yes", length(boost.pred))
  dt[boost.pred < cut[i]] <- "No"
  dt.res <- calc.cost(factor(dt, levels = c("No", "Yes")), factor(testset$Churn, levels = c("No", "Yes")))
  entry <- c(cut[i], dt.res$per.cust)
  errors <- rbind(errors, data.frame(cut[i], dt.res[[1]], dt.res[[2]], dt.res[[3]], dt.res[[4]], dt.res[[5]])) 
  boosting.results <- c(boosting.results, entry)
}  

## Get Min Cost and associated Cut
cost.per <- boosting.results[boosting.results>1]
cuts.per <- boosting.results[boosting.results<1]
results <- rbind(results, data.frame(model = 'Boosting', cut = cuts.per[which.min(cost.per)], cost = cost.per[which.min(cost.per)]))

## Print Results
names <- c("Cut", "FP", "FN", "TP", "TN", "Error")
colnames(errors) <- names

errors
results

## Best Cut Model 

print("Best Model Confusion Matrix")
dt <- rep("Yes", length(boost.pred))
dt[boost.pred < .35] <- "No"
table(true = testset$Churn, prediction = factor(dt, levels = c("No", "Yes")))

## write CSV
write.csv(errors, file = "ErrorRates.csv")

