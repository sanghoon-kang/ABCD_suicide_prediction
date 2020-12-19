library(dplyr)
library(randomForest)
library(caTools)
library(pROC)
library(ROCR)
library(caret)
library(e1071)

#load IV, DV
X <- read.csv('SuicideAttempt_interaction_test.csv')
Y <- read.csv('SuicideAttempt_interaction_train.csv')

head(X)
head(Y)

Z <- rbind(X,Y)
Z <- subset(Z, select = -c(subjectkey))
Z <- na.(Z)

nrow(Y)
nrow(X)
nrow(Z)
ncol(Z)
X$subjectkey
Y$subjectkey

##merge X and Y
write.csv(Z,'merge_interaction.csv')
summary(Z)
sapply(Z, class)
class(Z$SuicideAttempt.x)
Z <- transform(Z, SuicideAttempt.x=as.factor(SuicideAttempt.x))
colSums(is.na(Z))
ZY <- subset(Z, select = -c(SuicideAttempt.x))
nrow(Z)

set.seed(123)
bestmtry <- tuneRF(ZY, Z$SuicideAttempt.x, stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)

##split data
set.seed(123)
sample = sample.split(Z$SuicideAttempt.x, SplitRatio = .8)

train = subset(Z, sample == TRUE)
test = subset(Z, sample == FALSE)

dim(train)
dim(test)

##initialize random forest

rf <- randomForest(
  SuicideAttempt.x ~ .,
  mtry=40,
  ntree=500,
  data=train,
  importance=TRUE,
  na.action=na.omit
)

rf$votes
rf.roc <- roc(train$SuicideAttempt.x,rf$votes[,2])
plot(rf.roc)
auc(rf.roc)

#mean decrease in accuracy
importance(rf, type=1)

varImpPlot(rf, type=1)

#mean decrease in node impurity
importance(rf, type=2)

varImpPlot(rf, type=2)

##predict with test set

pred = predict(rf, newdata=test, type='response')
confusionMatrix(pred,test$SuicideAttempt.x)

pred.prob = predict(rf, newdata=test, type='prob')
rf_pred = prediction(pred.prob[,2],test$SuicideAttempt.x)

rf_model.perf1 <- performance(rf_pred, 'tpr','fpr')
plot(rf_model.perf1, colorize=TRUE)
abline(a=0, b=1, lty=3)

rf_model.perf2 <- performance(rf_pred,'lift','rpp')
plot(rf_model.perf2,colorize=TRUE)
abline(v=0.4,lty=3)

performance(rf_pred,'auc')@y.values[[1]]
