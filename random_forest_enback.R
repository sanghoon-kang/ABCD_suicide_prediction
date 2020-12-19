library(dplyr)
library(randomForest)
library(caTools)
library(pROC)
library(ROCR)
library(caret)
library(e1071)

#load IV, DV
X <- read.csv('Suicidalideation_enbackfMRI_test.csv')
Y <- read.csv('Suicidalideation_enbackfMRI_train.csv')

head(X)
head(Y)

Z <- rbind(X,Y)
Z <- na.omit(Z)
Z <- subset(Z, select = -c(subjectkey))
Z <- Filter(function(x)!all(is.na(x)), Z)

nrow(Y)
nrow(X)
nrow(Z)
ncol(Z)
X$subjectkey
Y$subjectkey

##merge X and Y
write.csv(Z,'merge_enback.csv')
summary(Z)
sapply(Z, class)
class(Z$Suicidalideation.x)
Z <- transform(Z, Suicidalideation.x=as.factor(Suicidalideation.x))
colSums(is.na(Z))
Z <- subset(Z, select = -c(subjectkey))
ZY <- subset(Z, select = -c(Suicidalideation.x))
nrow(Z)

set.seed(1234)
bestmtry <- tuneRF(ZY, Z$Suicidalideation.x, stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)

##split data


sample = sample.split(Z$Suicidalideation.x, SplitRatio = .8)

train = subset(Z, sample == TRUE)
test = subset(Z, sample == FALSE)

dim(train)
dim(test)

##initialize random forest

rf <- randomForest(
  Suicidalideation.x ~ .,
  mtry=63,
  ntree=500,
  data=train,
  importance=TRUE,
  na.action=na.omit
)

rf$votes
rf.roc <- roc(train$Suicidalideation.x,rf$votes[,2])
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
confusionMatrix(pred,test$Suicidalideation.x)

pred.prob = predict(rf, newdata=test, type='prob')
rf_pred = prediction(pred.prob[,2],test$Suicidalideation.x)

rf_model.perf1 <- performance(rf_pred, 'tpr','fpr')
plot(rf_model.perf1, colorize=TRUE)
abline(a=0, b=1, lty=3)

rf_model.perf2 <- performance(rf_pred,'lift','rpp')
plot(rf_model.perf2,colorize=TRUE)
abline(v=0.4,lty=3)

performance(rf_pred,'auc')@y.values[[1]]

###for newframe (only survey data)

suicide <- Z$Suicidalideation.x
newframe1 <- Z[ , -grep('tfmri', colnames(Z))]
newframe1 <- newframe1[ , -grep('Suicidalideation', colnames(newframe1))]
newframe <- cbind(newframe1, suicide)
newframe <- transform(newframe, suicide=as.factor(suicide))
newframe1 <- transform(newframe1, suicide=as.factor(suicide))
View(newframe1)
View(newframe)

set.seed(1234)
bestmtry <- tuneRF(newframe1, newframe$suicide, stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)

##split data

sample = sample.split(newframe$suicide, SplitRatio = 0.8)

train = subset(newframe, sample == TRUE)
test = subset(newframe, sample == FALSE)

dim(train)
dim(test)

##initialize random forest

rf <- randomForest(
  suicide ~ .,
  mtry=9,
  ntree=500,
  data=train,
  importance=TRUE,
  na.action=na.omit
)
rf
rf$votes
rf.roc <- roc(train$suicide,rf$votes[,2])
plot(rf.roc)
auc(rf.roc)
#mean decrease in accuracy
importance(rf, type=1)

varImpPlot(rf, type=1)

#mean decrease in node impurity
importance(rf, type=2)

varImpPlot(rf, type=2)

# class(Z[,1])
# for (i in 1:ncol(Z)){
#   if (class(Z[,i]) != numeric){
#     print(name(Z[,i]), class(Z[,i]))
#   }
#   else {
#     pass
#   }
# }

##predict with test set

pred = predict(rf, newdata=test, type='response')
confusionMatrix(pred,test$suicide)

pred.prob = predict(rf, newdata=test, type='prob')
rf_pred = prediction(pred.prob[,2],test$suicide)

rf_model.perf1 <- performance(rf_pred, 'tpr','fpr')
plot(rf_model.perf1, colorize=TRUE)
abline(a=0, b=1, lty=3)

rf_model.perf2 <- performance(rf_pred,'lift','rpp')
plot(rf_model.perf2,colorize=TRUE)
abline(v=0.4,lty=3)

performance(rf_pred,'auc')@y.values[[1]]


###for newframe (only mri data)

suicide <- Z$Suicidalideation.x
newframe1 <- Z[ , grep('tfmri', colnames(Z))]
newframe <- cbind(newframe1, suicide)
newframe <- transform(newframe, suicide=as.factor(suicide))
newframe1 <- transform(newframe1, suicide=as.factor(suicide))
View(newframe1)
View(newframe)

set.seed(123)
bestmtry <- tuneRF(newframe1, newframe$suicide, stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)

##split data
set.seed(123)
sample = sample.split(newframe$suicide, SplitRatio = 0.8)

train = subset(newframe, sample == TRUE)
test = subset(newframe, sample == FALSE)

dim(train)
dim(test)

##initialize random forest

rf <- randomForest(
  suicide ~ .,
  mtry=121,
  ntree=500,
  data=train,
  importance=TRUE,
  na.action=na.omit
)
rf
rf$votes
rf.roc <- roc(train$suicide,rf$votes[,2])
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
confusionMatrix(pred,test$suicide)

pred.prob = predict(rf, newdata=test, type='prob')
rf_pred = prediction(pred.prob[,2],test$suicide)

rf_model.perf1 <- performance(rf_pred, 'tpr','fpr')
plot(rf_model.perf1, colorize=TRUE)
abline(a=0, b=1, lty=3)

rf_model.perf2 <- performance(rf_pred,'lift','rpp')
plot(rf_model.perf2,colorize=TRUE)
abline(v=0.4,lty=3)

performance(rf_pred,'auc')@y.values[[1]]
