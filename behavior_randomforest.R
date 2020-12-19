library(dplyr)
library(randomForest)
library(caTools)
library(pROC)
library(ROCR)
library(caret)
library(e1071)

#load IV, DV
X2 <- read.csv('behav_short.csv')
View(X2)
nrow(X2)
X2 <- na.omit(X2)
subjectkey <- X2$subjectkey

X <- read.csv('behav_short_nona_num.csv')
nrow(X)
Y <- read.csv('behav_short_nona_binary.csv')
colSums(unique(Y[,]))

X1 <- apply(X, 2, as.numeric)
Y1 <- apply(Y, 2, as.factor)
View(Y)
class(X1[,1])
length(unique(Y1[,1]))

for (i in 1:ncol(Z)){
  print(length(unique(Z[,i])))
}

X1 <- scale(X1)

Z <- cbind(X1, Y1)

for (i in 1:ncol(Z)){
  print(class(Z[,i]))
}

for (i in 1:ncol(Z)){
  if (length(unique(Z[,i]))>2){
    Z[,i] <- as.numeric(Z[,i])
  }
  else{
    Z[,i] <- as.factor(Z[,i])
  }
}

Z <- read.csv('behav.csv')
Z$ <- as.numeric(Z[,i])
Z <- data.frame(Z)
View(Z)
Z <- subset(Z, select = -c(SuicideAttempt.x, SelfInjuriousBehaviorwithoutsuicidalintent.x,SuicidalideationPassive.x,
                           SuicidalideationActive.x, PreparatoryActionstowardimminentSuicidalbehavior.x,InterruptedAttempt.x,
                           AbortedAttempt.x))
Z <- subset(Z, select = -c(subjectkey))

##merge X and Y
ZY <- subset(Z, select = -c(Suicidalideation.x))
Z <- data.frame(Z)
nrow(Z)

set.seed(123)
bestmtry <- tuneRF(ZY, Z$Suicidalideation.x, stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)

##split data
set.seed(123)
sample = sample.split(Z$Suicidalideation.x, SplitRatio = .8)

train = subset(Z, sample == TRUE)
test = subset(Z, sample == FALSE)

dim(train)
dim(test)

##initialize random forest

rf <- randomForest(
  Suicidalideation.x ~ .,
  mtry=4,
  ntree=500,
  data=train,
  importance=TRUE,
  na.action=na.omit
)

rf # check for confusion matrix and OOB error
rf$votes
rf.roc <- roc(train$Suicidalideation.x,rf$votes[,2]) # check for AUC
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
