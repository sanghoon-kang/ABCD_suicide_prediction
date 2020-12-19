library(dplyr)
library(randomForest)
library(caTools)
library(pROC)
library(ROCR)
library(caret)
library(e1071)

#load IV, DV
X2 <- read.csv('merged_fmri_test_sub.csv')
View(X2)
nrow(X2)

subjectkey <- X2$subjectkey

X <- read.csv('behav_short_nona_num.csv')
nrow(X)
Y <- read.csv('behav_short_nona_binary.csv')


MRI <- read.csv('merged_fMRI_nonsurvey_preprocessed.csv')
MRI1 <- MRI[ , grep('mri', colnames(MRI))]
MRI1$subjectkey <- subjectkey
MRI2 <- na.omit(MRI1)
subjectkey <- MRI2$subjectkey
MRI2 <- subset(MRI2, select = -c(subjectkey))
MRI2 <- data.frame(MRI2)
MRI3 <- apply(MRI2, 2, as.numeric)
MRI3 <- data.frame(MRI3)
MRI4 <- scale(MRI3)
MRI4$subjectkey <- subjectkey
MRI4 <- data.frame(MRI4)

colSums(is.na(MRI4[,]))
nrow(MRI)
nrow(MRI4)

X1 <- apply(X, 2, as.numeric)
X1 <- scale(X1)
Y1 <- apply(Y, 2, as.factor)

Z <- cbind(X1, Y1)

Z$subjectkey <- subjectkey
length(intersect(Z$subjectkey, MRI4$subjectkey))
behav_MRI <- merge(Z, MRI1)
nrow(behav_MRI)

for (i in 1:ncol(behav_MRI)){
  print(class(behav_MRI[,i]))
}

for (i in 1:ncol(behav_MRI)){
  if (length(unique(behav_MRI[,i]))>2){
    behav_MRI[,i] <- as.numeric(behav_MRI[,i])
  }
  else{
    behav_MRI[,i] <- as.factor(behav_MRI[,i])
  }
}

# Z <- subset(Z, select = -c(Suicidalideation.x, SelfInjuriousBehaviorwithoutsuicidalintent.x,SuicidalideationPassive.x,
#                            SuicidalideationActive.x, PreparatoryActionstowardimminentSuicidalbehavior.x,InterruptedAttempt.x,
#                            AbortedAttempt.x))

nrow(behav_MRI)
behav_GPS <- subset(behav_GPS, select = -c(SuicidalideationActive.x,SuicidalideationPassive.x))

##merge X and Y
ZY <- subset(behav_GPS, select = -c(SuicideAttempt.x))
behav_GPS <- data.frame(behav_GPS)
nrow(Z)

set.seed(123)
bestmtry <- tuneRF(ZY, behav_GPS$SuicideAttempt.x, stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)

##split data
set.seed(123)
sample = sample.split(behav_GPS$SuicideAttempt.x, SplitRatio = .8)

train = subset(behav_GPS, sample == TRUE)
test = subset(behav_GPS, sample == FALSE)

dim(train)
dim(test)

##initialize random forest

rf <- randomForest(
  SuicideAttempt.x ~ .,
  mtry=16,
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
