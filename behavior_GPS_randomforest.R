library(dplyr)
library(randomForest)
library(caTools)
library(pROC)
library(ROCR)
library(caret)
library(e1071)




#load IV, DV
X2 <- read.csv('behav_short_nona.csv')
View(X2)
nrow(X2)
X2 <- na.omit(X2)
subjectkey <- X2$subjectkey

X <- read.csv('behav_short_nona_num.csv')
nrow(X)
Y <- read.csv('behav_short_nona_binary.csv')
GPS <- read.csv('merge_GPS.csv')
GPS <- na.omit(GPS)
nrow(GPS)
colSums(unique(Y[,]))

X1 <- apply(X, 2, as.numeric)
Y1 <- apply(Y, 2, as.factor)
View(Y)
class(X1[,1])
length(unique(behav_GPS[,'SuicideAttempt.x']))

for (i in 1:ncol(Z)){
  print(length(unique(Z[,i])))
}

X1 <- scale(X1)

Z <- cbind(X1, Y1)

for (i in 1:ncol(behav_GPS)){
  print(class(behav_GPS[,i]))
}

for (i in 1:ncol(behav_GPS)){
  if (length(unique(behav_GPS[,i]))>2){
    behav_GPS[,i] <- as.numeric(behav_GPS[,i])
  }
  else{
    behav_GPS[,i] <- as.factor(behav_GPS[,i])
  }
}

Z$subjectkey <- subjectkey
Z$subjectkey
Z <- data.frame(Z)
View(Z)

Z <- subset(Z, select = -c(SuicideAttempt.x, SelfInjuriousBehaviorwithoutsuicidalintent.x, SuicidalideationPassive.x,
                           SuicidalideationActive.x, PreparatoryActionstowardimminentSuicidalbehavior.x, InterruptedAttempt.x,
                           AbortedAttempt.x))


intersect(Z$subjectkey, GPS$subjectkey)
behav_GPS <- merge(Z, GPS)
behav_GPS <- read.csv('behav_GPS.csv')
behav_GPS <- subset(behav_GPS, select = c(SuicideAttempt.x, Major.Depressive.Disorder.x., Internal.and.External.Comobidity, Conduct.CBCL, Depress.CBCL, TotProb.CBCL, External.CBCL,
                                          AnxDep.CBCL, Aggressive.CBCL, AnxDisord.CBCL, Opposit.CBCL, Social.CBCL, Somatic.CBCL,
                                          Obsessive.Compulsive.Problems..OCD..CBCL, WithDep.CBCL, Internal.CBCL, Attention.CBCL, Thought.CBCL,
                                          Sluggish.Cognitive.Tempo..SCT..CBCL, ADHD.CBCL, RuleBreak.CBCL, SomaticPr.CBCL,INSOMNIA, GENERALHAPPINESS,
                                          GENERALHAPPINESS_HEALTH, ASP, RISK4PC, WORRY, SMOKER, DEPRESSION_SUB, BIP,
                                          CP, SCZ, MDD, CANNABIS, GENERALHAPPINESS_MEANINGFUL, ADHD, EA, DEPRESSION, IQ, ASD,
                                          PTSD, NEUROTICISM, HAPPINESS, SNORING, AD))
behav_GPS <- subset(behav_GPS, select = -c(X))

behav <- read.csv('behav.csv')

ideation <- behav$Suicidalideation.x
sum(ideation)
length(ideation)
sum(ideation)/length(ideation)
attempt <- behav$SuicideAttempt.x
sum(attempt)/length(attempt)

View(behav_GPS)
colSums(is.na(behav_GPS[,]))
nrow(behav_GPS)
behav_GPS <- subset(behav_GPS, select = -c(subjectkey.2,X))
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
  mtry=4,
  ntree=500,
  data=train,
  importance=TRUE,
  na.action=na.omit
)
rf
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
