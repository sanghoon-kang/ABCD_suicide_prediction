# Helper packages
library(dplyr)     # for data wrangling
library(ggplot2)   # for awesome plotting
library(rsample)   # for data splitting

# Modeling packages
library(caret)     # for logistic regression modeling

# Model interpretability packages
library(vip)       # variable importance

library(ROCR)

library(glmnet)

library(coefplot)

df <- read.csv('merge_enback.csv')
class(df$Suicidalideation.x)
df <- transform(df, Suicidalideation.x=as.factor(Suicidalideation.x))
df <- subset(df, select = -c(subjectkey))
df <- subset(df, select = -c(X))
df <- na.omit(df)
nrow(df)

## extract survey data
df <- df[ , -grep('tfmri', colnames(df))]

# Create training (70%) and test (30%) sets for the 
# rsample::attrition data.
set.seed(123)  # for reproducibility
churn_split <- initial_split(df, prop = .7, strata = "Suicidalideation.x")
churn_train <- training(churn_split)
churn_test  <- testing(churn_split)

# Dumy code categorical predictor variables
x <- model.matrix(Suicidalideation.x~., churn_train)[,-1]
# Convert the outcome (class) to a numerical variable
y <- ifelse(churn_train$Suicidalideation.x == 1, 1, 0)

# Find the best lambda using cross-validation
set.seed(123) 
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")

model1 <- glmnet(x, y, family = "binomial", alpha = 1, lambda = cv.lasso$lambda.min)
summary(model1)
coef(model1)
coefplot(model1)

# Make predictions on the test data
x.test <- model.matrix(Suicidalideation.x ~., churn_test)[,-1]
probabilities <- model1 %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
# Model accuracy
observed.classes <- churn_test$Suicidalideation.x
mean(predicted.classes == observed.classes)

# Model performance metrics
# model_perf <- data.frame(
#   RMSE = RMSE(predictions, churn_test$Suicidalideation.x),
#   Rsquare = R2(predictions, churn_test$Suicidalideation.x)
# )

##################################

set.seed(123)

cv_model3 <- train(
  Suicidalideation.x ~ ., 
  data = churn_train, 
  method = "glmnet",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)

# extract out of sample performance measures
cv_model3

# predict class
pred_class <- predict(cv_model3, churn_test, type='raw')

# create confusion matrixt
confusionMatrix(
  data = relevel(pred_class, ref = 1), 
  reference = relevel(churn_test$Suicidalideation.x, ref = 1)
)

# Compute predicted probabilities
m3_prob <- predict(cv_model3, churn_test, type = "prob")[,2]

# Compute AUC metrics for cv_model1 and cv_model3
perf2 <- performance(prediction(m3_prob, churn_test$Suicidalideation.x), 
                     measure = "tpr", x.measure = "fpr")

# Plot ROC curves for cv_model1 and cv_model3
plot(perf2, col = "blue", lty=2)
legend(0.8, 0.2, legend = c("cv_model1", "cv_model3"),
       col = c("black", "blue"), lty = 2:1, cex = 0.6)

vip(cv_model3, num_features = 20)
