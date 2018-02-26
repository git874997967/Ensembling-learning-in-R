###load some labs
library(caret)
library(RANN)
##read data
data = read.csv(file.choose())
## analysis names   with NA values
str(data)
summary(data)
aggr(data)
table(data$Credit_History)
### fix the NA values with RANN median
preProValue = preProcess(data, method = c("medianImpute", "center", "scale"))
preProValue$median
data_processed = predict(preProValue, data)
sum(is.na(data_processed))
aggr(data_processed)
summary(data_processed)
summary(data)
##split the train and test dataset
samp = sample(2,
              nrow(data_processed),
              replace = T,
              prob = c(0.75, 0.25))
table(samp)
trainSet = data_processed[samp == 1, ]
testSet = data_processed[samp == 2, ]
### define the train controls and the predictor and outcome variables
fitControl = trainControl(
  method = 'cv',
  number = 10,
  savePredictions = 'final',
  classProbs = T
)
###define the predictors and out come
predictors = c(
  'Credit_History',
  "LoanAmount",
  "Loan_Amount_Term",
  "ApplicantIncome",
  "CoapplicantIncome"
)
outcomeName = "Loan_Status"
#### train the random forest
model_rf = train(
  trainSet[, predictors],
  trainSet[, outcomeName],
  method = 'rf',
  trControl = fitControl,
  tuneLength = 3
)

testSet$pred_rf = predict(model_rf, testSet[, predictors])
###test the model
confusionMatrix(testSet$Loan_Status, testSet$pred_rf)
####train the knn
model_knn = train(
  trainSet[, predictors],
  train[, outcomeName],
  method = 'knn',
  trControl = fitControl,
  tuneLength = 3
)
testSet$pred_knn = predict(object = model_knn, testSet[, predictors])
confusionMatrix(testSet$Loan_Status, testSet$pred_knn)
### train the logistic regression
model_lr = train(
  trainSet[, predictors],
  train[, outcomeName],
  method = 'glm',
  trControl = fitControl,
  tuneLength = 1
)
testSet$pred_lr = predict(object = model_lr, testSet[, predictors])
confusionMatrix(testSet$Loan_Status, testSet$pred_lr)
#### the first method is average the prob  split at 0.5
testSet$pred_rf_prob = predict(object = model_rf, testSet[, predictors], type =
                                 'prob')
testSet$pred_knn_prob = predict(object = model_knn, testSet[, predictors], type =
                                  'prob')
testSet$pred_lr_prob = predict(object = model_lr, testSet[, predictors], type =
                                 'prob')
testSet$pred_avg = (testSet$pred_knn_prob$Y + testSet$pred_lr_prob$Y + testSet$pred_rf_prob$Y) /
  3
testSet$pred_avg = as.factor(ifelse(testSet$pred_avg > 0.5, 'Y', 'N'))
### the second method is major voting   like one more level of knn vote but by models
testSet$pred_majority = as.factor(ifelse(
  testSet$pred_rf == 'Y' &
    testSet$pred_knn == 'Y',
  "Y",
  ifelse(
    testSet$pred_rf == 'Y' &
      testSet$predlr == "Y",
    "Y",
    ifelse(testSet$pred_knn == "Y" &
             testSet$pred_lr == "Y", "Y", "N")
  )
))
testSet$pred_majority[is.na(testSet$pred_majority)]='N'
###the third way is weight average  not as common avg
testSet$pred_majority=as.factor(testSet$pred_majority)

testSet$pred_weighted_avg = (testSet$pred_knn_prob$Y*0.25 + testSet$pred_lr_prob$Y*0.35 + testSet$pred_rf_prob$Y*0.4)  
testSet$pred_weighted_avg=as.factor(ifelse(testSet$pred_weighted_avg>0.5,"Y","N"))
##### evaluate   
confusionMatrix(testSet$Loan_Status, testSet$pred_avg)
confusionMatrix(testSet$Loan_Status, testSet$pred_majority)
confusionMatrix(testSet$Loan_Status, testSet$pred_weighted_avg)




