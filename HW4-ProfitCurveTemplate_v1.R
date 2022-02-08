rm(list = ls())

#load packages - may have to add your own package for your model's caret method
#library(data.table)
library(caret)
library(pROC)

#Training data set
traindat <- read.csv("HW4-dsm_housing_Train.csv",
                   stringsAsFactors = FALSE, na.strings = c("NA", ""))
#Testing data set
testdat <- read.csv("HW4-dsm_housing_Test.csv",
                   stringsAsFactors = FALSE, na.strings = c("NA", ""))

#Prediction data set
preddat <- read.csv("HW4-dsm_housing_Pred.csv",
                   stringsAsFactors = FALSE, na.strings = c("NA", ""))




######Pre-Processing######
#change relevant data type and output to ensure it works with a categorical model
traindat$top10per <- as.factor(traindat$top10per)
testdat$top10per <- as.factor(testdat$top10per)
#turn target variable (top10per) column into a variable to input for Y when training the models
assign("trainY", traindat$top10per)
assign("testY", testdat$top10per)
#delete top10per column from data frame so model does not attempt to predict using the target variable itself
traindat$top10per <- NULL
testdat$top10per <- NULL



Sys.time() #RF start
######Random Forest Model######
#specify 5-fold cross validation 
control <- trainControl(method = 'cv', number = 5,classProbs = TRUE, summaryFunction = twoClassSummary) 
#create grid of parameters which train function will use later to identify the ideal parameters for best performance
grid <- expand.grid(mtry = seq(as.integer(sqrt(ncol(traindat))),ncol(traindat),2), 
                    splitrule = "gini", 
                    min.node.size = c(1,5,10,15))
#create random forest model and train using cross validation and a grid of parameters
rfmodel <- train(x = traindat, y = trainY,
               method = "ranger", num.trees = 200, importance = "impurity",
               trControl = control, tuneGrid = grid, metric = "ROC")
#predict using trained model
predictY <- as.numeric(predict(rfmodel, testdat))
#predict probability using trained model
predictYprb <- predict(rfmodel, testdat, type="prob")
#associate probabilites with class for calculating ROC
test = as.data.frame(cbind(testY,predictYprb$Y))
#sort on probability from high to low
test=test[order(-test$V2),]
#calculate auc and plot roc curve
#roc(class,probability)
roc <- roc(test$testY,test$V2)
auc <- auc(roc)
plot <- plot(roc)
assign("RFplot", plot)
RFplot
#save this model
assign("model_rf", rfmodel)
#save this iteration auc
assign("AUC_rf", auc)
Sys.time() #RF end

#let's produce a csv file with all our predictors, target, and probabilities
#so we can build a profit curve
rftest=testdat
rftest$Prob = predictYprb$Y
rftest$top10per = testY
rftest = rftest[order(-rftest$Prob),]
write.csv(rftest, "rftest.csv", row.names = FALSE)

#####prediction - We don't have a target for this data
#predict using trained model
predictY <- as.numeric(predict(rfmodel, preddat))
#predict probability using trained model
predictYprb <- predict(rfmodel, preddat, type="prob")
#let's produce a csv file with all our predictors, target, and probabilities
#so we can build a profit curve
rfpred=preddat
rfpred$Prob = predictYprb$Y
rfpred = rfpred[order(-rfpred$Prob),]
write.csv(rfpred, "rfpred.csv", row.names = FALSE)



Sys.time() #glm start
######Logistic Regression Modeling######
#specify 5-fold cross validation 
control <- trainControl(method = 'cv', number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)
#create grid of parameters which train function will use later to identify the ideal parameters for best performance
#create model and train using cross validation and a grid of parameters
glmmodel <- train(x = traindat, y = trainY,
                  method = "glm", trControl = control, family="binomial", metric = "ROC")
#predict testing data using trained glmmodel
predictY <- as.numeric(predict(glmmodel, testdat))
#predict probability using trained model
predictYprb <- predict(glmmodel, testdat, type="prob")
#associate probabilites with class for calculating ROC
test = as.data.frame(cbind(testY,predictYprb$Y))
#sort on probability from high to low
test=test[order(-test$V2),]
#calculate auc and plot roc curve
#roc(class,probability)
roc <- roc(test$testY,test$V2)
auc <- auc(roc)
#plot our ROC curve
plot <- plot(roc)
assign("GLMplot", plot)
GLMplot
#save this iteration glmmodel
assign("model_glm", glmmodel)
#save this iteration auc
assign("AUC_glm", auc)

#let's produce a csv file with all our predictors, target, and probabilities
#so we can build a profit curve
glmtest=testdat
glmtest$Prob = predictYprb$Y
glmtest$top10per = testY
glmtest = glmtest[order(-glmtest$Prob),]
write.csv(glmtest, "glmtest.csv", row.names = FALSE)

#####prediction - We don't have a target for this data
#predict using trained model
predictY <- as.numeric(predict(glmmodel, preddat))
#predict probability using trained model
predictYprb <- predict(glmmodel, preddat, type="prob")
#let's produce a csv file with all our predictors, target, and probabilities
#so we can build a profit curve
glmpred=preddat
glmpred$Prob = predictYprb$Y
glmpred = glmpred[order(-glmpred$Prob),]
write.csv(glmpred, "glmpred.csv", row.names = FALSE)
