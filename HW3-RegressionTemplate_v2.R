################################################################################
################################################################################
#Clear environment

rm(list = ls())

#Set margins for plots
par(mar=c(2.5,2.5,2.5,2.5))


#train data set
traindatsav <- read.csv("HW3-dsm_housing_Train.csv",
                     stringsAsFactors = FALSE, na.strings = c("NA", ""))
#test data set
testdatsav <- read.csv("HW3-dsm_housing_Test.csv",
                   stringsAsFactors = FALSE, na.strings = c("NA", ""))
#rename
colnames(traindatsav)[1] <- "SalePrice"
colnames(testdatsav)[1] <- "SalePrice"

#load from saved data frames
traindat = traindatsav
testdat = testdatsav


#drop the classification target
traindat$top10per=NULL
#drop the classification target
testdat$top10per=NULL


########################
# Create a linear regression model
#Fit a linear regression model (to establish a familiar baseline)
model <- lm(SalePrice ~ . , traindat)

summary(model)

#Compute mean absolute error of model on train set
trainMAE <- mean(abs(model$residuals)) 
trainMAE

#Calculate predictions for each observation in test set
predictY <- predict(model, testdat)

#Compute model residuals on the test set
testerrors <- testdat$SalePrice - predictY

#Compute mean absolute error of model on test set
testMAE <- mean(abs(testerrors)) 
testMAE


################################################################################
### Removing Correlated Variables and Near Zero Variance
#---------------------------
#check for near zero variance
library(caret)

nearZeroVar(traindat)
names(traindat[, nearZeroVar(traindat)])
remove = names(traindat[, nearZeroVar(traindat)])
remove

#Remove near zero variance (constants) features and create new dataframe.
#note the negative sign before the 'which' function.
#-which(names(dffull) %in% remove) will return the columns
#numbers that are NOT in the 'remove'
traindatp = traindat[,-which(names(traindat) %in% remove)]
testdatp = testdat[,-which(names(testdat) %in% remove)]


## To filter on correlations, we first get the correlation matrix for the 
## predictor set
dfnumCorr <- cor(traindatp[,-1]) 

## caret's findCorrelation function is used to identify columns to remove.
## The absolute values of pair-wise correlations are considered. If two variables 
## have a high correlation, the function looks at the mean absolute correlation
## of each variable and removes the variable with the largest mean absolute correlation.
highCorr <- findCorrelation(dfnumCorr, .75)
highCorr

#Check columns before removing
names(traindatp[highCorr])
remove = names(traindatp[highCorr])

#Remove highly correlated features and create new dataframe.
#note the negative sign before the which function.
#-which(names(dffull) %in% remove) will return the columns
#numbers that are NOT in the 'remove'
traindatp = traindatp[,-which(names(traindatp) %in% remove)]
testdatp = testdatp[,-which(names(testdatp) %in% remove)]
#------------------------------------------

#Creates training set from observations corresponding to indices sampled
#Note that it corresponds to roughly 1 fold of a 10-fold cross-validation scheme for this data
traindat = traindatp

#The rest of the observations are set aside for test
testdat = testdatp

#Let's see the impact of removing correlated and near zero variance 
#columns on our model. Fit a linear regression model (to establish 
#a familiar baseline)
model <- lm(SalePrice ~ . , traindat)

#Compute mean absolute error of model on train set
trainMAE <- mean(abs(model$residuals)) #
trainMAE

#Calculate predictions for each observation in test set
predictY <- predict(model, testdat)

#Compute error on the test set
testerrors <- testdat$SalePrice - predictY

#Compute mean absolute error of model on test set
testMAE <- mean(abs(testerrors)) # 
testMAE



################################################################################
#lets use backward selection drive our feature selection

#Backward Selection
#1. Tune/train the model on training set using all predictors
#2. Calculate model performance and save
#3. Calculate variable importance
#4.Loop removing feature of least performance
#   -Tune/train model
#   -Calculate model performance (we are using p-value of variables)
#   -Calculate variable importance, save current set
#   -If model performance improves, repeat, else end
#5. Use saved current set


#Lets reset train / testa
#load from saved data frames
traindat = traindatsav
testdat = testdatsav

model <- lm(SalePrice ~ . , traindat)


for (i in 1:ncol(traindat)){
  # Create a linear regression model
  varimportance = data.frame(pvalue=coef(summary(model))[,4])
  varimportance$Predictor <- rownames(varimportance)
  #drop intercept
  varimportance = varimportance[2:nrow(varimportance),]
  #sort on pvalue
  varimportance = varimportance[order(-varimportance$pvalue),]
  print(nrow(varimportance))
  print(varimportance[1,1])
  if (varimportance[1,1]<.05){
    break
  }
  #drop highest p-value
  varimportance = varimportance[2:nrow(varimportance),]
  #store x variable names for use in lm()
  newxvalues = varimportance$Predictor
  #Fit a linear regression model
  model <- lm(SalePrice ~ . , traindat[,c("SalePrice",newxvalues)])
  

  #Compute mean absolute error of model on train set
  newtrainMAE <- mean(abs(model$residuals))
}

#lets look at the selected features
newxvalues

newtrainMAE  

#Calculate predictions for each observation in test set
predictY <- predict(model, testdat[,newxvalues])

#Compute model residuals on the test set
testerrors <- testdat$SalePrice - predictY

#Compute mean absolute error of model on test set
testMAE <- mean(abs(testerrors))
testMAE


#######Let's try CrossValidation
################################################################################
### Linear Regression

### Create a control function that will be used across models. We
### create the fold assignments explicitly instead of relying on the
### random number seed being set to identical values.

#Set up several data sets and Y variable vectors
#full
#fullPriceY = dffull$SalePrice
#dffull$SalePrice = NULL

#train
trainPriceY = traindat$SalePrice
traindat$SalePrice = NULL

#test
testPriceY = testdat$SalePrice
testdat$SalePrice = NULL


library(caret)
set.seed(1)
#create 10 indexes of our target SalePrice 
indx <- createFolds(trainPriceY, returnTrain = TRUE)
#creats a 10 sets of control variables to be used by our models
ctrl <- trainControl(method = "cv", index = indx)

### Linear regression model with all of the predictors. This will
### produce some warnings that a 'rank-deficient fit may be
### misleading'. This is related to the predictors being so highly
### correlated that some of the math has broken down.
set.seed(1)
lmTune0 <- train(x = traindat[,newxvalues], y = trainPriceY,
                 method = "lm",
                 trControl = ctrl)

lmTune0               

#Compute mean absolute error of model on train set
lmTune0$results$MAE 

#Calculate predictions for each observation in test set
predictY <- predict(lmTune0, testdat[,newxvalues])

#Compute model residuals on the test set
testerrors <- testPriceY - predictY

#Compute mean absolute error of model on test set
testMAE <- mean(abs(testerrors))
testMAE
########################################################################################

  
  

################################################################################
###Rule Based Regression
###Basic Regression Trees

library(rpart)
library(caret)

set.seed(1)
#create 10 indexes of our target SalePrice 
indx <- createFolds(trainPriceY, k = 10, returnTrain = TRUE)
#creats a 10 sets of control variables to be used by our models
ctrl <- trainControl(method = "cv", index = indx)

#Build a tree and tune on complexity with method "rpart"
#change tuneLength parameter to specify how many times to run
#recall the parameter ctrl was set above
#we will use the full trainset with all predictors
cartTune <- train(x = traindat, y = trainPriceY,
                  method = "rpart",
                  tuneLength = 100,
                  trControl = ctrl)


cartTune
#plot the model
plot(cartTune)

#Train performance
predictY <- predict(cartTune, traindat)
trainerrors <-  predictY - trainPriceY
trainMAE <- mean(abs(trainerrors)) 
# we will see below
trainMAE



#Calculate predictions for each observation in test set
predictY <- predict(cartTune, testdat)

#Compute model residuals on the test set
testerrors <-  predictY - testPriceY

#Compute mean absolute error of model on test set
testMAE <- mean(abs(testerrors))
testMAE

#Build a tree and tune on max depth with method "rpart2"
#rpart2 will tune on depth
#change tuneLength parameter to specify how many times to run
#recall the parameter ctrl was set above
#we will use the full trainset with all predictors
cartTune <- train(x = traindat, y = trainPriceY,
                  method = "rpart2",
                  tuneLength = 100,
                  trControl = ctrl)
cartTune
#plot the model
plot(cartTune)

predictY <- predict(cartTune, traindat)
trainerrors <-  predictY - trainPriceY
trainMAE <- mean(abs(trainerrors))
# we will see below
trainMAE


#Calculate predictions for each observation in test set
predictY <- predict(cartTune, testdat)

#Compute model residuals on the test set
testerrors <-  predictY - testPriceY

#Compute mean absolute error of model on test set
testMAE <- mean(abs(testerrors)) 
testMAE

################################################################################
### Non Linear Regression
### K-Nearest Neighbors - 
set.seed(1)
#create 10 indexes of our target SalePrice 
indx <- createFolds(trainPriceY, k = 10, returnTrain = TRUE)
#creats a 10 sets of control variables to be used by our models
ctrl <- trainControl(method = "cv", index = indx)


#keep only significant values
knndat = traindat[, newxvalues]
knntestdat = testdat[, newxvalues]
#remove near-zero variance predictors
removenzv = names(knndat[nearZeroVar(knndat)])
knndat = knndat[,-which(names(knndat) %in% removenzv)]
knntestdat = knntestdat[,-which(names(knntestdat) %in% removenzv)]

set.seed(1)
knnTune <- train(x = knndat, y = trainPriceY,
                 method = "knn",
                 preProc = c("center", "scale"),
                 tuneGrid = data.frame(k = 1:20),
                 trControl = ctrl)

knnTune

plot(knnTune)

#model performance
predictY <- predict(knnTune, knndat)
trainerrors <-  predictY - trainPriceY
trainMAE <- mean(abs(trainerrors)) 
trainMAE

#Calculate predictions for each observation in test set
predictY <- predict(knnTune, knntestdat)

#Compute model residuals on the test set
testerrors <- testPriceY - predictY

#Compute mean absolute error of model on test set
testMAE <- mean(abs(testerrors))
testMAE

################################################################################
###Ensemble methods
###Random Forests for Regression


set.seed(1)
#create 10 indexes of our target SalePrice 
#indx <- createFolds(trainPriceY, k = 10, returnTrain = TRUE)
#creats a 10 sets of control variables to be used by our models
#ctrl <- trainControl(method = "cv", index = indx)
#mtryGrid <- data.frame(mtry = floor(seq(10, ncol(traindat), length = 10)))

library(randomForest)
rfModel = randomForest(traindat[,newxvalues] , trainPriceY,
                       importance = TRUE,
                       ntree = 500)

plot(rfModel)


#add feature importance to a data frame and sort
rfmodelimport=as.data.frame(rfModel$importance)
rfmodelimport = rfmodelimport[order(-rfmodelimport$`%IncMSE`),]
View(rfmodelimport)

#model performance
predictY <- predict(rfModel, traindat[,newxvalues])
#predictY <- predict(rfModel, traindat)
trainerrors <-  predictY - trainPriceY
trainMAE <- mean(abs(trainerrors)) 
# we will see below
trainMAE

#Calculate predictions for each observation in test set
predictY <- predict(rfModel, testdat[,newxvalues])

#Compute model residuals on the test set
testerrors <- testPriceY - predictY

#Compute mean absolute error of model on test set
testMAE <- mean(abs(testerrors)) 
testMAE

####Now let's try random forrest with the full data set (no feature selection)
# and see evalution criteria performance
rfModel = randomForest(traindat , trainPriceY,
                       importance = TRUE,
                       ntree = 500)

plot(rfModel)


#add feature importance to a data frame and sort
rfmodelimport=as.data.frame(rfModel$importance)
rfmodelimport = rfmodelimport[order(-rfmodelimport$`%IncMSE`),]
View(rfmodelimport)


#model performance
predictY <- predict(rfModel, traindat)
#predictY <- predict(rfModel, traindat)
trainerrors <-  predictY - trainPriceY
trainMAE <- mean(abs(trainerrors)) 
# we will see below
trainMAE

#Calculate predictions for each observation in test set
predictY <- predict(rfModel, testdat)

#Compute model residuals on the test set
testerrors <- testPriceY - predictY

#Compute mean absolute error of model on test set
testMAE <- mean(abs(testerrors)) 
testMAE


