################################################################################
################################################################################
#Clear environment
rm(list = ls())

#Set margins for plots
  par(mar=c(1,1,1,1))

#load packages - may have to add your own package for your model's caret method
library(data.table)
library(caret)
library(pROC)
library(ranger)
#library(installr)
library(fastAdaboost)
library(xgboost)
library(ggplot2)

#Full data set
dffull <- read.csv("AmesHousingFull.csv",
                   stringsAsFactors = FALSE, na.strings = c("NA", ""))

#remove numeric target
dffull$SalePrice=NULL
#remove zerovariance
dffull$Utilities_AllPub = NULL
dffull$Condition.2_RRNn = NULL
dffull$Bsmt.Cond_Po = NULL
dffull$Heating_Grav = NULL
dffull$Pool.QC_Fa = NULL
dffull$Sale.Type_Oth = NULL


######Split into 5 Training and Testing Sets######

#set seed
set.seed(1)

#create list specifiying rows for each fold
fold_list <- split(dffull, sample(1:5, nrow(dffull), replace=T))

#turn list into five separate data frames
fold1 <- as.data.frame(fold_list[[1]])
fold2 <- as.data.frame(fold_list[[2]])
fold3 <- as.data.frame(fold_list[[3]])
fold4 <- as.data.frame(fold_list[[4]])
fold5 <- as.data.frame(fold_list[[5]])

#turn combinations of folds into five sets of training and test sets
cv1train <- rbind(fold1, fold2, fold3, fold4)
cv1test <- fold5

cv2train <- rbind(fold1, fold2, fold3, fold5)
cv2test <- fold4

cv3train <- rbind(fold1, fold2, fold4, fold5)
cv3test <- fold3

cv4train <- rbind(fold1, fold3, fold4, fold5)
cv4test <- fold2

cv5train <- rbind(fold2, fold3, fold4, fold5)
cv5test <- fold1

rm(list = c("fold1","fold2", "fold3", "fold4", "fold5", "fold_list"))

#ALTERATIVEYLY we could just use createfolds()
#this creates a more consistent test/train split
fold_list = createFolds(dffull$top10per,k = 5, returnTrain = TRUE)
cv1train <- dffull[fold_list$Fold1,]
cv1test <- dffull[-fold_list$Fold1,]

cv2train <- dffull[fold_list$Fold2,]
cv2test <- dffull[-fold_list$Fold2,]

cv3train <- dffull[fold_list$Fold3,]
cv3test <- dffull[-fold_list$Fold3,]

cv4train <- dffull[fold_list$Fold4,]
cv4test <- dffull[-fold_list$Fold4,]

cv5train <- dffull[fold_list$Fold5,]
cv5test <- dffull[-fold_list$Fold5,]


#put training and testing datasets into lists for loops
cvtrain_list <- list(cv1train, cv2train, cv3train, cv4train, cv5train)

cvtest_list <- list(cv1test, cv2test, cv3test, cv4test, cv5test)

#remove objects not needed later
rm(list = c("cv1test", "cv2test", "cv3test", "cv4test", "cv5test"))
rm(list = c("cv1train", "cv2train", "cv3train", "cv4train", "cv5train"))

######Pre-Processing######
#Here we are just changing 0 and 1 to N and Y,
#splitting the Target out into a vector
#and deleting the numeric target from the df
for (i in 1:5) {
  #print interation number to track loop's progress
  print(i)
  #change relevant data type and output to ensure it works with a categorical model
  cvtrain_list[[i]]$top10per <- ifelse(cvtrain_list[[i]]$top10per == 1, "Y", "N")
  cvtrain_list[[i]]$top10per <- as.factor(cvtrain_list[[i]]$top10per)
  cvtest_list[[i]]$top10per <- ifelse(cvtest_list[[i]]$top10per == 1, "Y", "N")
  cvtest_list[[i]]$top10per <- as.factor(cvtest_list[[i]]$top10per)
  #turn target variable (top10per) column into a variable to input for Y when training the models
  assign(paste("trainY", i, sep = ""), cvtrain_list[[i]]$top10per)
  assign(paste("testY", i, sep = ""), cvtest_list[[i]]$top10per)
  #delete top10per column from data frame so model does not attempt to predict using the target variable itself
  cvtrain_list[[i]]$top10per <- NULL
  cvtest_list[[i]]$top10per <- NULL
}

#turn Y variables into lists for loops
trainY_list <- list(trainY1, trainY2, trainY3, trainY4, trainY5)
testY_list <- list(testY1, testY2, testY3, testY4, testY5)

#remove objects not needed later
rm(list = c("trainY1", "trainY2", "trainY3", "trainY4", "trainY5"))
rm(list = c("testY1", "testY2", "testY3", "testY4", "testY5"))

#save original dataframes
cvtrain_list_t = cvtrain_list
cvtest_list_t = cvtest_list

Sys.time() #FS start
######Feature Selection######
for (i in 1:5) {
  #print interation number to track loop's progress
  print(i)
  #Calculate correlation matrix for training set
  correlation <- cor(cvtrain_list[[i]])
  #Find highly correlated attributes (>0.75)
  highlyCorrelated <- findCorrelation(correlation, cutoff = .75)
  #Remove highly correlated attributes
  if(length(highlyCorrelated) != 0){(cvtrain_list[[i]] = cvtrain_list[[i]][-highlyCorrelated])}
  #create feature selection control
  FScontrol <- rfeControl(functions = lrFuncs, method = "cv", number = 2)
  #Feature selection on training set only using recursive factor elimination
  results <- rfe(cvtrain_list[[i]], as.factor(trainY_list[[i]]), rfeControl = FScontrol)
  #reduces training and testing dataset down to only those columns identified by feature selection
  print(predictors(results))
  cvtrain_list[[i]] <- cvtrain_list[[i]][,predictors(results)]
  cvtest_list[[i]] <- cvtest_list[[i]][,predictors(results)]
  #plot results of feature selection
  plot <- plot(results, type=c("g", "o"))
  #save plot
  assign(paste("FSplot", i, sep = ""), plot)
}
Sys.time() #FS end

Sys.time() #RF start
######Random Forest Model######
for (i in 1:5) {
  #print interation number to track loop's progress
  print(i)
  #specify 5-fold cross validation 
  control <- trainControl(method = 'cv', number = 5, classProbs = TRUE, summaryFunction = twoClassSummary) 
  #create grid of parameters which train function will use later to identify the ideal parameters for best performance
  grid <- expand.grid(mtry = seq(as.integer(sqrt(ncol(cvtrain_list[[i]]))),ncol(cvtrain_list[[i]]),2), 
                      splitrule = "gini", 
                      min.node.size = c(1,5,10,15))
  #create random forest model and train using cross validation and a grid of parameters
  model <- train(x = cvtrain_list[[i]], y = trainY_list[[i]],
                 method = "ranger", num.trees = 200, importance = "impurity",
                 trControl = control, tuneGrid = grid, metric = "ROC")
  #predict probability using trained model
  predictY <- predict(model, cvtest_list[[i]],type = "prob")
  #associate probabilites with class for calculating ROC
  test = as.data.frame(cbind(testY_list[[i]],predictY$Y))
  #sort on probability from high to low
  test=test[order(-test$V2),]
  #calculate auc and plot roc curve
  #roc(class,probability)
  roc <- roc(test$V1,test$V2)
  auc <- auc(roc)
  #save this iteration model
  assign(paste("model_rf", i, sep = ""), model)
  #save this iteration auc
  assign(paste("AUC_rf", i, sep = ""), auc)
}
Sys.time() #RF end

#put auc's into a vector for average and sd
AUC_rf <- c(AUC_rf1, AUC_rf2, AUC_rf3, AUC_rf4, AUC_rf5)

#average the auc results for each fold 
avgAUC_rf <- mean(AUC_rf)

#standard deviation of the auc results for each fold
sdAUC_rf <- sd(AUC_rf)

Sys.time() #ada start
######Adaboost Model######
for (i in 1:5) {
  #print interation number to track loop's progress
  print(i)
  #reassign testY_list to origional testY variables so they will be a factor data type for modeling
  #testY_list <- list(testY1, testY2, testY3, testY4, testY5)
  #specify 5-fold cross validation
  control <- trainControl(method = 'cv', number = 5, classProbs = TRUE, summaryFunction = twoClassSummary) 
  #create grid of parameters which train function will use later to identify the ideal parameters for best performance
  grid <- expand.grid(nIter = c(5,10,25,50,100), method = "adaboost")
  #create model and train using cross validation and a grid of parameters
  model <- train(x = cvtrain_list[[i]], y = trainY_list[[i]],
                 method = "adaboost", trControl = control, 
                 tuneGrid = grid, metric = "ROC")
  #predict probability using trained model
  predictY <- predict(model, cvtest_list[[i]],type = "prob")
  #associate probabilites with class for calculating ROC
  test = as.data.frame(cbind(testY_list[[i]],predictY$Y))
  #sort on probability from high to low
  test=test[order(-test$V2),]
  #calculate auc and plot roc curve
  #roc(class,probability)
  roc <- roc(test$V1,test$V2)
  auc <- auc(roc)
  #save this iteration model
  assign(paste("model_ada", i, sep = ""), model)
  #save this iteration auc
  assign(paste("AUC_ada", i, sep = ""), auc)
}
Sys.time() #ada end

#put auc's into a vector for average and sd
AUC_ada <- c(AUC_ada1, AUC_ada2, AUC_ada3, AUC_ada4, AUC_ada5)

#average the auc results for each fold 
avgAUC_ada <- mean(AUC_ada)

#standard deviation of the auc results for each fold
sdAUC_ada <- sd(AUC_ada)


Sys.time() #gb start
######Gradient Boost Model######
for (i in 1:5) {
  #print interation number to track loop's progress
  print(i)
  #specify 5-fold cross validation 
  control <- trainControl(method = 'cv', number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)
  #create grid of parameters which train function will use later to identify the ideal parameters for best performance
  grid <- expand.grid(nrounds = 2,
                      max_depth = c(1,5,10,15),
                      eta = c(.01,.001,.0001),
                      gamma = c(1,2,3),
                      colsample_bytree = c(.4,.7,1),
                      min_child_weight = c(.5,1,1.5),
                      subsample = 1)
  #create model and train using cross validation and a grid of parameters
  model <- train(x = cvtrain_list[[i]], y = trainY_list[[i]], method = "xgbTree",
                 trControl = control, tuneGrid = grid, metric = "ROC")
  #predict probability using trained model
  predictY <- predict(model, cvtest_list[[i]],type = "prob")
  #associate probabilites with class for calculating ROC
  test = as.data.frame(cbind(testY_list[[i]],predictY$Y))
  #sort on probability from high to low
  test=test[order(-test$V2),]
  #calculate auc and plot roc curve
  #roc(class,probability)
  roc <- roc(test$V1,test$V2)
  auc <- auc(roc)
  #save this iteration model
  assign(paste("model_gb", i, sep = ""), model)
  #save this iteration auc
  assign(paste("AUC_gb", i, sep = ""), auc)
}
Sys.time() #gb end

#put auc's into a vector for average and sd
AUC_gb <- c(AUC_gb1, AUC_gb2, AUC_gb3, AUC_gb4, AUC_gb5)

#average the auc results for each fold
avgAUC_gb <- mean(AUC_gb)

#standard deviation of the auc results for each fold
sdAUC_gb <- sd(AUC_gb)

Sys.time() #glm start
######Logistic Regression Modeling######
for (i in 1:5) {
  #print interation number to track loop's progress
  print(i)
  #specify 5-fold cross validation 
  control <- trainControl(method = 'cv', number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)
  #create model and train using cross validation and a grid of parameters
  model <- train(x = cvtrain_list[[i]], y = trainY_list[[i]],
                 method = "glm", trControl = control, family="binomial", metric = "ROC")
  #predict probability using trained model
  predictY <- predict(model, cvtest_list[[i]],type = "prob")
  #associate probabilites with class for calculating ROC
  test = as.data.frame(cbind(testY_list[[i]],predictY$Y))
  #sort on probability from high to low
  test=test[order(-test$V2),]
  #calculate auc and plot roc curve
  #roc(class,probability)
  roc <- roc(test$V1,test$V2)
  auc <- auc(roc)
  #save this iteration model
  assign(paste("model_glm", i, sep = ""), model)
  #save this iteration auc
  assign(paste("AUC_glm", i, sep = ""), auc)
}
Sys.time() #glm end
#put auc's into a vector for average and sd
AUC_glm <- c(AUC_glm1, AUC_glm2, AUC_glm3, AUC_glm4, AUC_glm5)

#average the auc results for each fold
avgAUC_glm <- mean(AUC_glm)

#standard deviation of the auc results for each fold
sdAUC_glm <- sd(AUC_glm)



######create visual comparing model AUCs######
AllAUC <- c(AUC_rf, AUC_ada, AUC_gb, AUC_glm)
allmodels <- c("Random Forest", "Random Forest", "Random Forest", "Random Forest", "Random Forest", "Adaboost", "Adaboost", "Adaboost", "Adaboost", "Adaboost", 
               "Gradient Boost", "Gradient Boost", "Gradient Boost", "Gradient Boost", "Gradient Boost", "Logistic Regression", "Logistic Regression", "Logistic Regression", "Logistic Regression", "Logistic Regression")
df_results <- data.frame("Type" = allmodels, "AUC" = AllAUC)

AUC_plot <- qplot(Type, AUC, data = df_results, 
                  geom = "boxplot", fill = I("cornflowerblue"), color = I("Blue4"))
AUC_plot <- AUC_plot + ggtitle("Model Results")

ggsave(filename = "ModelResults.pdf", plot = AUC_plot, 
       width = 6, height = 4, units = "in")


