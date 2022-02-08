################################################################################
################################################################################
#Clear environment
rm(list = ls())

#read in data
#Numeric Target and Numeric Predictors
dfnoutnp <- read.csv("AmesHousingNoutNpredv1.csv",
               stringsAsFactors = FALSE, na.strings = c("NA", ""))

#Numeric Target and Categorical Predictors
dfnoutcp <- read.csv("AmesHousingNoutCpredv1.csv",
                     stringsAsFactors = FALSE, na.strings = c("NA", ""))

#Categorical Target and Numeric Predictors
dfcoutnp <- read.csv("AmesHousingCoutNpredv1.csv",
                     stringsAsFactors = FALSE, na.strings = c("NA", ""))

#Categorical Target and Categorical Predictors
dfcoutcp <- read.csv("AmesHousingCoutCpredv1.csv",
                     stringsAsFactors = FALSE, na.strings = c("NA", ""))

#Full data set
dffull <- read.csv("AmesHousingFullv1.csv",
                     stringsAsFactors = FALSE, na.strings = c("NA", ""))

########################
#find relationships between predictors and themselves
################################################################################
### Removing Correlated Variables

## To filter on correlations, we first get the correlation matrix for the 
## predictor set

#save target positions
ntargetpos=1
ctargetpos=2

#remove targets for predictor only analysis
dffullp = dffull[,c(-ntargetpos,-ctargetpos)]

## To filter on correlations, we first get the correlation matrix for the 
## predictor set
dfnumCorr <- cor(dffullp) 

## caret's findCorrelation function is used to identify columns to remove.
## The absolute values of pair-wise correlations are considered. If two variables 
## have a high correlation, the function looks at the mean absolute correlation
## of each variable and removes the variable with the largest mean absolute correlation.
library(caret)
highCorr <- findCorrelation(dfnumCorr, .75)
highCorr

#Check columns before removing
#####names(dffull[highCorr]) this was incorrect dataframe form Script 4v5
names(dffullp[highCorr])

#####Added addition functions to write the correlation out in a file for visual analysis
#change to dataframe
outCorMatrx = as.data.frame(dfnumCorr)

#write correlation matrix
write.csv(outCorMatrx, "AmesHousingCorMatrx.csv", row.names = TRUE)



#########################
#Find linear relationships for numeric target and numeric predictors
#store the number of columns in a variable
numcol = ncol(dfnoutnp)

#set target position in a variable
targetpos = 1

#Pearson correlations
# If you just run cor(dfnoutnp), you would get a correlation matrix with ALL the variables included. 
# So what is this part doing [-(targetpos),(targetpos)]? 
# It is really just [-(1),(1)]
#[-(Says which column to drop from the correlation), (Tells us which column to run correlation against all the other columns)]

correlations <- cor(dfnoutnp)[-(targetpos),(targetpos)]

#Spearman correlations
#note the method of "spearman"
scorrelations <- cor(dfnoutnp, method = "spearman")[-(targetpos),(targetpos)]

#set the first predictor position as a variable
firstpred = targetpos + 1

#Create data frame
#we are adding the names of the predictors, and the two correlations
corrs <- data.frame(Predictor = names(dfnoutnp)[firstpred:numcol],
                    pearson = correlations,
                    spearman  = scorrelations)

#sort on scorrelation
corrs = corrs[order(-corrs$spearman),]


write.csv(corrs, "AmesHousingNtar_v_Npred.csv", row.names = FALSE)


###########################
#Find relationships for numeric target and categoric predictors
# t.test test of two means - An example running one predictor at a time
#Let's look at MS.SubClass_20
t.test(dfnoutcp$SalePrice ~ dfnoutcp$MS.SubClass_20)
#Now let's look at MS.SubClass_50
t.test(dfnoutcp$SalePrice ~ dfnoutcp$MS.SubClass_50)

# make a dataframe with the results for MS.SubClass_120
tStats=as.data.frame(t(unlist(t.test(dfnoutcp$SalePrice ~ dfnoutcp$MS.SubClass_120)[c("statistic", "p.value", "estimate")])))
# add names to your dataframe
names(tStats) <- c("t.Statistic", "t.test_p.value", "mean0", "mean1")
# add a difference column
tStats$difference <- tStats$mean1 - tStats$mean0
tStats
# end one predictor at a time example

## Now let's calculate t.test for ALL the categortic predictors
#move Y value from the data frame to prepare for running t.test for all columns
SalePriceY = dfnoutcp$SalePrice
#Remove columns
dfnoutcp$SalePrice=NULL
#dfnoutcp$PID=NULL

##this will run t.test for every column against SalePrice
ttests <- apply(dfnoutcp, 2,
               function(x, y)
               {
                 tStats <- t.test(y ~ x)[c("statistic", "p.value", "estimate")]
                 unlist(tStats)
               },
               y = SalePriceY)

## The results are a matrix with predictors in columns. We reverse this
# with transpose t() and create a dataframe of our results
ttests <- as.data.frame(t(ttests))

#Rename columns
names(ttests) <- c("t.Statistic", "t.test_p.value", "mean0", "mean1")

#Create diff column
ttests$difference <- ttests$mean1 - ttests$mean0
ttests

#keep only t.test_p.values that are significant (<= .05) and sort by difference
ttests = ttests[which(ttests$t.test_p.value<=.05),]
ttests = ttests[order(-ttests$difference),]

#Write out for use.
write.csv(ttests, "AmesHousingNtar_v_Cpred.csv")


#########################
#Find relationships for categoric target and numeric predictors
dfcoutnp$PID=NULL

## Compute the areas under the ROC curve for each feature
library(caret)
aucVals <- filterVarImp(x = dfcoutnp[, -1], y = as.factor(dfcoutnp$top10per))
 
#add predictor names as a column
aucVals$Predictor <- rownames(aucVals)
#sort on importance
aucVals = aucVals[order(-aucVals$X1),]
#reorder columns
aucVals = aucVals[,c(3,2)]
#rename column
colnames(aucVals)[2] <- "AUC"

write.csv(aucVals, "AmesHousingCtar_v_Npred-AUC.csv", row.names = FALSE)


#########################
#Find relationships for categoric target and categoric predictors
dfcoutcp$PID=NULL
dfcoutcp$top10per = as.factor(dfcoutcp$top10per)

## Sample use of table function 2x2 row is needed for a fisher test.
## You will see top10Per in the c
table(dfcoutcp$top10per,dfcoutcp$Exter.Qual_Ex, dnn = c("top10per", "Exter.Qual_Ex"))

## This is a simple function to compute several statistics for binary predictors
library(CORElearn)
tableCalcs <- function(x, y)
{
  tab <- table(x, y)
  fet <- fisher.test(tab)
  out <- c(odds.ratio = fet$estimate,
           fisher_p.value = fet$p.value,
           info.gain = attrEval(y ~ x, estimator = "GainRatio"))
}

## lapply() is used to execute the function on each column
tableResults <- lapply(dfcoutcp[, -1], tableCalcs, y = dfcoutcp$top10per)

## The results come back as a list of vectors, and "rbind" is used to join
## them together as rows of a table
tableResults <- do.call("rbind", tableResults)
# coerce to a data frame
tableResults = as.data.frame(tableResults)

#keep only fisher_p.values that are significant (<= .05) and sort by info.gain
tableResults = tableResults[which(tableResults$fisher_p.value<=.05),]
tableResults = tableResults[order(-tableResults$info.gain.x),]
#add predictor names to the dataframe
tableResults$Predictor <- rownames(tableResults)
#reorder columns
tableResults = tableResults[,c(4,1,2,3)]

#Let's look at our classification target and Exter.Qual.Ex. You
#should sort by Exter.Qual.Ex so the 1's flood the top of the View
View(dfcoutcp[,c(1,162)])

write.csv(tableResults, "AmesHousingCtar_v_Cpred-Gain.csv", row.names = FALSE)
