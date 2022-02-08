################################################################################
################################################################################
#Data Quality report generations and preprocessing 1

#Clear environment
rm(list = ls())

#read in data
df <- read.csv("AmesHousing.csv",
               stringsAsFactors = FALSE, na.strings = c("NA", ""))


names(df)



################################################################################
#Create Data Quality report in working directory
library(dataQualityR)
num.file <- paste(getwd(), "/dqames_num.csv", sep= "")
cat.file <- paste(getwd(), "/dqames_cat.csv", sep= "")
checkDataQuality(data= df, out.file.num= num.file, out.file.cat= cat.file)

#Change numeric to categorical 
class(df$MS.SubClass)
df$MS.SubClass
df$MS.SubClass = as.factor(df$MS.SubClass)
class(df$MS.SubClass)
levels(df$MS.SubClass)

#Remove Order, not needed 
df$Order=NULL

num.file <- paste(getwd(), "/dqames_num.csv", sep= "")
cat.file <- paste(getwd(), "/dqames_cat.csv", sep= "")
checkDataQuality(data= df, out.file.num= num.file, out.file.cat= cat.file)


################################################################################
#Add skewness and kurtosis to your data quality report
#Identify numeric data
numer = c("PID","Lot.Frontage", "Lot.Area", "Overall.Qual", "Overall.Cond",
          "Year.Built", "Year.Remod.Add", "Mas.Vnr.Area", "BsmtFin.SF.1",
          "BsmtFin.SF.2", "Bsmt.Unf.SF", "Total.Bsmt.SF", "X1st.Flr.SF",
          "X2nd.Flr.SF", "Low.Qual.Fin.SF", "Gr.Liv.Area", "Bsmt.Full.Bath",
          "Bsmt.Half.Bath", "Full.Bath", "Half.Bath", "Bedroom.AbvGr",
          "Kitchen.AbvGr", "TotRms.AbvGrd", "Fireplaces", "Garage.Yr.Blt",
          "Garage.Cars", "Garage.Area", "Wood.Deck.SF", "Open.Porch.SF",
          "Enclosed.Porch", "X3Ssn.Porch", "Screen.Porch", "Pool.Area",
          "Misc.Val", "Mo.Sold", "Yr.Sold", "SalePrice")

#library for skewness() and kurtosis()
library(e1071)

#read in numeric data quality report to add skewness and kurtosis
dfnumqr=read.csv("dqames_num.csv")

# loop over features and add two new columns 
# this loop will calculate skewness and kurtosis for each
# numeric column in df and add it to the data quality report
# Noraml distribution has kurtosis or 3
# Skewed > 1 or < -1
for (i in 1:nrow(dfnumqr)){
  dfnumqr$skew[i] = skewness(df[,numer[i]], na.rm = TRUE)
  dfnumqr$kurt[i] = kurtosis(df[,numer[i]], na.rm = TRUE)
}

#write new data quality report
write.csv(dfnumqr,"dqames_num.csv", row.names = FALSE)

plot(density(df$X2nd.Flr.SF))
plot(density(df$Lot.Area))
plot(density(df$Lot.Frontage))

################################################################################
###Missing Values

#Remove NAs for feature Mas.Vnr.Area = NA
df=df[which(is.na(df$Mas.Vnr.Area)==FALSE),]

dfsave = df

#Impute the mean Lot.Frontage
plot(density(df$Lot.Frontage[!is.na(df$Lot.Frontage)]))
origmean = mean(df$Lot.Frontage, na.rm = TRUE)
origmean #before imputation
origsd = sd(df$Lot.Frontage, na.rm = TRUE)
origsd #before imputation

df$Lot.Frontage[is.na(df$Lot.Frontage)] <- mean(df$Lot.Frontage, na.rm = TRUE)
newmean=mean(df$Lot.Frontage)
newmean # imputed with mean
newsd = sd(df$Lot.Frontage, na.rm = TRUE)
newsd #imputed with mean
plot(density(df$Lot.Frontage))

#restore data set 
df = dfsave

#Impute by sampleing. Can be preffered in order to keep original distributions
library(mice)
#use mice to impute missing values by sample. This is work the entire data frame
#please refer to mice documentation
imputed = mice(df, m=1, seed = 169, method = "sample")

micedf = complete(imputed)
micemean=mean(micedf$Lot.Frontage)
micemean # imputed with sample
origmean # original mean
micesd = sd(micedf$Lot.Frontage, na.rm = TRUE)
micesd #imputed with sample
origsd #original sd

# review orignal distibutions
plot(density(df$Lot.Frontage[!is.na(df$Lot.Frontage)]))
# this distribution looks more like the original
plot(density(micedf$Lot.Frontage))

# replace original data frame Lot.Frontage
# with imputed values
df$Lot.Frontage = micedf$Lot.Frontage

################################################################################
###Outliers
#OUtlier Clamp
#find the quartiles
quar=summary(df$Lot.Frontage)
#alternate way to by using quantiles function
#t=quantile(df$X2nd.Flr.SF, prob = seq(0, 1, length = 5), type = 5)

#25% quartile
quar[2]
#75%quartile
quar[5]
#interqaurtile
quar[[5]]-quar[[2]]
#1.5 times the interquartile
iquar=1.5*(quar[[5]]-quar[[2]])
iquar
#add 1.5 times interquartile to the 75%
topClamp = quar[[5]] + iquar
topClamp
#subtract 1.5 times interquartile to the 25%
lowClamp = quar[[2]] - iquar 
lowClamp
#remove potential outliers
#you would want to review these outliers before removing.
#in general you only remove outliers for models that are sensitive to this
df=df[which(df$Lot.Frontage<topClamp),]
df=df[which(df$Lot.Frontage>lowClamp),]


plot(density(df$Lot.Frontage))


################################################################################
#create a new data frame with just numeric features for transformation processing
#we include the PID indentifier to merge later
dfnum = df[,numer]

#keep only complete cases - no NAs for correlation
dfnumcor.input = dfnum[complete.cases(dfnum),]

################################################################################
#Dealing with skewness and normalization
library(caret)
## Use caret's preProcess function to transform for skewness
# 'BoxCox' removes skewness, 'scale' noramlizes with z-score and
# 'center' will create a mean of 0
dfPP <- preProcess(dfnum, method = c("BoxCox", "scale", "center"))

## Apply the transformations
dfTrans <- predict(dfPP, dfnum)

####Skewness
#check skewness of one feature prior to transformation
skewness(df$Lot.Area,na.rm = TRUE)
#check skewness of one feature after to transformation
skewness(dfTrans$Lot.Area,na.rm = TRUE)
#note in this case log of Lot.Area produces the same result
skewness(log(df$Lot.Area),na.rm = TRUE)


histogram(~df$Lot.Area,
          xlab = "Natural Units",
          type = "count")

histogram(~dfTrans$Lot.Area,
          xlab = "Transformed Data",
          ylab = " ",
          type = "count")

###Normalization and Center
### from above 'scale' and 'center' standardized Lot.Area
#preProcess(dfnum, method = c("BoxCox", "scale", "center"))
#Check range before transformation
summary(df$Lot.Area)
#Check range after transformation
summary(dfTrans$Lot.Area)

df$Lot.Area = dfTrans$Lot.Area



################################################################################
### Removing Variables

## To filter on correlations, we first get the correlation matrix for the 
## predictor set

dfnumCorr <- cor(dfnumcor.input) 

library(corrplot)
corrplot(dfnumCorr, na.label = "NA")

## caret's findCorrelation function is used to identify columns to remove.
## The absolute values of pair-wise correlations are considered. If two variables 
## have a high correlation, the function looks at the mean absolute correlation
## of each variable and removes the variable with the largest mean absolute correlation.

highCorr <- findCorrelation(dfnumCorr, .75)
highCorr

#Check which columns before removing
#Gr.Liv.Area is correlated to SalePrice, which is the target...don't remove
names(dfnum[6])
names(dfnum[13])
names(dfnum[16])
names(dfnum[26])
names(dfnum[37])

dfnumCorr

#in case you wanted to remove the feature
#dfnum=dfnum[,-16]

################################################################################
#Categoric Data
#Identify categoric data
categ = c("MS.SubClass", "MS.Zoning" ,"Street" ,"Alley"
          ,"Lot.Shape" ,"Land.Contour" ,"Utilities" ,"Lot.Config"
          ,"Land.Slope" ,"Neighborhood" ,"Condition.1" ,"Condition.2"
          ,"Bldg.Type" ,"House.Style" ,"Roof.Style" ,"Roof.Matl"
          ,"Exterior.1st" ,"Exterior.2nd" ,"Mas.Vnr.Type" ,"Exter.Qual"
          ,"Exter.Cond" ,"Foundation" ,"Bsmt.Qual" ,"Bsmt.Cond"
          ,"Bsmt.Exposure" ,"BsmtFin.Type.1" ,"BsmtFin.Type.2" ,"Heating"
          ,"Heating.QC" ,"Central.Air" ,"Electrical" ,"Kitchen.Qual"
          ,"Functional" ,"Fireplace.Qu" ,"Garage.Type" ,"Garage.Finish"
          ,"Garage.Qual" ,"Garage.Cond" ,"Paved.Drive" ,"Pool.QC"
          ,"Fence" ,"Misc.Feature" ,"Sale.Type" ,"Sale.Condition")

#create a new data frame with just categorical features for processing
#we also include the PID identifier to merge later
dfcateg = df[,c("PID",categ)]

################################################################################
#create dummy variables
#change House.Style from character to factor
dfcateg$House.Style=as.factor(dfcateg$House.Style)
#create dummy variables
library(fastDummies)
# Use the function dummy_cols on dfcateg dataframe "House.Style" column
# This will create a new column for each category of House.Style and populate
# with a zero or one. This can be helpful to use categorical data in models that
# expect numeric data.
levels(dfcateg$House.Style)
table(dfcateg$House.Style)
dfcateg = dummy_cols(dfcateg, "House.Style")


View(dfcateg[,46:53])

table(dfcateg$House.Style)


################################################################################
#merge the numeric and categorical back
dffinal = merge(dfnum, dfcateg, by = intersect("PID","PID"))

#write out your file
write.csv(dffinal,"ProcessAmesHousing.csv", row.names = FALSE)
