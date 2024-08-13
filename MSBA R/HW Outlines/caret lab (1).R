################################################################################
# caret lab
################################################################################
options(java.parameters = "-Xmx64048m") # 64048 is 64 GB
library(RMariaDB)
conn <- dbConnect(RMariaDB::MariaDB(), host="datamine.rcac.purdue.edu", port=3306
                  , dbname="khou", user="gen_user", password="gen_user")

# pull in the training data into a data.frame called tr
tr <- dbGetQuery(conn, "select * from train")
# disconnect from db
dbDisconnect(conn)

################################################################################
## Data Cleaning
################################################################################
# You cannot create variable names in R that begin with a number or underscore.
# However, since we pulled this data in from a database, it allowed a couple
# columns to slip by. This can cause us issues later on, so lets fix the following
# column names (1stFlrSF, 2ndFlrSF, 3SsnPorch) to (FstFlrSF, SecFlrSf, ThiSsnPorch)
names(tr)
names(tr)[44] <- "FstFlrSF"
names(tr)[45] <- "SecFlrSf"
names(tr)[70] <- "ThiSsnPorch"
names(tr)

# One thing you might notice is that some of the columns show NA implying that
# there are missing values. However, this is misleading as in the database they
# were stored as literal NA values instead of missing values. Some of the values
# indeed should be missing, and I show below how I corrected these for each
# field.

# Features where 'NA' is really a missing value, make them blank
# LotFrontage, MasVnrType, MasVnrArea, Electrical, GarageYrBlt
realBlanks <- c("LotFrontage", "MasVnrType", "MasVnrArea", "Electrical"
                , "GarageYrBlt")
# loop through each field and make 'NA' values missing values
for (i in 1:length(realBlanks)){
    tr[which(tr[,realBlanks[i]]=='NA'),realBlanks[i]] <- NA 
}

# Features where 'NA' is a meaningful factor level, we will keep them. These are:
# Alley, BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinType2, FireplaceQu,
# GarageType, GarageFinish, GarageQual, GarageCond, PoolQC, Fence, MiscFeature

# You read about the dataset by reviewing the data dictionary for this data located
# here: https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data
# Based on your understanding of data, you coerced every variable in the 'tr' dataset
# to the correct R data type it should be for proper data analysis. Last assignment
# this required using several repetitive 'as.' functions to corerce each column
# to the approriate type. However you might do this using a for loop.
str(tr)
# corece these fields to numeric vectors
tr$LotFrontage <- as.numeric(tr$LotFrontage)
tr$GarageYrBlt <- as.numeric(tr$GarageYrBlt)
tr$MasVnrArea <- as.numeric(tr$MasVnrArea)

# Write a for loop that coerces the following fields to factor types:
#"MSSubClass","MSZoning","Street","Alley","LotShape","LandContour","Utilities",
#"LotConfig","LandSlope","Neighborhood","Condition1","Condition2","BldgType",
#"HouseStyle","RoofStyle","RoofMatl","Exterior1st","Exterior2nd","MasVnrType",
#"ExterQual","ExterCond","Foundation", "BsmtQual","BsmtCond",
#"BsmtExposure", "BsmtFinType1","BsmtFinType2","Heating","HeatingQC","CentralAir",
#"Electrical","KitchenQual","Functional","FireplaceQu","GarageType","GarageFinish",
#"GarageQual","GarageCond","PavedDrive","PoolQC","Fence","MiscFeature","SaleType",
#"SaleCondition"

s <- c("MSSubClass","MSZoning","Street","Alley","LotShape","LandContour","Utilities",
       "LotConfig","LandSlope","Neighborhood","Condition1","Condition2","BldgType",
       "HouseStyle","RoofStyle","RoofMatl","Exterior1st","Exterior2nd","MasVnrType",
       "ExterQual","ExterCond","Foundation", "BsmtQual","BsmtCond",
       "BsmtExposure", "BsmtFinType1","BsmtFinType2","Heating","HeatingQC","CentralAir",
       "Electrical","KitchenQual","Functional","FireplaceQu","GarageType","GarageFinish",
       "GarageQual","GarageCond","PavedDrive","PoolQC","Fence","MiscFeature","SaleType",
       "SaleCondition")
for (i in 1:length(s)){
    tr[,s[i]] <- as.factor(tr[,s[i]])
}
str(tr)


# For the missing GarageYrBlt values, replace those with the YearBuilt value.
tr$GarageYrBlt <- ifelse(is.na(tr$GarageYrBlt), tr$YearBuilt, tr$GarageYrBlt)

# MasVnrType and MasVnrArea both have missing values. Tweak your answers 
# in Q2 and Q3 to show that 1) MasVnrType NA values and MasVnrArea NA values
# correspond to the same rows, and 2) assume that the MasVnrType missing values
# should be adjusted to 'None' and MasVnrArea values to 0. The MasVnrType should
# still be a factor vector with the same factor levels as before.
sum(ifelse(is.na(tr$MasVnrType) & is.na(tr$MasVnrArea), 1, 0))
tr$MasVnrType <- as.factor(ifelse(is.na(tr$MasVnrType),as.character("None")
                                  ,as.character(tr$MasVnrType)))
tr$MasVnrArea <- ifelse(is.na(tr$MasVnrArea), 0, tr$MasVnrArea)

# Delete the row where Electrical values are missing
tr <- tr[!is.na(tr$Electrical),]
#remove the Id column in tr
tr$Id <- NULL

# set the median value for missing values of LotFrontage
tr[is.na(tr$LotFrontage),"LotFrontage"] <- median(tr$LotFrontage, na.rm=T)

# create a dataset called d so I don't have to change alot of code later in 
# in my script
d <- tr

# clean up my R environment
rm(tr, conn, drv, i, realBlanks, s)

# % of rowing having missing values
dim(d[!complete.cases(d),])[[1]]/nrow(d)*100

# Make target variable first column in dataset
d <- d[,c(80,1:79)]
# Make target (SalePrice) column name "y"
names(d)[1] <- "y"
names(d)

################################################################################
## Predictive Modeling using caret
################################################################################
library(caret)
set.seed(123) # so you can replicate results

# 1) Creating Dummy Variables using the caret approach


# 2) identify correlated predictors and remove them (use a corrrelation cutoff of 0.80)


# 3) identify linear dependencies and remove them to reduce the issue of 
#    perfect collinearity



# 4) using preProcess() standardize your numeric features using a min-max normalization


# 5) Create a 70/30 train/test set


# 6) Using trainControl(), specify a 5-fold cross-validation design for a regression problem


# 7) train a linear regression model on the train set using the train() function


# 8) generate predictions using the predict() function on the test set



# 9) using the defaultSummary() function, evaluate the performance of the train
#    and test set. I do this for you. However, based on the Rsquared, do you think
#    the model is overfit?

options(scipen=999) # removes scientific notation in outputs
# train
defaultSummary(data=data.frame(obs=train$y, pred=predict(myModel1, newdata=train))
               , model=myModel1)
# test 
defaultSummary(data=data.frame(obs=test$y, pred=predict(myModel1, newdata=test))
               , model=myModel1)

