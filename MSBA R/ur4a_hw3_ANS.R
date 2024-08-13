################################################################################
# MGMT 590 Using R for Analytics
# Team Assignment #3
# Due: October 7, 2020 11:59pm
#
# Are you in the 2:50-4:20 or 4:30-6:00pm section: _________________________
#
# Team #:_______________________________________________________
#
#
# Submission instructions: This assignment must be completed with your assigned 
# team with no help from others. Only one person from your team will submit your 
# team's solutions to Brightspace on time. It is due by Wednesday, October 7th, 
# 2020 by Midnight. Late submissions for any reason will be subject to a 25% 
# late penalty per day past due. No Brightspace, Scholar, or other excuses.
#
# While there is only one team submission, everyone should try to answer the 
# questions on their own and when in doubt discuss with your teammates. This 
# is the only way you will truly learn the R language. Assigning people to do 
# certain questions of the homework is a bad strategy to prepare you for the 
# final exam. 
# 
# Please put all your answers in the ur4a_hw3.R script. This script should 
# clearly show which question you are answering and have comments where you 
# need to comment to answer the question provided. Some questions will ask you 
# to provide output. In those cases, copy and paste the output into your R script
# and then comment it out.
#
# All work should be done using RStudio Server via Scholar
################################################################################
################################################################################
# Case: Your boss has tasked with you learning how to do descriptive, predictive,
# and prescriptive analytics using RStudio Server. He heard many great data 
# scientists learn from participating in competitions at Kaggle.com. He has 
# ask that you participate in the "House Prices: Advanced Regression Techniques"
# competition to learn R and build predictive models.
# https://www.kaggle.com/c/house-prices-advanced-regression-techniques
# The training and scoring data from this competition is saved on your companies
# MySQL database server and can be accessed using the lines 42 thru 56 below.
#
# set memory limits
options(java.parameters = "-Xmx64048m") # 64048 is 64 GB
# Connect to our MySQL on the servrer
install.packages("odbc")
install.packages("RMariaDB", configure.vars=c("INCLUDE_DIR=/usr/include/mysql LIB_DIR=/usr/lib64/mysql"))

library(RMariaDB)
# Connect to a MariaDB version of a MySQL database
# Do not change the user id or password; Leave it as gen_user
con <- dbConnect(RMariaDB::MariaDB(), host="datamine.rcac.purdue.edu", port=3306
                 , dbname="khou", user="gen_user", password="gen_user")

# pull in the training data into a data.frame called tr and kaggle evaluation
# set called 'te'
tr <- dbGetQuery(con, "select * from train")
te <- dbGetQuery(con, "select * from test")
# disconnect from db
dbDisconnect(con)

#getwd()
# read in training dataset
#tr <- read.table(file="train.csv", header=T, sep=",")
# read in testing dataset
#te <- read.table(file="test.csv", header=T, sep=",")

################################################################################
## Data Cleaning
################################################################################
# You cannot create variable names in R that begin with a number or underscore.
# However, since we pulled this data in from a database, it allowed a couple
# columns to slip by. This can cause us issues later on, so lets fix the following
# column names (1stFlrSF, 2ndFlrSF, 3SsnPorch) to (FstFlrSF, SecFlrSf, ThiSsnPorch)
names(tr)
names(tr)[44] <- "FstFlrSF"; names(te)[44] <- "FstFlrSF"
names(tr)[45] <- "SecFlrSf"; names(te)[45] <- "SecFlrSf"
names(tr)[70] <- "ThiSsnPorch"; names(te)[70] <- "ThiSsnPorch"
names(tr)
# Whenever we make changes to our kaggle 'train' set we need to make sure we
# do similar changes to our 'submission' dataset called 'te'.

# One thing you might notice is that some of the columns show NA implying that
# there are missing values. However, this is misleading as in the database they
# were stored as literal NA values instead of missing values. Some of the values
# indeed should be missing, and I show below how I corrected these for each
# field.

# Features where 'NA' is really a missing value, make them blank
# LotFrontage, MasVnrType, MasVnrArea, Electrical, GarageYrBlt
realBlanks <- c("LotFrontage", "MasVnrType", "MasVnrArea", "Electrical","PoolQC"
                , "GarageYrBlt")
# loop through each field and make 'NA' values missing values
for (i in 1:length(realBlanks)){
    tr[which(tr[,realBlanks[i]]=='NA'),realBlanks[i]] <- NA 
    te[which(te[,realBlanks[i]]=='NA'),realBlanks[i]] <- NA 
}

# Features where 'NA' is a meaningful factor level, we will keep them. These are:
# Alley, BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinType2, FireplaceQu,
# GarageType, GarageFinish, GarageQual, GarageCond, PoolQC, Fence, MiscFeature

NA_features <- c("Alley","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1"
                 ,"BsmtFinType2","FireplaceQu","GarageType","GarageQual","GarageCond","GarageFinish"
                 ,"PoolQC","Fence","MiscFeature")

# loop through each NA_features field and make missing values 'NA' levels
for (i in 1:length(NA_features)){
  tr[which(is.na(tr[,NA_features[i]])), NA_features[i]] <- "NA" 
}

for (i in 1:length(NA_features)){
  te[which(is.na(te[,NA_features[i]])), NA_features[i]] <- "NA" 
}
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

te$LotFrontage <- as.numeric(te$LotFrontage)
te$GarageYrBlt <- as.numeric(te$GarageYrBlt)
te$MasVnrArea <- as.numeric(te$MasVnrArea)

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
    te[,s[i]] <- as.factor(te[,s[i]])
}
str(tr)

# For the missing GarageYrBlt values, replace those with the YearBuilt value.
tr$GarageYrBlt <- ifelse(is.na(tr$GarageYrBlt), tr$YearBuilt, tr$GarageYrBlt)
te$GarageYrBlt <- ifelse(is.na(te$GarageYrBlt), te$YearBuilt, te$GarageYrBlt)

# MasVnrType and MasVnrArea both have missing values. Tweak your answers 
# in Q2 and Q3 to show that 1) MasVnrType NA values and MasVnrArea NA values
# correspond to the same rows, and 2) assume that the MasVnrType missing values
# should be adjusted to 'None' and MasVnrArea values to 0. The MasVnrType should
# still be a factor vector with the same factor levels as before.
sum(ifelse(is.na(tr$MasVnrType) & is.na(tr$MasVnrArea), 1, 0))
tr$MasVnrType <- as.factor(ifelse(is.na(tr$MasVnrType),as.character("None")
                                  ,as.character(tr$MasVnrType)))
tr$MasVnrArea <- ifelse(is.na(tr$MasVnrArea), 0, tr$MasVnrArea)

te$MasVnrType <- as.factor(ifelse(is.na(te$MasVnrType),as.character("None")
                                  ,as.character(te$MasVnrType)))
te$MasVnrArea <- ifelse(is.na(te$MasVnrArea), 0, te$MasVnrArea)

# remove the Id column in tr; Don't remove the id column from the 'te' set as 
# you will need it later to submit your predictions to Kaggle
tr$Id <- NULL

###############################################################################
# Imputing missing values using a predictive model-based imputation approach for
# both the tr and te data.frames.
###############################################################################
# LotFrontage and Electrical features have missing values
sapply(tr, function(x) sum(is.na(x)))
sapply(te, function(x) sum(is.na(x)))

library(mice)    # call the mice library so we can use the functions in this library
?mice            # tells you a bit about the mice() function

# here we use a decision tree predictive model approach to predict the missing
# values for LotFrontage and Electrical. The algorithm used is "cart" which stands 
# for "Classification And Regression Trees".
imputedValues <- mice(data=tr
                      , seed=2016     # keep to replicate results
                      , method="cart" # model you want to use
                      , m=1           # Number of multiple imputations
                      , maxit = 1     # number of iterations
)
# impute the missing values in our tr data.frame
tr <- mice::complete(imputedValues,1) # completely fills in the missing

imputedValues <- mice(data=te
                      , seed=2016     # keep to replicate results
                      , method="cart" # model you want to use
                      , m=1           # Number of multiple imputations
                      , maxit = 1     # number of iterations
)
# impute the missing values in our tr data.frame
te <- mice::complete(imputedValues,1) # completely fills in the missing

# LotFrontage and Electrical features no longer have missing values
sapply(tr, function(x) sum(is.na(x)))
sapply(te, function(x) sum(is.na(x)))

te$Utilities <- NULL
tr$Utilities <- NULL

# A common mistake is people assume that the dataset has values for all factor
# levels. Sometimes one dataset (e.g., training data) might have a few factor 
# levels represented and another dataset (e.g., testing data) has other factor 
# levels appearing. Thus, you need to directly tell R what the set of possible
# factor levels are first based on the data dictionary. If you miss this, it
# can cause you issues later.

# Define factor levels based on data dictionary 
tr$ExterQual <- factor(tr$ExterQual,levels=c("Po","Fa","TA","Gd","Ex"))
te$ExterQual <- factor(te$ExterQual,levels=c("Po","Fa","TA","Gd","Ex"))

# prove that you fixed the issue with all factor levels represented
table(tr$ExterQual)
table(te$ExterQual)

# now we can change the levels to numbers and be confident the numbers shown
# match what we expect them to be everytime.
library(plyr)
tr$ExterQual <- as.numeric(revalue(tr$ExterQual, c("Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
te$ExterQual <- as.numeric(revalue(te$ExterQual, c("Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))

# Perform a similar process as I did for ExterQual to change ExterCond, HeatingQC
# and KitchenQual to values 1 through 5
tr$ExterCond <- factor(tr$ExterCond,levels=c("Po","Fa","TA","Gd","Ex"))
tr$HeatingQC <- factor(tr$HeatingQC,levels=c("Po","Fa","TA","Gd","Ex"))
tr$KitchenQual <- factor(tr$KitchenQual,levels=c("Po","Fa","TA","Gd","Ex"))
tr$ExterCond <- as.numeric(revalue(tr$ExterCond, c("Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
tr$HeatingQC <- as.numeric(revalue(tr$HeatingQC, c("Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
tr$KitchenQual <- as.numeric(revalue(tr$KitchenQual, c("Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))

te$ExterCond <- factor(te$ExterCond,levels=c("Po","Fa","TA","Gd","Ex"))
te$HeatingQC <- factor(te$HeatingQC,levels=c("Po","Fa","TA","Gd","Ex"))
te$KitchenQual <- factor(te$KitchenQual,levels=c("Po","Fa","TA","Gd","Ex"))
te$ExterCond <- as.numeric(revalue(te$ExterCond, c("Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
te$HeatingQC <- as.numeric(revalue(te$HeatingQC, c("Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
te$KitchenQual <- as.numeric(revalue(te$KitchenQual, c("Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))

# Perform a similar process as you did above to change BsmtQual, "BsmtCond", 
# "FireplaceQu", "GarageQual", "GarageCond"to values 0 through 5 where level
# NA is equal to 0
tr$BsmtQual <- factor(tr$BsmtQual,levels=c("NA","Po","Fa","TA","Gd","Ex"))
tr$BsmtCond <- factor(tr$BsmtCond,levels=c("NA","Po","Fa","TA","Gd","Ex"))
tr$FireplaceQu <- factor(tr$FireplaceQu,levels=c("NA","Po","Fa","TA","Gd","Ex"))
tr$GarageQual <- factor(tr$GarageQual,levels=c("NA","Po","Fa","TA","Gd","Ex"))
tr$GarageCond <- factor(tr$GarageCond,levels=c("NA","Po","Fa","TA","Gd","Ex"))

te$BsmtQual <- factor(te$BsmtQual,levels=c("NA","Po","Fa","TA","Gd","Ex"))
te$BsmtCond <- factor(te$BsmtCond,levels=c("NA","Po","Fa","TA","Gd","Ex"))
te$FireplaceQu <- factor(te$FireplaceQu,levels=c("NA","Po","Fa","TA","Gd","Ex"))
te$GarageQual <- factor(te$GarageQual,levels=c("NA","Po","Fa","TA","Gd","Ex"))
te$GarageCond <- factor(te$GarageCond,levels=c("NA","Po","Fa","TA","Gd","Ex"))

tr$BsmtQual <- as.numeric(revalue(tr$BsmtQual, c("NA"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
tr$BsmtCond <- as.numeric(revalue(tr$BsmtCond, c("NA"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
tr$FireplaceQu <- as.numeric(revalue(tr$FireplaceQu, c("NA"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
tr$GarageQual <- as.numeric(revalue(tr$GarageQual, c("NA"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
tr$GarageCond <- as.numeric(revalue(tr$GarageCond, c("NA"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))

te$BsmtQual <- as.numeric(revalue(te$BsmtQual, c("NA"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
te$BsmtCond <- as.numeric(revalue(te$BsmtCond, c("NA"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
te$FireplaceQu <- as.numeric(revalue(te$FireplaceQu, c("NA"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
te$GarageQual <- as.numeric(revalue(te$GarageQual, c("NA"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
te$GarageCond <- as.numeric(revalue(te$GarageCond, c("NA"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
# create a dataset called d so I don't have to change alot of code later in 
# in my script
d <- tr

# Make target variable first column in dataset
d <- d[,c(79,1:78)]

# Make target (SalePrice) column name "y"
names(d)[1] <- "y"
names(d)

# clean up my R environment
rm(tr, con, drv, i, realBlanks, s, imputedValues)
################################################################################
## Predictive Modeling using caret
################################################################################

################################################################################
# Q1 - Using the dummyVars() function, create one-hot encoded variables for all
# the categorical variables. Do this for both the 'd' data.frame and 'te' set
################################################################################

#d set
library(caret)
dummies <- dummyVars(y ~ ., data = d)            # create dummies for Xs
ex <- data.frame(predict(dummies, newdata = d))  # actually creates the dummies
names(ex) <- gsub("\\.", "", names(ex))          # removes dots from col names
d <- cbind(d$y, ex)                              # combine target var with Xs
names(d)[1] <- "y"                               # name target var 'y'
rm(dummies, ex)                                  # clean environment

# te set
dummies <- dummyVars(~ ., data = te)            # create dummies for Xs
te <- data.frame(predict(dummies, newdata = te))  # actually creates the dummies
names(te) <- gsub("\\.", "", names(te))          # removes dots from col names
rm(dummies)

################################################################################
# Q2 - Identify correlated variables in the 'd' dataset that are less than or 
# greater than 80% and remove them. You don't need to do this on the 'te' dataset
################################################################################

descrCor <-  cor(d[,2:ncol(d)])                           # correlation matrix
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .80) # num Xs with cor > t
summary(descrCor[upper.tri(descrCor)])                    # summarize the cors

# which columns in your correlation matrix have a correlation greater than some
# specified absolute cutoff. Find them and remove them
highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.80)
filteredDescr <- d[,2:ncol(d)][,-highlyCorDescr] # remove those specific columns
descrCor2 <- cor(filteredDescr)                  # calculate a new cor matrix
# summarize those correlations to see if all features are now within our range
summary(descrCor2[upper.tri(descrCor2)])

# update dataset by removing those filtered vars that were highly correlated
d <- cbind(d$y, filteredDescr)
names(d)[1] <- "y"

rm(filteredDescr, descrCor, descrCor2, highCorr, highlyCorDescr)  # clean up

################################################################################
# Q3 - identify linear dependencies and remove them to reduce the issue of 
# perfect collinearity using the findLinearCombos() function. Do this on the 
# 'd' data.frame only.
################################################################################

# first save response
y <- d$y

# create a column of 1s. This will help identify all the right linear combos
d <- cbind(rep(1, nrow(d)), d[2:ncol(d)])
names(d)[1] <- "ones"

# identify the columns that are linear combos
comboInfo <- findLinearCombos(d)
comboInfo

# remove columns identified that led to linear combos
d <- d[, -comboInfo$remove]

# remove the "ones" column in the first column
d <- d[, c(2:ncol(d))]

# Add the target variable back to our data.frame
d <- cbind(y, d)

rm(y, comboInfo)  # clean up

################################################################################
# Q4 - remove features with limited variation using nearZeroVar() from the 'd'
# data.frame the way I showed in the notes. However, first look at the head of 
# nzv by using head(nzv), and remove columns where nzv equals to TRUE. This 
# means you need to modify this line of code (d <- d[, c(TRUE,!nzv$zeroVar[2:ncol(d)])])
# somehow. You do not need to remove anything from 'te'.
################################################################################

nzv <- nearZeroVar(d, saveMetrics = TRUE)
head(nzv)

#d <- d[, c(TRUE,!nzv$zeroVar[2:ncol(d)])]
d <- d[, c(TRUE,!nzv$nzv[2:ncol(d)])]
names(d)

rm(nzv) # clean up R environment

################################################################################
# Q5 - using preProcess(), standardize your numeric features using a min-max 
# normalization. Do this for both the 'd' and 'te' data sets. 
# hint: Remember, the 'te' dataset does not have a Y column and you should not
#       transform the Id column because you are not using that for modeling, but
#       to submit predictions for each Id later.
################################################################################

# d set
preProcValues <- preProcess(d[,2:ncol(d)], method = c("range"))
d <- predict(preProcValues, d)
# te set
preProcValues <- preProcess(te[,2:ncol(te)], method = c("range"))
te <- predict(preProcValues, te)

rm(preProcValues) # clean up R environment

################################################################################
# Q6 - Create a 90/10 train/test set using createDataPartition(). Discuss why
# you the function requires you to specify your target variable column. What
# kind of random sampling is going on?
################################################################################

set.seed(1234) # set a seed so you can replicate your results
inTrain <- createDataPartition(y = d$y,   # outcome variable
                               p = .90,   # % of training data you want
                               list = F)
# create your partitions
train <- d[inTrain,]  # training data set
test <- d[-inTrain,]  # test data set

# ANSWER: createDataPartition() is doing stratified random sampling to try and 
# and make the train and test sets more similar than simple random sampling.

rm(inTrain) # clean up R environment

################################################################################
# Q7 - Using trainControl(), specify a 5-fold cross-validation design.
################################################################################

ctrl <- trainControl(method="cv",     # cross-validation set approach to use
                     number=5,        # k number of times to do k-fold
                     classProbs = F,  # if you want probabilities
                     #summaryFunction = twoClassSummary, # for classification
                     summaryFunction = defaultSummary,  # for regression
                     allowParallel=T)

################################################################################
# Q8 -  train a linear regession model on the train set using the train() function.
# Call your model 'm1'. Use RMSE as your metric.
################################################################################

m1 <- train(y ~ .,               # model specification
            data = train,        # train set used to build model
            method = "lm",      # type of model you want to build
            trControl = ctrl,    # how you want to learn
            metric = "RMSE"       # performance measure
)

################################################################################
# Q9 -  train a linear regession model on the train set using the train() function.
# However, for this model try to take the log(y) as your target variable when
# you train your model. Call your model 'm2'. Use RMSE as your metric.
################################################################################

m2 <- train(log(y) ~ .,               # model specification
            data = train,        # train set used to build model
            method = "lm",      # type of model you want to build
            trControl = ctrl,    # how you want to learn
            metric = "RMSE"       # performance measure
)

################################################################################
# Q10 (2 points) - using the defaultSummary() function, evaluate the performance 
# of the train and test sets for both models. Based on the RMSE values, are any 
# models overfit? Also, which model is "best"? Justify the reasoning for your 
# answers based on the statistics.
################################################################################

options(scipen=999) # removes scientific notation in outputs
# m1 train
defaultSummary(data=data.frame(obs=train$y, pred=predict(m1, newdata=train)), model=m1)
# m1 test 
defaultSummary(data=data.frame(obs=test$y, pred=predict(m1, newdata=test)), model=m1)

# m2 train
defaultSummary(data=data.frame(obs=log(train$y)
                               , pred=predict(m2, newdata=train)), model=m2)
# m2 test 
defaultSummary(data=data.frame(obs=log(test$y)
                               , pred=predict(m2, newdata=test)), model=m2)

# ANSWER: m1 is not overfit because the train and test set statistics are very close.
# For example, RMSE (train=30501 vs.test=27374). m2 is also not overfit for the 
# same reasoning - RMSE (train=0.13 vs.test=0.13). 
# Students should use the log() in the obs= argument for the m2 model.
# Comparing the test set statistics for both of these candidate models, would suggest
# that m1 is better as all statistics are better on the test set. 

################################################################################
# Q11 (2 points) - generate predictions using the predict() function on the 'te' data.frame
# using the best model you found from the previous question. These values should
# be saved in a numeric vector called 'preds'. Next, create a new data.frame
# called 'results' that has the 'Id' column from the 'te' data.frame in the first
# column and 'preds' in the second column. The column names for the results table
# should be c("Id","SalePrice"). Lastly, write this out into a comma-seperated
# file called "results.csv" using the write.table() function. Make sure you
# specify row.names=F and sep="," as arguments in your write.table function.
################################################################################

preds <- predict(m2, newdata=te)
results <- data.frame(Id=te$Id, SalePrice=exp(preds))
head(results)
getwd()
write.table(x=results, sep=",", file="results.csv", row.names=F)
# If students used m2, they should have exponentiated their predictions. 

################################################################################
# Q12 - Go to https://www.kaggle.com/c/house-prices-advanced-regression-techniques/submit
# to upload your "results.csv" file for the Kaggle competition. Report your rank
# and score below. If you click on 'Jump to your position on the leaderboard'.
# it will show your rank.
# Note: You will need to create a Kaggle account (which is Free) and join the 
# competition to submit your predictions.
################################################################################
# student should show a rank and score 

rm(ctrl, preds) # clean up

################################################################################
## Clustering
#
# Assuming your properly standardized your features using a min-max normalization
# in the previous prediction problem above, you are going to perform clustering here
# using the train and test set you already created.
################################################################################

################################################################################
# Q13 (3 points) - write a for loop that performs k-means clustering on your train set, as
# well as your test set you created from your previous problems. The loop should
# run from k=1 to 10. Have the number of random starts be 25 and the maximum number 
# of algorithm iterations set at 100. Plot the total within sum of squared errors
# in an elbox plot. Use a seed of 123.
# Based on your elbow plot, does the clustering in a train and test set appear
# to be replicable? Explain what you observed and justify why you believe the
# results turned out the way that they did.
################################################################################

# Set seed for reproducibility
set.seed(123)

# Create an empty data frame to store the cost results
cost_df <- data.frame()

# Select the column names from the second to the last column
tr_col <- colnames(d)[2:ncol(d)]

# Perform k-means clustering for k ranging from 1 to 10
for(k in 1:10) {
  # Train set k-means
  kmeans_tr <- kmeans(x = d[, tr_col], centers = k, nstart = 25, iter.max = 100)
  # Test set k-means
  kmeans_te <- kmeans(x = te[, tr_col], centers = k, nstart = 25, iter.max = 100)
  # Combine cluster number and cost together, write to cost_df
  cost_df <- rbind(cost_df, cbind(k, kmeans_tr$tot.withinss, kmeans_te$tot.withinss))
}

# Assign column names to cost_df
names(cost_df) <- c("cluster", "tr_cost", "te_cost")

# Print the cost_df data frame
print(cost_df)

# divide by 1000 to get MSE in 1000s
cost_df[, 2:3] <- cost_df[, 2:3] / 1000

# Create the elbow plot
par(mfrow = c(1, 1))
plot(x = cost_df$cluster, y = cost_df$tr_cost, main = "k-Means Elbow Plot",
     col = "blue", pch = 19, type = "b", cex.lab = 1.2,
     xlab = "Number of Clusters", ylab = "MSE (in 1000s)")
points(x = cost_df$cluster, y = cost_df$te_cost, col = "red")

# ANSWER: The elbow plots for the train and test sets were not much different and
# suggest a clustering can be repeatable.
# The elbow plot suggests a kink somewhere between 5 and 7 clusters.

################################################################################
# Q14 -Write another for loop that runs k-means algorithm on the entire 'd' dataset on
# the input variables. Don't create a train or test set. Have the number of starts
# and max iterations be the same as in the previous problem. Run this for k=1 to 100.
# Based on your elbow plot, identify two possible k values that might lead to
# a good clustering. This is subjective - justify your answer.
################################################################################

# Set the seed for reproducibility
set.seed(123)

# Create an empty data frame to accumulate cost results
cost_df <- data.frame()

# Perform k-means clustering for k from 1 to 100
for (k in 1:100) {
  # k-means clustering on the entire dataset
  kmeans_result <- kmeans(x = d[, 2:ncol(d)], centers = k, nstart = 25, iter.max = 100)
  
  # Combine cluster number and cost together, write to cost_df
  cost_df <- rbind(cost_df, cbind(k, kmeans_result$tot.withinss))
}

# Assign column names to cost_df
names(cost_df) <- c("cluster", "cost")

# Display the cost_df data frame
print(cost_df)

# Create an elbow plot
par(mfrow = c(1, 1))

# Scale the cost by 1000 for easier visualization
cost_df$cost <- cost_df$cost / 1000

# Plot the elbow plot
plot(x = cost_df$cluster, y = cost_df$cost, main = "k-Means Elbow Plot",
     col = "blue", pch = 19, type = "b", cex.lab = 1.2,
     xlab = "Number of Clusters", ylab = "MSE (in 1000s)")

# Add legend
legend("topright", legend = "Total Within SS", col = "blue", pch = 19)

# The elbow plot suggests a kink somewhere at 20 clusters.This can be subjective.

################################################################################
# Q15 - Perform k-means clustering on the 'd' dataset only on the input variables 
# with the k values you determined in the previous question. Use the same number of 
# starts and max iterations. Once you run these two models, clearly show the
# model parameters for each k-means model.
################################################################################

kmeans_5 <- kmeans(x=d[,2:ncol(d)], centers=5, nstart=25, iter.max=100)
kmeans_8 <- kmeans(x=d[,2:ncol(d)], centers=8, nstart=25, iter.max=100)

# k=5 k-means model
kmeans_5$centers

# k=8 k-means model
kmeans_8$centers

################################################################################
# Q16 - Visually inspect the cluster formations you obtained in the previous
# question using the 'useful' library. Plot the clusterings in one figure by 
# sourcing the multiplot.R function and calling it.
################################################################################

source("multiplot.R")
library(useful)

library(useful)
p1 <- plot(kmeans_5, data=d[,2:ncol(d)])
p2 <- plot(kmeans_8, data=d[,2:ncol(d)])
multiplot(p1, p2)

rm(p1, p2) # clean up

# ANSWER: The kmeans model with k=5 shows a fairly clean clustering with 
# cohesive clusters, but some separation among them. The kmeans model with
# k=8 has much more overlap (less separation).

################################################################################
# Q17 (2 points) - Create silhoutte plots for the two k's you identified previously using the 
# same settings used previously. Based on the output of these plots, justify
# which k is "best"? If it is inconclusive explain why.
################################################################################

library(cluster)

# kmeans (k=5)
km5 <- kmeans(x=d[,2:ncol(d)], centers=5, nstart=25, iter.max=100)
dist5 <- dist(d[,2:ncol(d)], method="euclidean")
sil5 <- silhouette(km5$cluster, dist5)

# kmeans (k=8)
km8 <- kmeans(x=d[,2:ncol(d)], centers=8, nstart=25, iter.max=100)
dist8 <- dist(d[,2:ncol(d)], method="euclidean")
sil8 <- silhouette(km8$cluster, dist8)

# silhoutte plots
par(mfrow=c(1,2))
library(wesanderson)
plot(sil5, col=wes_palette("Zissou1", 5, type = "continuous")
     , main="Silhouette plot (k=5) K-means", border=NA)
plot(sil8, col=wes_palette("Zissou1", 8, type = "continuous")
     , main="Silhouette plot (k=8) K-means", border=NA)

# ANSWER: My average silhouette width is 0.10 for both k=5 and k=8 is 0.10.
# Thus, there is no difference among the two clusterings.

# clean up
rm(cost_df, km5, km8, kmeans_5, kmeans_te, kmeans_tr, dist8, dist8, k, sil5, sil8)

################################################################################
## Predictive Modeling using H2O
################################################################################

################################################################################
# Q18 - Initialize your h2o cluster in RStudio Server by specifying you want
# 12 cores (i.e. nthreads) and a max memory of 64 GB. Next, load your 'd' data.frame
# into the h2o cluster.
################################################################################

# initialize cluster
library(h2o)
h2o.init(nthreads=4, max_mem_size="8g")

# load data into h2o cluster
data <- as.h2o(d)

################################################################################
# Q19 (3 points) - using setdiff() and h2o.splitFrame functions, partition your data set
# into an 80/20 split. Use a seed of 99. Next, train a Random Forest, Deep Learning,
# and Gradient Boosting Machine model. Using h2o.performance() show the performance
# on the train and test sets for all three models. Add your outputs into the
# R script and comment them out. Explain which are "candidate models" (i.e. not
# over fit) based on RMSE, and which one performed the "best". Justify your answers.
################################################################################

y <- "y"                                    # target variable to learn
x <- setdiff(names(data), y)                # features are all other columns
parts <- h2o.splitFrame(data, .8, seed=99)  # randomly partition data into 80/20
train <- parts[[1]]                         # random set of training obs
test <- parts[[2]]                          # random set of testing obs

# models
rf <- h2o.randomForest(x, y, training_frame=train, nfolds=5)
dl <- h2o.deeplearning(x, y, training_frame=train, nfolds=5)
gbm <- h2o.gbm(x, y, training_frame=train, nfolds=5)

# model evaluation
h2o.performance(rf, xval=T)
#RMSE:  32816.62

h2o.performance(rf, newdata=test)
#RMSE:  31742.88

h2o.performance(dl, xval=T)
#RMSE:  36433.61

h2o.performance(dl, newdata=test)
#RMSE:  36834.04

h2o.performance(gbm, xval=T)
#RMSE:  30758.69

h2o.performance(gbm, newdata=test)
#RMSE:  34140.49

# ANSWER: 
# Interesting the RF model did better on the holdout (test) set compared to the
# training set thus it is not overfit. The deep learning model is not overfit. 
# The GBM model shows the most overfitting, but all could be considered
# candidate models.
# Overall, the RF performed best among the three models 
# (31742.88 < 36834.04 & 34140.49)

################################################################################
# Q20 - In both the train and test sets, transform the 'y' target column to 
# log(y). Then re-train a gradient boosting machine model. Comment out, but show
# the performance on the train and test sets. Is this model overfit based on 
# RMSE?
################################################################################

train$y <- log(train$y)
test$y <- log(test$y)
train$y
gbm <- h2o.gbm(x, y, training_frame=train, nfolds=5)

h2o.performance(gbm, xval=T)
#RMSE:  0.1478276

h2o.performance(gbm, newdata=test)
#RMSE:  0.1442966

# ANSWER: No, the model is not overfit as RMSE (train=0.1478276 vs. test=0.1442966).


################################################################################
# Q21 (2 points) - Use h2o's driverless AI functionality by using auto.ml. Set the max run
# time (in seconds) to at least 300 (5 minutes). Once the the best (i.e. champion)
# has been obtained, generate predictions on the 'te' dataset using that model.
# Similarly to question 9, write our these predictions into a file called
# "h2o_results.csv". Report your score.
################################################################################

# train with AutoML - specify how long you are willing to wait
auto <- h2o.automl(x, y, train, max_runtime_secs=300)
auto
str(auto)

# load 'te' dataset into h2o cluster
data2 <- as.h2o(te)

# make predictions
p <- h2o.predict(auto, data2)
p <- as.data.frame(exp(p))    # if students didn't exp() their preds, they lose 1 point.
head(p)

h2o_results <- data.frame(Id=te$Id, SalePrice=p)
names(h2o_results)[2] = "SalePrice"
write.table(x=h2o_results, sep=",", file="h2o_results.csv", row.names=F)

################################################################################
# Q22 (4 points) - Create a simple shiny app that: 
# a) pulls data from housing tr data from the dataset (make sure dbDisconnect(conn)
#    is used right after you get the tr dataset)
# b) generates a predictive model
# c) plots the predicted SalePrice versus the actual SalePrice
# d) shows train and test set statistics
################################################################################

#This is an example. Students can make variations to their shiny app. 

library(shiny)
library(ggplot2)
library(caret)
library(dplyr)
library(RMariaDB)
library(mice)
library(plyr)

# Define UI
ui <- fluidPage(
  titlePanel("Housing Data Predictive Model"),
  sidebarLayout(
    sidebarPanel(
      h4("Training and Testing Statistics"),
      verbatimTextOutput("train_stats"),
      verbatimTextOutput("test_stats")
    ),
    mainPanel(
      plotOutput("prediction_plot")
    )
  )
)

# Define server
server <- function(input, output) {
  # Connect to the database and pull data
  con <- dbConnect(RMariaDB::MariaDB(), host="datamine.rcac.purdue.edu", port=3306, dbname="khou", user="gen_user", password="gen_user")
  tr <- dbGetQuery(con, "select * from train")
  te <- dbGetQuery(con, "select * from test")
  dbDisconnect(con)
  
  #or 
  
  #getwd()
  #tr<-read.table("train.csv", header=T, sep=",")
  #te<-read.table("test.csv", header=T, sep=",")
  
  # Data Cleaning and Preparation
  names(tr)[44] <- "FstFlrSF"; names(te)[44] <- "FstFlrSF"
  names(tr)[45] <- "SecFlrSf"; names(te)[45] <- "SecFlrSf"
  names(tr)[70] <- "ThiSsnPorch"; names(te)[70] <- "ThiSsnPorch"
  
  realBlanks <- c("LotFrontage", "MasVnrType", "MasVnrArea", "Electrical", "GarageYrBlt")
  for (i in 1:length(realBlanks)) {
    tr[which(tr[,realBlanks[i]]=='NA'),realBlanks[i]] <- NA 
    te[which(te[,realBlanks[i]]=='NA'),realBlanks[i]] <- NA 
  }
  
  NA_features <- c("Alley","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2","FireplaceQu","GarageType","GarageQual","GarageCond","GarageFinish","PoolQC","Fence","MiscFeature")
  for (i in 1:length(NA_features)){
    tr[which(is.na(tr[,NA_features[i]])), NA_features[i]] <- "NA" 
    te[which(is.na(te[,NA_features[i]])), NA_features[i]] <- "NA" 
  }
  
  num_col <- c("LotFrontage","GarageYrBlt","MasVnrArea")
  for (i in 1:length(num_col)){
    tr[,num_col[i]] <- as.numeric(tr[,num_col[i]])
    te[,num_col[i]] <- as.numeric(te[,num_col[i]])
  }
  
  s <- c("MSSubClass","MSZoning","Street","Alley","LotShape","LandContour","Utilities","LotConfig","LandSlope","Neighborhood","Condition1","Condition2","BldgType","HouseStyle","RoofStyle","RoofMatl","Exterior1st","Exterior2nd","MasVnrType","ExterQual","ExterCond","Foundation", "BsmtQual","BsmtCond","BsmtExposure", "BsmtFinType1","BsmtFinType2","Heating","HeatingQC","CentralAir","Electrical","KitchenQual","Functional","FireplaceQu","GarageType","GarageFinish","GarageQual","GarageCond","PavedDrive","PoolQC","Fence","MiscFeature","SaleType","SaleCondition")
  for (i in 1:length(s)){
    tr[,s[i]] <- as.factor(tr[,s[i]])
    te[,s[i]] <- as.factor(te[,s[i]])
  }
  
  tr$GarageYrBlt <- ifelse(is.na(tr$GarageYrBlt), tr$YearBuilt, tr$GarageYrBlt)
  te$GarageYrBlt <- ifelse(is.na(te$GarageYrBlt), te$YearBuilt, te$GarageYrBlt)
  tr$MasVnrType <- as.factor(ifelse(is.na(tr$MasVnrType),as.character("None"),as.character(tr$MasVnrType)))
  tr$MasVnrArea <- ifelse(is.na(tr$MasVnrArea), 0, tr$MasVnrArea)
  te$MasVnrType <- as.factor(ifelse(is.na(te$MasVnrType),as.character("None"),as.character(te$MasVnrType)))
  te$MasVnrArea <- ifelse(is.na(te$MasVnrArea), 0, te$MasVnrArea)
  
  tr$Id <- NULL
  
  imputedValues <- mice(data=tr, seed=2016, method="cart", m=1, maxit = 1)
  tr <- mice::complete(imputedValues,1)
  imputedValues <- mice(data=te, seed=2016, method="cart", m=1, maxit = 1)
  te <- mice::complete(imputedValues,1)
  
  te$Utilities <- NULL
  tr$Utilities <- NULL
  
  tr$ExterQual <- factor(tr$ExterQual,levels=c("Po","Fa","TA","Gd","Ex"))
  te$ExterQual <- factor(te$ExterQual,levels=c("Po","Fa","TA","Gd","Ex"))
  tr$ExterQual <- as.numeric(revalue(tr$ExterQual, c("Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
  te$ExterQual <- as.numeric(revalue(te$ExterQual, c("Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
  
  tr$ExterCond <- factor(tr$ExterCond,levels=c("Po","Fa","TA","Gd","Ex"))
  tr$HeatingQC <- factor(tr$HeatingQC,levels=c("Po","Fa","TA","Gd","Ex"))
  tr$KitchenQual <- factor(tr$KitchenQual,levels=c("Po","Fa","TA","Gd","Ex"))
  tr$ExterCond <- as.numeric(revalue(tr$ExterCond, c("Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
  tr$HeatingQC <- as.numeric(revalue(tr$HeatingQC, c("Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
  tr$KitchenQual <- as.numeric(revalue(tr$KitchenQual, c("Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
  te$ExterCond <- factor(te$ExterCond,levels=c("Po","Fa","TA","Gd","Ex"))
  te$HeatingQC <- factor(te$HeatingQC,levels=c("Po","Fa","TA","Gd","Ex"))
  te$KitchenQual <- factor(te$KitchenQual,levels=c("Po","Fa","TA","Gd","Ex"))
  te$ExterCond <- as.numeric(revalue(te$ExterCond, c("Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
  te$HeatingQC <- as.numeric(revalue(te$HeatingQC, c("Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
  te$KitchenQual <- as.numeric(revalue(te$KitchenQual, c("Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
  
  tr$BsmtQual <- factor(tr$BsmtQual,levels=c("NA","Po","Fa","TA","Gd","Ex"))
  tr$BsmtCond <- factor(tr$BsmtCond,levels=c("NA","Po","Fa","TA","Gd","Ex"))
  tr$FireplaceQu <- factor(tr$FireplaceQu,levels=c("NA","Po","Fa","TA","Gd","Ex"))
  tr$GarageQual <- factor(tr$GarageQual,levels=c("NA","Po","Fa","TA","Gd","Ex"))
  tr$GarageCond <- factor(tr$GarageCond,levels=c("NA","Po","Fa","TA","Gd","Ex"))
  te$BsmtQual <- factor(te$BsmtQual,levels=c("NA","Po","Fa","TA","Gd","Ex"))
  te$BsmtCond <- factor(te$BsmtCond,levels=c("NA","Po","Fa","TA","Gd","Ex"))
  te$FireplaceQu <- factor(te$FireplaceQu,levels=c("NA","Po","Fa","TA","Gd","Ex"))
  te$GarageQual <- factor(te$GarageQual,levels=c("NA","Po","Fa","TA","Gd","Ex"))
  te$GarageCond <- factor(te$GarageCond,levels=c("NA","Po","Fa","TA","Gd","Ex"))
  tr$BsmtQual <- as.numeric(revalue(tr$BsmtQual, c("NA"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
  tr$BsmtCond <- as.numeric(revalue(tr$BsmtCond, c("NA"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
  tr$FireplaceQu <- as.numeric(revalue(tr$FireplaceQu, c("NA"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
  tr$GarageQual <- as.numeric(revalue(tr$GarageQual, c("NA"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
  tr$GarageCond <- as.numeric(revalue(tr$GarageCond, c("NA"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
  te$BsmtQual <- as.numeric(revalue(te$BsmtQual, c("NA"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
  te$BsmtCond <- as.numeric(revalue(te$BsmtCond, c("NA"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
  te$FireplaceQu <- as.numeric(revalue(te$FireplaceQu, c("NA"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
  te$GarageQual <- as.numeric(revalue(te$GarageQual, c("NA"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
  te$GarageCond <- as.numeric(revalue(te$GarageCond, c("NA"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)))
  
  d <- tr
  d <- d[,c(79,1:78)]
  names(d)[1] <- "y"
  dummies <- dummyVars(y ~ ., data = d)
  ex <- data.frame(predict(dummies, newdata = d))
  names(ex) <- gsub("\\.", "", names(ex))
  d <- cbind(d$y, ex)
  names(d)[1] <- "y"
  
  dummies <- dummyVars(~ ., data = te)
  te <- data.frame(predict(dummies, newdata = te))
  names(te) <- gsub("\\.", "", names(te))
  
  descrCor <-  cor(d[,2:ncol(d)])
  highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.80)
  filteredDescr <- d[,2:ncol(d)][,-highlyCorDescr]
  d <- cbind(d$y, filteredDescr)
  names(d)[1] <- "y"
  
  y <- d$y
  d <- cbind(rep(1, nrow(d)), d[2:ncol(d)])
  names(d)[1] <- "ones"
  comboInfo <- findLinearCombos(d)
  d <- d[, -comboInfo$remove]
  d <- d[, c(2:ncol(d))]
  d <- cbind(y, d)
  
  nzv <- nearZeroVar(d, saveMetrics = TRUE)
  d <- d[, c(TRUE,!nzv$nzv[2:ncol(d)])]
  
  preProcValues <- preProcess(d[,2:ncol(d)], method = c("range"))
  d <- predict(preProcValues, d)
  preProcValues <- preProcess(te[,2:ncol(te)], method = c("range"))
  te <- predict(preProcValues, te)
  
  set.seed(1234)
  inTrain <- createDataPartition(y = d$y, p = .90, list = F)
  train <- d[inTrain,]
  test <- d[-inTrain,]
  
  ctrl <- trainControl(method="cv", number=5, classProbs = F, summaryFunction = defaultSummary, allowParallel=T)
  
  m1 <- train(y ~ ., data = train, method = "lm", trControl = ctrl, metric = "RMSE")
  m2 <- train(log(y) ~ ., data = train, method = "lm", trControl = ctrl, metric = "RMSE")
  
  output$prediction_plot <- renderPlot({
    predictions <- predict(m1, test)
    ggplot(test, aes(x = y, y = predictions)) +
      geom_point() +
      geom_abline(slope = 1, intercept = 0, color = "red") +
      labs(x = "Actual SalePrice", y = "Predicted SalePrice", title = "Predicted vs Actual SalePrice")
  })
  
  output$train_stats <- renderPrint({
    m1_train <- defaultSummary(data = data.frame(obs = train$y, pred = predict(m1, newdata = train)), model = m1)
    m2_train <- defaultSummary(data = data.frame(obs = log(train$y), pred = predict(m2, newdata = train)), model = m2)
    list(m1_train = m1_train, m2_train = m2_train)
  })
  
  output$test_stats <- renderPrint({
    m1_test <- defaultSummary(data = data.frame(obs = test$y, pred = predict(m1, newdata = test)), model = m1)
    m2_test <- defaultSummary(data = data.frame(obs = log(test$y), pred = predict(m2, newdata = test)), model = m2)
    list(m1_test = m1_test, m2_test = m2_test)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
