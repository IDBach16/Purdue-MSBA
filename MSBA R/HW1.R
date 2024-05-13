################################################################################
# MGMT 590 Using R for Analytics
# Team Assignment #1
# Due: 09/09/2020 11:59pm
#
# Are you in the 2:50-4:20 or 4:30-6:00pm section: ______Online___________________
#
# Team #:______Group 2_________________________________________________
#
#
# Submission instructions: This assignment must be completed with your assigned 
# team with no help from others. Only one person from your team will submit your 
# team's solutions to Brightspace on time. It is due by Wednesday, September 9th, 
# 2020 by Midnight. Late submissions for any reason will be subject to a 25% 
# late penalty per day past due. No Brightspace, Scholar, or other excuses.
#
# While there is only one team submission, everyone should try to answer the 
# questions on their own and when in doubt discuss with your teammates. This 
# is the only way you will truly learn the R language. Assigning people to do 
# certain questions of the homework is a bad strategy to prepare you for the 
# final exam. 
# 
# Please put all your answers in the hw1_main.R script. This script should 
# clearly show which question you are answering and have comments where you 
# need to comment to answer the question provided.
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
# The data from this competition is saved on your companies MySQL database
# server and can be accessed using the lines 40 thru 47 below.
#
# set memory limits
options(java.parameters = "-Xmx64048m") # 64048 is 64 GB
# connect to db, pull in table, and then close connection.

install.packages("odbc")
install.packages("RMariaDB", configure.vars=c("INCLUDE_DIR=/usr/include/mysql LIB_DIR=/usr/lib64/mysql"))

# Connect to a MariaDB version of a MySQL database
# Do not change the user id or password; Leave it as gen_user
# con <- dbConnect(RMariaDB::MariaDB(), host="datamine.lanhamm.geddes.rcac.purdue.edu"
#                  , port=3306, dbname="khou", user="gen_user", password="gen_user")
# # list of db tables
# dbListTables(con)
# # query table
# tr <- dbGetQuery(con, "SELECT * FROM train")
# te <- dbGetQuery(con, "SELECT * FROM test")
# # close db connection
# suppressWarnings(dbDisconnect(con)); rm(con)

################################################################################
## Q1
################################################################################
# use dbGetQuery() to pull a dataset from the MySQL database called 'train'. Call
# it 'tr' in R



library(RMariaDB)

con <- dbConnect(RMariaDB::MariaDB(), host="datamine.lanhamm.geddes.rcac.purdue.edu"
                 , port=3306, dbname="khou", user="gen_user", password="gen_user")
# list of db tables
#dbListTables(con)
# query table
tr <- dbGetQuery(con, "SELECT * FROM train")
# close db connection
suppressWarnings(dbDisconnect(con)); rm(con)


# use dbGetQuery() to pull a dataset from the MySQL database called 'test'. Call
# it 'te' in R

con <- dbConnect(RMariaDB::MariaDB(), host="datamine.lanhamm.geddes.rcac.purdue.edu"
                 , port=3306, dbname="khou", user="gen_user", password="gen_user")
# list of db tables
#dbListTables(con)
# query tabletr <- dbGetQuery(con, "SELECT * FROM train")
te <- dbGetQuery(con, "SELECT * FROM test")
# close db connection
suppressWarnings(dbDisconnect(con)); rm(con)



################################################################################
## Q2
################################################################################
# use R code to easily identify how R views each column data type in your tr 
# dataset

sapply(tr,class)

################################################################################
## Q3
################################################################################
# print the first three rows in the tr dataset. Then print the last two rows.

head(tr, n=3)
tail(tr, n=2)
# One thing you might notice is that some of the columns show NA implying that
# there are missing values. However, this is misleading as in the database they
# were stored as literal NA values instead of missing values. Thus R thinks
# these values are meaningful characters. As an analyst you might have to correct
# a dataset where certain values exist but really indicate missing values. Writing
# a 'for loop' is one way you might accomplish this task. You'll learn the details
# of this in the coming weeks.
for (i in 1:ncol(tr)){
  tr[which(tr[,i]=='NA'),i] <- NA 
}

# now print the first three rows in the tr dataset again, and notice the difference
# in how the missing values look when printed compared to before you ran the for
# loop

head(tr, n=3) # they now show as <NA> instead of just NA, indicating they're really missing values and not characters/literal values

################################################################################
## Q4
################################################################################
# Read about the dataset by reviewing the data dictionary for this data located
# here: https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data
# Based on your understanding of data, coerce every variable in the 'tr' dataset
# to the correct R data type it should for proper data analysis. If you think it
# should be an integer, go ahead an coerce to a numeric. If you think a categorical
# variable, then coerce to a factor vector. Remember: Just because something shows
# as a number in a database, does not mean it must be a numeric data type.


#tr$Id <- as.numeric(tr$Id)
#tr$MSSubClass <- as.factor(tr$MSSubClass)
#tr$MSZoning <- as.factor(tr$MSZoning)
#tr$LotFrontage <-  as.numeric(tr$LotFrontage)
#tr$LotArea <- as.numeric(tr$LotArea)
#tr$Street <- as.factor(tr$Street)
#tr$Alley <- as.factor(tr$Alley)

#faster way found here: https://stackoverflow.com/questions/33180058/coerce-multiple-columns-to-factors-at-once


cols<- c("MSSubClass","MSZoning","Street", "Alley","LotShape", "LandContour", "Utilities", "LotConfig", "LandSlope", 
         "Neighborhood", "Condition1", "Condition2", "BldgType", "HouseStyle", "OverallQual", "OverallCond", "YearBuilt", 
         "YearRemodAdd", "RoofStyle","RoofMatl", "Exterior1st", "Exterior2nd", "MasVnrType", "ExterQual", "ExterCond", 
         "Foundation", "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2", "Heating", "HeatingQC", 
         "CentralAir", "Electrical", "BsmtFullBath", "BsmtHalfBath", "FullBath", "HalfBath", "BedroomAbvGr", "KitchenAbvGr", 
         "KitchenQual", "TotRmsAbvGrd", "Functional", "Fireplaces", "FireplaceQu", "GarageType", "GarageYrBlt", "GarageFinish", 
         "GarageCars", "GarageQual", "GarageCond", "PavedDrive", "PoolQC", "Fence", "MiscFeature", "MiscVal", "MoSold", 
         "YrSold", "SaleType", "SaleCondition")
tr[,cols] <- lapply(tr[,cols], factor)

cols<- c("Id", "LotFrontage", "LotArea", "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF", "1stFlrSF", 
         "2ndFlrSF", "LowQualFinSF", "GrLivArea", "GarageArea", "WoodDeckSF", "OpenPorchSF", "EnclosedPorch", "3SsnPorch", 
         "ScreenPorch", "PoolArea", "SalePrice")
tr[,cols] <- lapply(tr[,cols], as.numeric)

sapply(tr, class)


################################################################################
## Q5
################################################################################
# use the DataQualityReportOverall.R function shown in class to understand the
# number of complete records. Copy the output into this R script and comment it
# out to prove you were successful in running it.

#must download and extract my.functions folder first, then uplaod DataQualtiyReport.R and DataQualityReportOverall.R to scholar before this will work.

source("DataQualityReportOverall.R")
DataQualityReportOverall(tr)

#   CompleteCases IncompleteCases CompleteCasePct
#1             0            1460               0


################################################################################
## Q6
################################################################################
# Use the DataQualityReport.R function to show the percent of complete records 
# by variable. However, add brackets at the end of the function call to subset
# the result set to return only those records where NumberMissing>0. Also, do not
# show the NumberLevels in the output. Copy and paste the output in your R script 
# and comment it out to prove you successfully were able to get the correct result.

source("DataQualityReport.R")
DataQualityReport(tr)[DataQualityReport(tr)$NumberMissing>0,1:8]

# Attributes    Type NumberMissing PercentComplete Min    Avg Median  Max
# 4   LotFrontage numeric           259           82.26  21  70.05     69  313
# 7         Alley  factor          1369            6.23   -      -      -    -
#   26   MasVnrType  factor             8           99.45   -      -      -    -
#   27   MasVnrArea numeric             8           99.45   0 103.69      0 1600
# 31     BsmtQual  factor            37           97.47   -      -      -    -
#   32     BsmtCond  factor            37           97.47   -      -      -    -
#   33 BsmtExposure  factor            38           97.40   -      -      -    -
#   34 BsmtFinType1  factor            37           97.47   -      -      -    -
#   36 BsmtFinType2  factor            38           97.40   -      -      -    -
#   43   Electrical  factor             1           99.93   -      -      -    -
#   58  FireplaceQu  factor           690           52.74   -      -      -    -
#   59   GarageType  factor            81           94.45   -      -      -    -
#   60  GarageYrBlt  factor            81           94.45   -      -      -    -
#   61 GarageFinish  factor            81           94.45   -      -      -    -
#   64   GarageQual  factor            81           94.45   -      -      -    -
#   65   GarageCond  factor            81           94.45   -      -      -    -
#   73       PoolQC  factor          1453            0.48   -      -      -    -
#   74        Fence  factor          1179           19.25   -      -      -    -
#   75  MiscFeature  factor          1406            3.70   -      -      -    -

################################################################################
## Q7
################################################################################
# Coerce the KitchenQual column in tr to an ordered factor.

tr$KitchenQual <- factor(x=tr$KitchenQual, ordered = T)

#levels(tr$KitchenQual) #[1] "Ex" "Fa" "Gd" "TA" #orders alphabetically since no manual order is given

################################################################################
## Q8
################################################################################
# Delete the features from you tr data.frame that have more than 50% of their 
# values missing, as well as the id column

source("DataQualityReport.R")
BadValues<-DataQualityReport(tr)[DataQualityReport(tr)$PercentComplete<50,1:8]
BadValues <- BadValues$Attributes
library(tidyverse)
tr <- tr %>% select(!BadValues)
tr <- tr %>% select(!Id)
#Reference https://dplyr.tidyverse.org/reference/select.html

################################################################################
## Q9
################################################################################
# Create a high quality looking histogram of of the SalesPrice column in tr using
# at least 5 function arguments using base graphics. Then create another histogram
# that uses the ggplot2 library. Describe the distribution of this variable in 
# words with supporting statistics.

#reference: https://stackoverflow.com/questions/65977474/strange-x-axis-in-histogram-for-discrete-variable-in-r

cap<-max(tr$SalePrice)+50000
vlabels=seq(from=0, to=cap, by=50000)


hist(tr$SalePrice, col="gold", main="Distribution of Sales Prices", 
     xlab="Sales Price", border="gold",freq=TRUE, breaks=vlabels, xaxt="n")
axis(1, at=vlabels, labels = c(vlabels)) 

library(ggplot2)
 p <- ggplot(tr, aes(x=SalePrice)) + geom_histogram(color="gold", fill="gold") 
 p <- p + geom_vline(aes(xintercept=mean(SalePrice)), color="black", linetype="dashed", size=1)
 p <- p + geom_vline(aes(xintercept=median(SalePrice)), color="white", linetype="dashed", size=1) + scale_x_continuous(labels = scales::comma)
 p

 # The histogram depicts a right (also called positively) skewed dataset, indicating that the 
 # mean (black dashed line) is greater than the median (white dashed line)
 
################################################################################
## Q10
################################################################################
# Use the table() function to describe the KitchenQual variable in the tr dataset

table(tr$KitchenQual)

 # Ex  Fa  Gd  TA 
 # 100  39 586 735 

 # just over 50% of rows have TA level kitchen quality factor. GD is the second most common factor while EX and FA are significantly lower. 
   
################################################################################
## Q11
################################################################################
# Using code, prove that R views KitchenQual column in tr as an ordered factor.
# Show this at least two different ways.

class(tr$KitchenQual)
#[1] "ordered" "factor" 

 
 ################################################################################
## Q12
################################################################################
# Create a new data.frame called 'tr2' from the 'tr' dataset where FireplaceQu
# is not null. You must use brackets and the is.na() function in your answer.
 #reference https://rpubs.com/tomhopper/brackets
 tr2 <- tr[!is.na(tr$FireplaceQu),]
 

################################################################################
## Q13
################################################################################
# Use the any() and is.na() functions together to identify if there are any missing
# values in the tr2 data.frame for the LotFrontage column.

 any(is.na(tr2$LotFrontage))
 
 #[1] TRUE
 
 
################################################################################
## Q14
################################################################################
# Create a new data.frame called 'tr3' from the 'tr' dataset where FireplaceQu
# is not null. You must use subset() function in your answer.
#reference: https://www.statmethods.net/management/subset.html
 tr3<- subset(tr, !is.na(tr$FireplaceQu))
 #any(is.na(tr3$FireplaceQu)) # [1] FALSE
################################################################################
## Q15
################################################################################
# Overwrite 'tr3' from the 'tr3' dataset by subsetting the data such that LotConfig
# is equal to FR2 and Condition1 is not equal to Feedr. Use
# brackets in as part of your answer. Now do this again but use the subset() function.

 #reference: https://rpubs.com/tomhopper/brackets
 tr3 <- tr3[which(tr3$LotConfig=="FR2" & tr3$Condition1 != "Feedr"),]

  #now contains 18 objects of 76 variables
 
 tr3<- subset(tr3, tr3$LotConfig=="FR2" & tr3$Condition1 != "Feedr")
 #now contains 18 objects of 76 variables

################################################################################
## Q16
################################################################################
# Plot SalePrice versus LotArea for the tr3 data.frame using base graphics. Then
# use the abline() function to add a linear line of this relationship onto the 
# plot. Make the line red in color. Now create the plot again using the ggplot2
# library.

 # base graphics
 plot(tr3$LotArea, tr3$SalePrice, xlab = "Lot Area", ylab = "Sale Price", main = "Sale Price vs Lot Area")
 abline(lm(SalePrice ~ LotArea, data = tr3), col = "red")
 
 #ggplot2
 library(ggplot2)
 ggplot(data = tr3, aes(x = LotArea, y = SalePrice)) +
   geom_point() +
   geom_smooth(method = "lm", color = "red") +
   labs(x = "Lot Area", y = "Sale Price", title = "Sale Price vs Lot Area") + scale_y_continuous(labels = scales::comma)
 
 
################################################################################
## Q17
################################################################################
# Using the par() function, show two plots together. For the first plot show
# the SalePrice column versus any categorical variable you want. In the second
# plot show a boxplot of any numeric variable you want. Use the tr data.frame.
# Lastly, reset your graphics so only one plot shows up in the future.
# Discuss what your plots show.

 
 cap<-max(tr$SalePrice)+50000
 vlabels=seq(from=0, to=cap, by=50000)
 
 
 hist(tr$SalePrice, col="gold", main="Distribution of Sales Prices", 
      xlab="Sales Price", border="gold",freq=TRUE, breaks=vlabels, xaxt="n")

 
 
par(mfrow=c(1,2))
plot(SalePrice ~ YearBuilt, data=tr, xlab="Year Built", ylab="Sale Price", Main="Sale Price Vs. Year Built")

boxplot(tr$GarageArea, xlab = "", ylab = "Garage Area", main = "BoxPlot of Garage Area")
par(mfrow=c(1,1))
# the plot of salesprice vs year shows a gradual increase in sales price rises with year built, meaning that, on average, newer homes sell for more money.
# the box plot indicates that most homes have a garages with areas between 400 and 600 square feet, with the median somewhere around 500 square feet.
# it also indicates a small number of homes with garages over 1000 square feet in area.

################################################################################
## Q18
################################################################################
# Create a sequence of numbers that increments by 10,000 but starts at the 
# smallest SalePrice value in the tr data.frame and goes to the largest
# value of SalePrice in the tr data.frame.

spSeq <- seq(from = min(tr$SalePrice), to = max(tr$SalePrice), by = 10000)
#spSeq
# 
# [1]  34900  44900  54900  64900  74900  84900  94900 104900 114900 124900 134900 144900 154900 164900 174900 184900 194900
# [18] 204900 214900 224900 234900 244900 254900 264900 274900 284900 294900 304900 314900 324900 334900 344900 354900 364900
# [35] 374900 384900 394900 404900 414900 424900 434900 444900 454900 464900 474900 484900 494900 504900 514900 524900 534900
# [52] 544900 554900 564900 574900 584900 594900 604900 614900 624900 634900 644900 654900 664900 674900 684900 694900 704900
# [69] 714900 724900 734900 744900 754900

################################################################################
## Q19
################################################################################
# Create a new data.frame called FireplaceQu that takes each factor level and 
# creates a new dummy variable column from the tr2 data.frame. Next, modify the 
# names of these columns so they match the factor level names. Add this data.frame 
# to the tr2 data.frame. Then delete the orginal FireplaceQu column from the 
# tr2 data.frame.

#referece: https://www.listendata.com/2015/08/create-dummy-columns-from-categorical.html

#dummies
FireplaceQu <- as.data.frame(model.matrix(~ FireplaceQu - 1, data = tr2))

#replace the column names
colnames(FireplaceQu) <- levels(tr2$FireplaceQu)

#add columns to tr2
tr2 <- cbind(tr2, FireplaceQu)

#delete the old column
tr2 <- tr2[, !names(tr2) %in% "FireplaceQu"]

################################################################################
## Q20
################################################################################
# Coerce the tr data.frame to a data.table called tr_dt. Use the appropriate
# function to identify which R objects are data.tables. Next, set a key on the
# Neighborhood column for all your data.tables. Lastly, create a new data.table
# called tr_dt2 from tr_dt using your key and having a value of Blueste.
# install.packages("data.table")
library(data.table)

#coerce
tr_dt <- as.data.table(tr)

#query for all tables
tables()

#loop through all tables setting the neighborhood key
#for(mytable in tables()){
# setkey(get(mytable), "Neighborhood") 
#}

setkey(tr_dt, Neighborhood)
tr_dt2 <-tr_dt[ J("Blueste")]


#copy tr_dt into tr_dt2 using the key
tr_dt2 <- tr_dt[.(Neighborhood = "Blueste")]
tr_dt2
