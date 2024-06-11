#install.packages("readxl")

library(readxl)

tr$...1 = NULL


################################################################################
# MGMT 590 Using R for Analytics
# Team Assignment #2
# Due: 09/22/2020 11:59pm
#
# Are you in the 2:50-4:20 or 4:30-6:00pm section: ______Online___________________
#
# Team #:_________Group 2______________________________________________
#
#
# Submission instructions: This assignment must be completed with your assigned 
# team with no help from others. Only one person from your team will submit your 
# team's solutions to Brightspace on time. It is due by Tuesday, September 22nd, 
# 2020 by Midnight. Late submissions for any reason will be subject to a 25% 
# late penalty per day past due. No Brightspace, Scholar, or other excuses.
#
# While there is only one team submission, everyone should try to answer the 
# questions on their own and when in doubt discuss with your teammates. This 
# is the only way you will truly learn the R language. Assigning people to do 
# certain questions of the homework is a bad strategy to prepare you for the 
# final exam. 
# 
# Please put all your answers in the hw2_main.R script (except Q15). This script
# should clearly show which question you are answering and have comments where you 
# need to comment to answer the question provided. Some questions will ask you 
# to provide output. In those cases, copy and paste the output into your R script
# and then comment it out. For Q15, upload your app.R script with the corresponding
# custom function you called. In total, you should have three files that you have
# uploaded.
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
# server and can be accessed using the lines 42 thru 49 below.
#
# set memory limits
options(java.parameters = "-Xmx64048m") # 64048 is 64 GB
# connect to db, pull in table, and then close connection.
# install.packages("odbc")
# install.packages("RMariaDB", configure.vars=c("INCLUDE_DIR=/usr/include/mysql LIB_DIR=/usr/lib64/mysql"))
# 
# library(RMariaDB)
# # Connect to a MariaDB version of a MySQL database
# # Do not change the user id or password; Leave it as gen_user
# con <- dbConnect(RMariaDB::MariaDB(), host="datamine.rcac.purdue.edu", port=3306
#                  , dbname="khou", user="gen_user", password="gen_user")
# 
# # pull in the training data into a data.frame called tr
# # it 'tr' in R
# tr <- dbGetQuery(con, "select * from train")
# # disconnect from db
# dbDisconnect(con)

################################################################################
## Q1 (1 point)
################################################################################
# One thing you might notice is that some of the columns show NA implying that
# there are missing values. However, this is misleading as in the database they
# were stored as literal NA values instead of missing values. Some of the values
# indeed should be missing.

# Write a for loop for these features where 'NA' is really a missing value, 
# and make them blank (i.e. really NA)
# LotFrontage, MasVnrType, MasVnrArea, Electrical, GarageYrBlt
realBlanks <- c("LotFrontage", "MasVnrType", "MasVnrArea", "Electrical"
                , "GarageYrBlt")
# loop through each field in realBlanks and make 'NA' values missing values:

for (field in realBlanks) {
  tr[[field]][tr[[field]] == 'NA'] <- NA
}


# Features where 'NA' is a meaningful factor level, we will keep them as is. These are:
# Alley, BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinType2, FireplaceQu,
# GarageType, GarageFinish, GarageQual, GarageCond, PoolQC, Fence, MiscFeature

################################################################################
## Q2 (1 point)
################################################################################
# You read about the dataset by reviewing the data dictionary for this data located
# here: https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data
# Based on your understanding of data, you coerced every variable in the 'tr' dataset
# to the correct R data type it should be for proper data analysis. Last assignment
# this required using several repetitive 'as.' functions to coerce each column
# to the appropriate type. However you might do this using a for loop.
str(tr)
# coerce these fields to numeric vectors
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

# Define the fields to be coerced to factor type
factorFields <- c("MSSubClass", "MSZoning", "Street", "Alley", "LotShape", "LandContour", 
                  "Utilities", "LotConfig", "LandSlope", "Neighborhood", "Condition1", 
                  "Condition2", "BldgType", "HouseStyle", "RoofStyle", "RoofMatl", 
                  "Exterior1st", "Exterior2nd", "MasVnrType", "ExterQual", "ExterCond", 
                  "Foundation", "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", 
                  "BsmtFinType2", "Heating", "HeatingQC", "CentralAir", "Electrical", 
                  "KitchenQual", "Functional", "FireplaceQu", "GarageType", "GarageFinish", 
                  "GarageQual", "GarageCond", "PavedDrive", "PoolQC", "Fence", "MiscFeature", 
                  "SaleType", "SaleCondition")

# Loop through each field in factorFields and coerce them to factor type
for (field in factorFields) {
  tr[[field]] <- as.factor(tr[[field]])
}


################################################################################
## Q3 (1 point)
################################################################################
# Here we use the DataQualityReport.R function to show the percent of complete
# records by variable. We added brackets at the end of the function call to subset
# the result set to return only those records where NumberMissing>0. 
#setwd("/home/lanhamm")
source("DataQualityReport.R")
DataQualityReport(tr)[DataQualityReport(tr)$NumberMissing>0,1:8]
#    Attributes    Type NumberMissing PercentComplete  Min     Avg Median  Max
#4  LotFrontage numeric           259           82.26   21   70.05     69  313
#26  MasVnrType  factor             8           99.45    -       -      -    -
#27  MasVnrArea numeric             8           99.45    0  103.69      0 1600
#43  Electrical  factor             1           99.93    -       -      -    -
#60 GarageYrBlt numeric            81           94.45 1900 1978.51   1980 2010

# The GarageYrBlt feature has 81 missing values. The issue is this feature
# has missing values because these homes do not have garages. Prove this by using 
# the sum() and ifelse() function together. Also, provide the output of your
# code commented out in your R script:

#To prove the number of NA garageyrblt values (81) due to the homes not having garages,
#Lets count all of the houses with garages that hold less than 1 car (ie, there is no garage)
num_homes_without_garage <- sum(ifelse(tr$GarageCars<1, 1, 0))

num_homes_without_garage

#> num_homes_without_garage <- sum(ifelse(tr$GarageCars<1, 1, 0))
#> 
#> num_homes_without_garage
#[1] 81
#> 


#Since the number of homes with no garage (81) is equal to the number of missing 
#garageyrblt values, we can be sure the NA values are due to there being no garage 
#for those homes.

################################################################################
## Q4 (4 points)
################################################################################
# For the missing GarageYrBlt values, replace those with the YearBuilt value. 
# Show how to do this 4 different ways:
# 1) for() loop, 2) ifelse(), 3) an apply family function, and 4) dplyr library

#1: For Loop
for (i in 1:nrow(tr)) {
  if (is.na( tr$GarageYrBlt[i])) {
    tr$GarageYrBlt[i] = tr$YearBuilt[i]
  }
  
}

#2: ifelse()
tr$GarageYrBlt <- ifelse(is.na(tr$GarageYrBlt), tr$YearBuilt, tr$GarageYrBlt)

#3 apply family function
tr$GarageYrBlt <- sapply(1:nrow(tr), function(i) {
  if (is.na(tr$GarageYrBlt[i])) {
    return(tr$YearBuilt[i])
  } else {
    return(tr$GarageYrBlt[i])
  }
})
#4 Dyplr -Note that yearbuilt is an integer, but GarageYrBlt is a numeric, 
#so we will need to convert inline to use dyplr with as.numeric or it will throw
#an error due to the type mismatch.
library(dplyr)
tr <- tr %>%
  mutate(GarageYrBlt = if_else(is.na(GarageYrBlt), as.numeric(YearBuilt), GarageYrBlt))


################################################################################
## Q5 (2 points)
################################################################################
# MasVnrType and MasVnrArea both have missing values. Tweak your answers 
# in Q2 and Q3 to show that 1) MasVnrType NA values and MasVnrArea NA values
# correspond to the same rows, and 2) assume that the MasVnrType missing values
# should be adjusted to 'None' and MasVnrArea values to 0. The MasVnrType should
# still be a factor vector with the same factor levels as before.

#sum up all the cases where each variable is null, then sum up all the cases where
#both variables are null.

MasVnrType_NA= sum(ifelse(is.na(tr$MasVnrType), 1, 0))
MasVnrArea_NA= sum(ifelse(is.na(tr$MasVnrArea), 1, 0))
both_NA= sum(ifelse((is.na(tr$MasVnrType)&is.na(tr$MasVnrArea)), 1, 0))

#if both individual variable sums are equal to the combined variable sum, then we know 
#all the rows correspond to cases where both are nul, return true.
#Oherwise, there are cases where only one is null, return false

ifelse((both_NA==MasVnrArea_NA & both_NA==MasVnrType_NA), TRUE, FALSE)
#output:
# > MasVnrType_NA= sum(ifelse(is.na(tr$MasVnrType), 1, 0))
# > MasVnrArea_NA= sum(ifelse(is.na(tr$MasVnrArea), 1, 0))
# > both_NA= sum(ifelse((is.na(tr$MasVnrType)&is.na(tr$MasVnrArea)), 1, 0))
# > 
#   > ifelse((both_NA==MasVnrArea_NA & both_NA==MasVnrType_NA), TRUE, FALSE)
# [1] TRUE
# >


#use dypler to easily change the null values, use the as. functions to 
#maintain, convert, and restore types as needed
tr <- tr %>%
  mutate(MasVnrType = as.character(MasVnrType)) %>%
  mutate(MasVnrType = if_else(is.na(MasVnrType), 'None', MasVnrType)) %>%
  mutate(MasVnrType = as.factor(MasVnrType)) %>%
  mutate(MasVnrArea = if_else(is.na(MasVnrArea), as.numeric(0), MasVnrArea))


#Check our work by updating the sum variables, then verifying they are all 0
MasVnrType_NA= sum(ifelse(is.na(tr$MasVnrType), 1, 0))
MasVnrArea_NA= sum(ifelse(is.na(tr$MasVnrArea), 1, 0))
both_NA= sum(ifelse((is.na(tr$MasVnrType)&is.na(tr$MasVnrArea)), 1, 0))

#if all variables are 0, we're done, return true; If not, return false
ifelse((both_NA==0 & MasVnrType_NA == 0 &MasVnrArea_NA == 0), TRUE, FALSE)

#output:
# > tr <- tr %>%
#   +   mutate(MasVnrType = as.character(MasVnrType)) %>%
#   +   mutate(MasVnrType = if_else(is.na(MasVnrType), 'None', MasVnrType)) %>%
#   +   mutate(MasVnrType = as.factor(MasVnrType)) %>%
#   +   mutate(MasVnrArea = if_else(is.na(MasVnrArea), as.numeric(0), MasVnrArea))
# > #Check our work by updating the sum variables, then verifying they are all 0
#   > MasVnrType_NA= sum(ifelse(is.na(tr$MasVnrType), 1, 0))
# > MasVnrArea_NA= sum(ifelse(is.na(tr$MasVnrArea), 1, 0))
# > both_NA= sum(ifelse((is.na(tr$MasVnrType)&is.na(tr$MasVnrArea)), 1, 0))
# > 
#   > #if all variables are 0, we're done, return true; If not, return false
#   > ifelse((both_NA==0 & MasVnrType_NA == 0 &MasVnrArea_NA == 0), TRUE, FALSE)
# [1] TRUE

################################################################################
## Q6 (1 point)
################################################################################
# Delete the row where Electrical values are missing


#use filter to replace tr with version of tr containing no NA Electrical values
library(dplyr)

tr <- tr %>%
  filter(!is.na(Electrical))

#lets check our work:

source("DataQualityReport.R")
DataQualityReport(tr)[DataQualityReport(tr)$NumberMissing>0,1:8]

# Attributes    Type NumberMissing PercentComplete Min   Avg Median Max
# 4 LotFrontage numeric           259           82.25  21 70.05     69 313

#As we can see from the comments, no missing values that we've fixed so far remain.

################################################################################
## Q7 (1 point)
################################################################################
# The LotFrontage feature has several missing values. Based on other features
# there might be an intelligent way to fill in these values. Create a ggplot
# object that overlays a histogram of LotArea for observations where LotFrontage
# is missing, and a histogram of LotArea for observations where LotFrontage is
# NOT missing. Make the x-axis range from 0 to 50000. Does it appear the 
# distributions are similar among these two groups of data? If so, we might
# try to impute the LotFrontage values later using a predictive model using the
# LotArea values.
# Example of what I'm looking for:
# https://www.dropbox.com/s/6onpl908nifcf75/Q6_histogram_example.png?dl=0



library(ggplot2)

# Filter observations where LotFrontage is missing and not missing
missing_lotfrontage <- tr[is.na(tr$LotFrontage), ]
not_missing_lotfrontage <- tr[!is.na(tr$LotFrontage), ]

# Create ggplot object
plot <- ggplot() +
  # Histogram for observations where LotFrontage is missing
  geom_histogram(data = missing_lotfrontage, aes(x = LotArea), fill = "blue", alpha = 0.5) +
  # Histogram for observations where LotFrontage is not missing
  geom_histogram(data = not_missing_lotfrontage, aes(x = LotArea), fill = "purple", alpha = 0.5) +
  # Set x-axis range from 0 to 50000
  xlim(0, 50000) +
  # Add labels and title
  labs(x = "LotArea", y = "Frequency", title = "Distribution for LotArea for LotFrontage Missing Values") +
  # Add legend
  scale_fill_manual(name = "LotFrontage", values = c("blue" = "Missing", "purple" = "Not Missing"))

# Print the plot
print(plot)

#the distributions are similar, though at a different scale and 
#still not mirror copies of one another we could build a linear 
#regression using lm(), then use predict() to try to fill the missing values. 
#We could use the summary statistics in the intro to linear regression data camp 
#examples to assess the model afterward. 


################################################################################
## Q8 (1 point)
################################################################################
# Create a function called 'lotReview' that plots LotFrontage versus other 
# features using the ggplot2 library. If the feature is numeric the function
# should provide a scatter plot, otherwise it should provide a side-by-side
# boxplot.



lotReview <- function(dataToPlot, plotVar) {
  Gtitle = ""
  if (is.numeric(dataToPlot[[plotVar]])) {
    Gtitle = paste("Scatterplot of ", plotVar, " vs. LotFrontage")
    p <- ggplot(dataToPlot, aes_string(y = plotVar, x = "LotFrontage")) + 
      geom_point(color="black") 
    
  } else {
    p <- ggplot(dataToPlot, aes_string(x = plotVar, y = "LotFrontage")) + 
      geom_boxplot(fill="skyblue")
    Gtitle = paste("Boxplot of LotFrontage vs. ", plotVar)
  }
  p <- p + labs(title=Gtitle)
  return(p)
}


# Here are two example calls to check to see if your function is returning how I 
# am expecting:
lotReview(dataToPlot=tr, plotVar="LotArea")
# https://www.dropbox.com/s/hhpveuasdbd63no/numeric_scatter_example.png?dl=0A
lotReview(dataToPlot=tr, plotVar="Functional")
# https://www.dropbox.com/s/139gev2udevwzft/factor_scatter_example.png?dl=0

################################################################################
## Q9 (1 point)
################################################################################
# 1) remove the Id column in tr
# 2) Obtain 10% quantiles of SalePrice by using the quantile() function. Using 
# these values create a summary statistics data.frame called 'results' for the
# numeric features for each quantile level. To construct this table use a for loop
# or double for loop.
# An example of the expected output would look like this. Note that your numbers
# might be slightly different based on how you set thresholds (inclusive/exclusive).
# Don't worry about that:
# https://www.dropbox.com/s/lsf3iseu6q94h6t/Q8_results.png?dl=0

#start with removing ID
tr <- tr[, -which(names(tr) == "Id")]

#ref= https://www.stat.auckland.ac.nz/~ihaka/787/lectures-quantiles-handouts.pdf
# Create quantiles
quantiles <- quantile(tr$SalePrice, probs = seq(0, .9, by = 0.1))

#Create an empty results dataframe and give it column names
emptyMatrix = matrix(nrow=length(quantiles), ncol = 4)
results <- data.frame(emptyMatrix)
colnames(results) <-c("quantile_level", "mean_val", "median_val", "std_dev")


# Loop through each quantile to populate the results dataframe
for (i in 1:(length(quantiles))) {
  # Define quantile level
  

  quantile_level <- paste0(((i)*10),"% quantile")
  if(i*10 <100)
  {
  # Subset the data frame based on the current quantile
  tr_subset <- filter(tr, SalePrice > quantiles[i] & SalePrice < quantiles[i+1])
  # Calculate summary statistics for numeric features
  #summary_stats = matrix(ncol=4, nrow=1)
  results$quantile_level[i] <- quantile_level
  results$mean_val[i] <- mean(tr_subset$SalePrice,na.rm=TRUE)
  results$median_val[i] <- median(tr_subset$SalePrice, na.rm=TRUE)
  results$std_dev[i] <- sd(tr_subset$SalePrice, na.rm=TRUE)
  }else{
    #because you can't check for values over 100%, this one is 0 for all summary stats
    results$quantile_level[i] <- quantile_level
    results$mean_val[i]<-0
    results$median_val[i] <- 0
    results$std_dev[i] <- 0
    
  }
}

results

# quantile_level  mean_val median_val   std_dev
# 1    10% quantile  86288.17      88000 15400.482
# 2    20% quantile 114921.14     115000  4954.938
# 3    30% quantile 130045.82     130000  3328.520
# 4    40% quantile 141060.16     140200  2983.437
# 5    50% quantile 154910.13     155000  4351.815
# 6    60% quantile 171720.87     172500  4668.494
# 7    70% quantile 187710.34     187500  5602.950
# 8    80% quantile 213447.36     213500  9124.973
# 9    90% quantile 252248.54     250000 14061.580
# 10  100% quantile      0.00          0     0.000

################################################################################
## Q10 (1 point)
################################################################################
# In two lines of code or less use any of the apply family of functions to 
# calculate the column means for only numeric columns in tr


numeric_means <- sapply(tr[, sapply(tr, is.numeric)], mean, na.rm = TRUE)
numeric_means

################################################################################
## Q11 (1 point)
################################################################################
# Create a new column in the tr data.frame called 'SalePriceQ' that says 10 if the
# SalesPrice for that row falls within the 10% decile of sales price, says 20, if 
# the Sales Price for a row falls within the 20% decidle, etc. all the way up to 100.
# Then use the aggregate() function to obtain the mean value for each decile for
# only numeric column types.


tr$SalePriceQ <- cut(tr$SalePrice, 
                     breaks = quantile(tr$SalePrice, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
                     labels = seq(10, 100, by = 10),
                     include.lowest = TRUE)

table(tr$SalePriceQ)
numeric_data <- tr[sapply(tr, is.numeric)]
numeric_data$SalePriceQ <- tr$SalePriceQ  # Ensure the grouping variable is included
mean_values_by_decile <- aggregate(. ~ SalePriceQ, data = numeric_data, FUN = mean, na.action = na.omit)
print(mean_values_by_decile)


################################################################################
## Q12 (1 point)
################################################################################
# Using ddply from the plyr package obtain the average YearBuilt and average
# GarageCars by SalePriceQ. What can you infer about the relationship of these
# features?

library(plyr)

my_averages <- ddply(tr, .(SalePriceQ), summarise, 
                     AvgYearBuilt = mean(YearBuilt, na.rm=TRUE),
                     AvgGarageCars = mean(GarageCars, na.rm=TRUE)
)
my_averages


# 
# SalePriceQ AvgYearBuilt AvgGarageCars
# 1          10     1939.596     0.9452055
# 2          20     1946.852     1.2013423
# 3          30     1956.160     1.3680556
# 4          40     1961.573     1.4733333
# 5          50     1966.706     1.7272727
# 6          60     1978.909     1.9230769
# 7          70     1987.103     1.9863014
# 8          80     1989.926     2.0604027
# 9          90     1989.910     2.2638889
# 10        100     1996.372     2.7448276



################################################################################
## Q13 (1 point)
################################################################################
# Use the sqldf library to generate the same result as in previous question. 
# Name your columns the same way you did previously.


# Load the sqldf package

#install.packages('sqldf')
library(sqldf)

# Write an SQL query to calculate the averages by SalePriceQ
#ading +0 to SalePriceQ in the order by implicitly conerts it to an integer 
#to apply numeric sorting instead of alphabetical, otherwise 100 will go before
#20
results_sqldf <- sqldf("
  SELECT
    SalePriceQ,
    AVG(YearBuilt) as AvgYearBuilt,
    AVG(GarageCars) as AvgGarageCars
  FROM
    tr
  GROUP BY
    SalePriceQ
  ORDER By
    SalePriceQ + 0
")

# Print the results to view them
results_sqldf

# SalePriceQ AvgYearBuilt AvgGarageCars
# 1          10     1939.596     0.9452055
# 2          20     1946.852     1.2013423
# 3          30     1956.160     1.3680556
# 4          40     1961.573     1.4733333
# 5          50     1966.706     1.7272727
# 6          60     1978.909     1.9230769
# 7          70     1987.103     1.9863014
# 8          80     1989.926     2.0604027
# 9          90     1989.910     2.2638889
# 10        100     1996.372     2.7448276

################################################################################
## Q14 (1 point)
################################################################################
# Using the dplyr package and pipes (%>%), take the tr data.frame and create a 
# new column called 'PricePerArea' which is the SalePrice divided by LotArea. 
# Then select the columns SalePriceQ, PricePerArea, and Neighborhood. Then subset
# the data where PricePerArea is greater than 40. Then sort the data in ascending 
# order by SalePriceQ then PricePerArea. Save this resultset into a new data.frame
# called PricePerAreaStats

library(dplyr)

PricePerAreaStats <- tr %>%
  mutate(PricePerArea = SalePrice / LotArea) %>%  
  select(SalePriceQ, PricePerArea, Neighborhood) %>%  
  filter(PricePerArea > 40) %>%  
  arrange(SalePriceQ, PricePerArea)  

PricePerAreaStats


# SalePriceQ PricePerArea Neighborhood
# <fct>             <dbl> <fct>
# 1 10                 42.3 MeadowV
# 2 10                 42.5 BrDale
# 3 10                 42.9 MeadowV
# 4 10                 43.6 MeadowV
# 5 10                 50.6 MeadowV
# 6 10                 50.8 BrDale
# 7 10                 52.4 BrDale
# 8 10                 52.6 BrDale
# 9 10                 53.3 BrDale
# 10 10                 54.2 MeadowV

################################################################################
## Q15 (5 points)
################################################################################
# Create a shiny app for the tr dataset. You may use whatever input functions
# you believe to be most appropriate, but at a minimum 1) should allow the user
# to select a Neighborhood, filter in some fashion by SalePrice, as well as
# filter by TotRmsAbvGrd. 2) The output should provide one base graphic plot, a
# ggplot, and a table of filtered data or statistics. 3) In your code, you should
# include some conditional logic (e.g. if, if-else) and 4) call a custom function
#
#install.packages("shiny")
library(shiny)
library(ggplot2)
library(dplyr)
library(shinyjs)  # Load shinyjs package

# Load the dataset
data(tr)

# Define a custom function to calculate summary statistics
calculate_summary <- function(data) {
  summary_data <- summarise(data,
                            Mean_SalePrice = mean(SalePrice),
                            Mean_GarageCars = mean(GarageCars),
                            Mean_LotArea = mean(LotArea))
  return(summary_data)
}

# Define UI
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs

  titlePanel("Training Data Explorer"),

  sidebarLayout(
    sidebarPanel(
      selectInput("neighborhood", "Neighborhood:",
                  choices = unique(tr$Neighborhood)),
      sliderInput("saleprice", "Filter by Sale Price:",
                  min = min(tr$SalePrice), max = max(tr$SalePrice),
                  value = c(min(tr$SalePrice), max(tr$SalePrice))),
      sliderInput("totrooms", "Filter by Total Rooms Above Ground:",
                  min = min(tr$TotRmsAbvGrd), max = max(tr$TotRmsAbvGrd),
                  value = c(min(tr$TotRmsAbvGrd), max(tr$TotRmsAbvGrd))),
      checkboxInput("showSummary", "Show Summary", value = FALSE, width = NULL)
    ),

    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Base Plot", plotOutput("base_plot")),
                  tabPanel("ggplot", plotOutput("ggplot")),
                  tabPanel("Filtered Data", tableOutput("filtered_data"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  filtered_data <- reactive({
    tr %>%
      filter(Neighborhood == input$neighborhood,
             SalePrice >= input$saleprice[1] & SalePrice <= input$saleprice[2],
             TotRmsAbvGrd >= input$totrooms[1] & TotRmsAbvGrd <= input$totrooms[2])
  })

  output$base_plot <- renderPlot({
    plot(filtered_data()$SalePrice, filtered_data()$TotRmsAbvGrd,
         xlab = "Sale Price", ylab = "Total Rooms",
         main = "Base Plot")
  })

  output$ggplot <- renderPlot({
    ggplot(filtered_data(), aes(x = SalePrice, y = TotRmsAbvGrd)) +
      geom_point() +
      labs(x = "Sale Price", y = "Total Rooms", title = "ggplot")+
      scale_x_continuous(labels = scales::label_number()) +
      scale_y_continuous(labels = scales::label_number())
  })

  output$filtered_data <- renderTable({
    filtered_data()
  })

  observeEvent(input$showSummary, {
    if (input$showSummary) {
      appendTab(inputId = "tabs",
                tabPanel("Summary", tableOutput("summary")),
                select = TRUE)
      output$summary <- renderTable({
        calculate_summary(filtered_data())
      }, rownames = TRUE)
    } else {
      removeTab(inputId = "tabs", target = "Summary")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
