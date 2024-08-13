################################################################################
# Data Reshaping Lab
# Credit card default data 
################################################################################
myUrl <- "https://raw.githubusercontent.com/MatthewALanham/Datasets/master/creditCardDefaultData.csv"
ccd <- read.table(file=myUrl, header=T, sep=","
                  , colClasses=c("numeric","factor","numeric",rep("factor",3)
                                 ,rep("numeric",19)))
str(ccd)
names(ccd)

# Q1: Using the aggregate() function, obtain the average of the X5_AGE variable 
# by the Y (default) column


# Q2: Use an appropriate apply family function to obtain the average value for 
# every column in the last five columns


# Q3: Using the ddply() function from the plyr package, obtain the average of 
# the X5_AGE variable by the Y (default) column


# Q4: Using the sqldf() function from the sqldf package, obtain the average of 
# the X5_AGE variable by the Y (default) column



