################################################################################
# Week 1 - Day 2 Lab
#
# Instructions: You will perform this lab using RStudio Server and NOT your
#               personal RStudio install. This is because your personal install
#               will not be able to access our database server to pull in the 
#               dataset you'll need for some of the questions.
#
#               To access RStudio Server go to: https://www.rcac.purdue.edu/compute/scholar
#
#               Once you login, you can open the R script in RStudio on your personal
#               machine and then copy it into RStudio Server.
#
#               After you have completed all the questions, save your R script
#               and upload it to Brightspace. Thanks!
################################################################################
# Option 1: Using the RMariaDB library
# This is the preferred option to interface with the Maria database version of MySQL
#install.packages("odbc")
#install.packages("RMariaDB")
library(RMariaDB)
# Connect to a MariaDB version of a MySQL database
con <- dbConnect(RMariaDB::MariaDB(), host="datamine.rcac.purdue.edu", port=3306
                 , dbname="ap", user="gen_user", password="gen_user")
# list of db tables
dbListTables(con)
# query data
d <- dbGetQuery(con, "select * from ap.invoices")
# close db connection
suppressWarnings(dbDisconnect(con)); rm(con)

# Option 2: Using the RJDBC library
#library(RJDBC)
# Remember, you need to change 'lanhamm' to your Purdue username in the path below
#drv <- JDBC(driverClass="com.mysql.jdbc.Driver"
#            ,classPath="/home/lanhamm/mysql-connector-java-5.1.47.jar")
#conn <- dbConnect(drv, url="jdbc:mysql://datamine.rcac.purdue.edu:3306/ap"
#                  , user="gen_user", password="gen_user")
# query table
#d <- dbGetQuery(conn, "select * from vendors")

################################################################################
# square three times four
(3*4)^2

# create a new variable called x that is blank
x = NA
x <- NA

# create a numeric vector called y that contains the numbers 2 thru 2931.
y <- as.numeric(c(2:2931))

is.numeric(y)
is.factor(y)
str(y)
class(y)
class(d)

# create a numeric vector called z that is a sequence starting from 1 thru 1000 that increments by 3.
z <- seq(from=1, to=1000, by=3)

# create a character vector called 'myBuds' that contains names 'Cavin', 'Judy', and 'Kai Wei'. 'Cavin should be repeated 5 fives. Also have the output print when we do the assignment.
(myBuds <- c(rep('Cavin',5), 'Judy','Kai Wei'))
myBuds

(myBuds <- c(rep('Calvin,5'), 'Judy', 'Kai Wei'))

# how many factor levels does the myBuds character vector have?
as.factor(myBuds)

# coerce the myBuds vector to a factor vector by overwritting it.
myBuds <- as.factor(myBuds)

# write a statement that would tell you how many total characters the myBuds vector has. Hint: work from the inside out.
#vector has. Hint:work from the inside out
sum(nchar(as.character(myBuds)))

# create a Date vector called 'today' that has today's date. If this date was converted to a number in R, what would it be?
today <- as.numeric(Sys.Date())
Sys.Date()
#The number of days since January 1970 

# create a logical vector called 'a' that has 34 TRUE values and 300 FALSE values.
a <- as.logical(c(rep('True',34),rep('FAlSE',300)))

#Treat logic vector like 1,0 (Can do arthmatic Example)
a*32

# write a statement that tests if the length of vector 'a' is the same as vector 'z'.
length(a)
length(z)

length(a) == length(z)

# What is the sum of vector a multiplied by vector z?
sum(a*z)

# What would happen if you added vector y and vector z? Would it work, why or why not?
y + z
## answer (In y + z : longer object is not multiple of shorter object length)

# use the any() function to see if vector z contains the value 100.
any(z==100)

# set variable z to NULL. Did this delete variable z?
z = NULL
rm(z)

# what statement would delete variable z from your environment
rm(z)

# what data structure type is 'd'?
class(d)

# how could you find the names of all the columns in your dataset?
str(d)

# if 'd' was a matrix type, would names(d) provide you the correct column names?
colnames(d)

# what statement would give you the dimensionality of 'd'?
dim(d)

# what statement would give you the number of columns in 'd'?
ncol(d)

# what statement would provide you the data structure type, column vector types, and dimensionality all in one?
str(d)

# print the invoice_number column.
d$invoice_number

# print the first three rows of your dataset 'd'.
#d[rows,columns]

d[1:3,]

# select rows 1 thru 10 for columns vendor_id and invoice_number.
d[c(1:10), c("vendor_id", "invoice_number")]

d[c(1:10), c(2:3)]

# what 'is' function asks if the 'terms_id' column is an integer vector?
is.integer(d$terms_id)

# coerce the terms_id column to a numeric vector type.
d$terms_id = as.numeric(d$terms_id)

# what 'is' function asks if the 'terms_id' column is a numeric vector?
d$terms_id <- is.numeric(as.character(d$terms_id))


# create a new data.frame called 'd2' that has columns 'vendor_id', 'invoice_total',
# and payment_total' columns in it from 'd'.
d2 <- d[ , c("vendor_id", "invoice_total", "payment_total")]
d2

# coerce the 'vendor_id' column in the d2 data.frame to a factor vector.
as.factor(d2$vendor_id)

# In the d2 data.frame, change the payment_total and invoice_total values in the second row to 400.20 instead of 40.20.
d2[2,c("payment_total", "invoice_total")] = 400.20

# create a vector called 'dummies' using the model.matrix function where the columns are dummy variables for 'vendor_id' in 'd2'.
d2$vendor_id <- as.factor(d2$vendor_id)
dummies <- model.matrix(~ vendor_id - 1, data = d2)
dummies

# create a new data.frame called 'd3' that contains 'd2' and 'dummies'
d3 <- cbind(d2, dummies)

# create a list called 'recs' that contains vendor_id vector from 'd3' in the first element and vector 'a' in the second element
recs <- list(vendor_id = d3$vendor_id, a = a)
recs

# assign names to the two elements in your 'recs' list
names(recs) <- c("Vendor_IDs", "Vector_A")
print(recs)

# access the last value in your first element of your 'recs' list.
last_value <- tail(recs$Vendor_IDs, n = 1)

# create an empty data.frame called 'results' that has 100 rows and 4 columns
results <- as.data.frame(matrix(NA, nrow = 100, ncol = 4))
results

# set the column names for 'results' to the following: model, vars, accuracy, runtime.
str(results)
results = c("model", "vars", "accuracy", "runtime")
results

# load the library data.table (first install it if its not installed)
install.packages("data.table")
library(data.table)

# coerce data.frame 'd' to a data.table called 'd'
data.table(d)

# use the str() to confirm 'd' is a data.table
str(d)

# set a data.table key on the column invoice_id in 'd'. Then show that there is indeed a key on that field.
setkey(d,invoice_id)
tables()

# remove all the objects in your environment
rm(list = ls())
