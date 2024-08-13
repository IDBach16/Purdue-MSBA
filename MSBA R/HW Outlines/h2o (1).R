################################################################################
# UR4A Class: H2O Example
#
################################################################################
# load package
library(h2o)

# start your h2o cluster
# By default, your h2o instance will be allowed to use all your cores and (typically)
# 25% of your system memory unless you specify otherwise.
#h2o.init()

# If you want to specify cores and memory to use you can like so:
h2o.init(nthreads=1, max_mem_size="4g")

# If you're working on RStudio Server (Scholar), you can specify your port as
# as well to match your H20 Flow url. To see the flow using Scholar, you have to
# use the RStudio within Remote Desktop and use the browser
#h2o.init(ip='localhost', port=54321, nthreads=4, max_mem_size='4g')

# Your H20 flow if you're running this on your local RStudio installation here: 
#       http://127.0.0.1:54321/flow/index.html
# If on RStudio Server, Flow link here: 
#       http://127.0.0.1:54321/flow/index.html

################################################################################
# The data set we want to investigate is online
datasets <- "https://raw.githubusercontent.com/DarrenCook/h2o/bk/datasets/"

# creates an H2O frame on the cluster which is like a data.frame in R. 
# It recognizes that the first line in the csv file was a header row, so it has 
# automatically named the columns. Also, recognizes that the class column is 
# categorical.
data <- h2o.importFile(paste0(datasets,"iris_wheader.csv"))
head(data)

# prepare the data
y <- "class"                                # target variable to learn
x <- setdiff(names(data), y)                # feature variables are all other columns
parts <- h2o.splitFrame(data, 0.8, seed=99) # randomly partition data into 80/20
train <- parts[[1]]                         # random set of training obs
test <- parts[[2]]                          # random set of testing obs

################################################################################
# train a model
################################################################################
# (Option 1: Default settings) train the model
m <- h2o.deeplearning(x, y, train)

# (Option 2: Custom settings) train the model (will only use one core)
#m <- h2o.deeplearning(x, y, train, seed=99, reproducible=T)

# Here are some other models you might try
#h2o.randomForest    # random forest
#h2o.gbm             # gradient boosting machine
#h2o.glm             # generalized linear model (logit, multinomial logit)
#h2o.naiveBayes      # naive Bayes
#h2o.stackedEnsemble # ensemble model

# You can see all the customization that is allowed for each algorithm.
?h2o.randomForest

################################################################################
# examining the performance of the trained model
################################################################################
h2o.mse(m)               # appropriate for regression-type problems
h2o.confusionMatrix(m)   # appropriate for classification-type problems
# show h2o. to find other functions you might use

# make predictions
p <- h2o.predict(m, test)
# calling p will only show the first six predictions
# 1st column is predicted class
# remaining columns are are confidence/probabilities (each row should sum to 1)
p
# to see all predictions, you have to actually download them (be careful if
# data is large). 
as.data.frame(p)

################################################################################
# obtain predictions versus actuals in test set
################################################################################
# OPTION A: Work in cloud cluster
# this way combines the records in the cluster to make a new data frame in the 
# cluster this does not download the data to your machine
as.data.frame(h2o.cbind(p$predict, test$class))

# OPTION B: Download results to local machine and work there
# column from p is downloaded, column from test is downloaded, then combined in
# R's memory to make a data.frame. First option above usually better because
# you are not actually downloading the data to your machine
cbind(as.data.frame(p$predict), as.data.frame(test$class))

################################################################################
# classification accuracy on test set
mean(p$predict == test$class)

# stats on test set
# the hit ratio section tells us that the model was 
#   93.3% right when picking one class
#   100%  right if it could pick the top two classes
#   100%  right if it could pick the top three classes (obviously this will 
#         always be 100% in this case as there are only 3 classes in total)
h2o.performance(m, test)

# while caret was designed for binary classification and regression problems
# the confusionMatrix() function can also calculate stats for multiple classes
results <- cbind(as.data.frame(test$class),as.data.frame(p))
head(results)
library(caret)
(confusionMatrix(data=results$class, results$predict))

################################################################################
# AutoML
################################################################################
# learn about various setting options using AutoML
?h2o.automl

# train models under some specified conditions
aml <- h2o.automl(x, y, train
                   , max_runtime_secs = 180     # max time to run in seconds
                   , max_models = 3            # max num of models
                   , seed = 123                # for reproducibility.
                   )

# In h2o flow, go to Admin -> Jobs. Then click on a 'Running' model. It will
# provide you an expected completion time for that model which is really useful
aml <- h2o.automl(x, y, train
                   #, max_runtime_secs = 180     # max time to run in seconds
                   , max_models = 10            # max num of models
                   , seed = 123                # for reproducibility.
)

# View the AutoML Leaderboard
lb <- aml@leaderboard
print(lb, n = nrow(lb))  # Print all rows instead of default (6 rows)

# Get leaderboard with 'extra_columns = 'ALL'
lb <- h2o.get_leaderboard(object = aml, extra_columns = 'ALL')
lb

# The leader model is stored here
aml@leader

# To generate predictions on a test set, you can make predictions
# directly on the `"H2OAutoML"` object or on the leader model
# object directly
pred <- h2o.predict(aml, test)  # predict(aml, test) also works
# or:
#pred <- h2o.predict(aml@leader, test)

################################################################################
# Saving, Loading, Downloading, and Uploading Models
################################################################################

# save the model. here it's saved to your working directory unless you specify
# otherwise. You can save the model locally or in HDFS.
model_path <- h2o.saveModel(object = aml@leader
                            , path = getwd()
                            , force = TRUE  #indicates how to deal with files that already exist
                            )
print(model_path)

# load the model
saved_model <- h2o.loadModel(model_path)

# download the model built above to your local machine
my_local_model <- h2o.download_model(model = aml@leader
                                     , path = "C:\\Users\\Matthew A. Lanham\\Dropbox\\_Purdue\\_Teaching\\UR4A\\10c_Predictive Modeling with h2o")

# upload the model that you just downloaded above to the H2O cluster
uploaded_model <- h2o.upload_model(my_local_model)


################################################################################
# shutdown your cluster
h2o.shutdown()

