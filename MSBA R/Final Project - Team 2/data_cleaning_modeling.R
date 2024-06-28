# data_cleaning_modeling.R

clean_and_model_data <- function(d) {
  # Check initial structure
  #print("Initial dataset structure:")
  #print(str(d))
  
  # Creating Dummy Variables
  dummy <- dummyVars(y ~ ., data = d)
  ex <- data.frame(predict(dummy, newdata = d))
  names(ex) <- gsub("\\.", "", names(ex))
  d <- cbind(y = d$y, ex)
  
  # Check structure after creating dummy variables
  #print("Structure after creating dummy variables:")
  #print(str(d))
  
  # Identify and remove correlated predictors
  descrCor <- cor(d[, -1], use = "pairwise.complete.obs")
  highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.80)
  if (length(highlyCorDescr) > 0) {
    #print("Highly correlated predictors to remove:")
    #print(highlyCorDescr)
    d <- d[, -c(highlyCorDescr + 1)]
  }
  
  # Check structure after removing correlated predictors
  #print("Structure after removing correlated predictors:")
  #print(str(d))
  
  # Remove linear dependencies
  linear_combos <- findLinearCombos(d)
  if (!is.null(linear_combos$remove) && length(linear_combos$remove) > 0) {
    #print("Linear dependencies to remove:")
    #print(linear_combos$remove)
    d <- d[, -linear_combos$remove, drop = FALSE]
  }
  
  # Check structure after removing linear dependencies
  #print("Structure after removing linear dependencies:")
  #print(str(d))
  
  #Remove near-zero variance predictors
  nzv <- nearZeroVar(d, saveMetrics = TRUE)
  if (sum(nzv$zeroVar) > 0) {
    #print("Near-zero variance predictors to remove:")
    ##print(which(nzv$zeroVar))
    d <- d[, c(TRUE, !nzv$zeroVar)]
  }
  
  # Check structure after removing near-zero variance predictors
  ##print("Structure after removing near-zero variance predictors:")
  ##print(str(d))
  
  # Standardize numeric features using min-max normalization
  preProcValues <- preProcess(d[, -1], method = c("range"))
  d <- predict(preProcValues, newdata = d)
  
  # Check structure after standardizing numeric features
  ##print("Structure after standardizing numeric features:")
  ##print(str(d))
  
  # Create a 60/40 train/test set
  set.seed(123) # For reproducibility
  trainIndex <- createDataPartition(y = d$y, p = 0.60, list = FALSE)
  train <- d[trainIndex, ]
  test <- d[-trainIndex, ]
  
  # Check structure of train and test sets
  ##print("Training set structure:")
  ##print(str(train))
  ##print("Testing set structure:")
  ##print(str(test))
  
  # Specify a 5-fold cross-validation design
  train_control <- trainControl(method = "cv", number = 5, classProbs = FALSE, summaryFunction = defaultSummary, allowParallel = TRUE)
  
  # Train a linear regression model
  #myModel1 <- train(y ~ ., data = train, method = "lm", trControl = train_control, metric = "RMSE")
  # Log-transform the response variable in the training data
  train$y <- log(train$y)
  # Define the model using the log-transformed response variable
  myModel1 <- train(y ~ ., data = train, method = "glm", trControl = train_control, metric = "RMSE")
  # Generate predictions
  predictions <- predict(myModel1, newdata = test)
  predictions <- exp(predictions)
  
  
  # Evaluate the performance
  train_summary <- defaultSummary(data = data.frame(obs = train$y, pred = exp(predict(myModel1, newdata = train))), model = myModel1)
  test_summary <- defaultSummary(data = data.frame(obs = test$y, pred = exp(predict(myModel1, newdata = test))), model = myModel1)
  
  # Extract coefficients and related statistics
  coefficients_df <- summary(myModel1)$coefficients
  #print(coefficients_df)
  
  
  
  
  # Return the model and summaries
  results<- list(
    model = myModel1,
    predictions = predictions,
    train_summary = as.data.frame(train_summary),
    test_summary = as.data.frame(test_summary),
    train = train,
    test = test
  )
  
  return(results)
}