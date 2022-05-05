# Kevin Sun


# The goal of this homework is simple. You're going to take your knn model from last homework and perform k-fold cross-validation using a for loop.  

# Load packages
library(tidyverse)
library(caret)
set.seed(888)

# Bring in data
credit <- read_csv("https://docs.google.com/spreadsheets/d/1jFkOKgD5NGeD8mDj_42oBNJfFVK42-1cMKk0JxVFxeA/gviz/tq?tqx=out:csv")

# QUESTION - Perform your dummy variable conversion first.  Also remember to bind back on your target feature 'credit_score' and then convert that to a binary factor.  Look at the home solution if you're struggling.  Do you get 300 0's and 700 1's?

# ANSWER - 

credit_dummy <- dummyVars(credit_score ~ ., data = credit, fullRank = TRUE)
credit_df_encoded <- predict(credit_dummy, newdata = credit)
credit_df_encoded <- data.frame(credit_df_encoded)
credit_df <- cbind(credit_df_encoded, credit[,'credit_score'])
credit_df$credit_score <- factor(ifelse(credit$credit_score == 'good', 1, 0))

# QUESTION - Create your split index.  I want you do start with 10 folds and a training set size of 70% of the data.  

# ANSWER - 

#dont forget: tiimes = 10 is what it means to create 10 folds and p needs to be .7
split_index <- createDataPartition(credit_df$credit_score, p = 0.7, list = FALSE, times = 10)



# QUESTION - Initalize your empty data frame.  Make sure it has two columns...  one for error and the other for fold_number

# ANSWER - 

error_ <- data.frame(matrix(ncol = 2, nrow = ncol(split_index)))
colnames(error_) <- c('test_error', 'fold')


# QUESTION - create your for loop.  It should run for each column within split index.  For each run it should use the i'th entry of the index to split the data, then preprocess, fit a model, predict with that knn model (use k = 11), calculate the TEST ERROR RATE (see 2.2.3 in the book for a reminder), then add that error to the ith spot of the error column in the data frame.  Also add the fold number to the data frame.

# ANSWER - 

# taken from the book example
for(i in 1:nrow(error_)){
  features_train <- credit_df[split_index[,i], !(names(credit_df) %in% c('credit_score'))]
  features_test <- credit_df[-split_index[,i], !(names(credit_df) %in% c('credit_score'))]
  target_train <- credit_df[split_index[,i], 'credit_score']
  target_test <- credit_df[-split_index[,i], 'credit_score']
  preprocess_object <- preProcess(features_train, method = c('scale', 'center', 'knnImpute'))
  features_train <- predict(preprocess_object, features_train)
  features_test <- predict(preprocess_object, features_test)
  knn_fit <- knn3(features_train, target_train, k = 11)
  knn_pred <- predict(knn_fit, features_test, type = 'class')
  error <- mean(ifelse(target_test != knn_pred, 1, 0))
  error_[i, 'test_error'] <- error
  error_[i, 'fold'] <- i
}


# QUESTION - Make a figure using your data frame of errors and fold numbers. 

# ANSWER - 

ggplot(error_, aes(x = fold, y = test_error)) + geom_line(color = 'green')
# did not include the small or normal error calcs; wasnt sure if needed

# QUESTION - Now that your for loop is running play around with the k of the knn training or the split amount.  Can you get a lower mean standard error?  What value allows you to minimize error more? 
mean(error_$test_error)

for(i in 1:nrow(error_)){
  features_train <- credit_df[split_index[,i], !(names(credit_df) %in% c('credit_score'))]
  features_test <- credit_df[-split_index[,i], !(names(credit_df) %in% c('credit_score'))]
  target_train <- credit_df[split_index[,i], 'credit_score']
  target_test <- credit_df[-split_index[,i], 'credit_score']
  preprocess_object <- preProcess(features_train, method = c('scale', 'center', 'knnImpute'))
  features_train <- predict(preprocess_object, features_train)
  features_test <- predict(preprocess_object, features_test)
  knn_fit <- knn3(features_train, target_train, k = 13)
  knn_pred <- predict(knn_fit, features_test, type = 'class')
  error <- mean(ifelse(target_test != knn_pred, 1, 0))
  error_[i, 'test_error'] <- error
  error_[i, 'fold'] <- i
}

for(i in 1:nrow(error_)){
  features_train <- credit_df[split_index[,i], !(names(credit_df) %in% c('credit_score'))]
  features_test <- credit_df[-split_index[,i], !(names(credit_df) %in% c('credit_score'))]
  target_train <- credit_df[split_index[,i], 'credit_score']
  target_test <- credit_df[-split_index[,i], 'credit_score']
  preprocess_object <- preProcess(features_train, method = c('scale', 'center', 'knnImpute'))
  features_train <- predict(preprocess_object, features_train)
  features_test <- predict(preprocess_object, features_test)
  knn_fit <- knn3(features_train, target_train, k = 9)
  knn_pred <- predict(knn_fit, features_test, type = 'class')
  error <- mean(ifelse(target_test != knn_pred, 1, 0))
  error_[i, 'test_error'] <- error
  error_[i, 'fold'] <- i
}

#currently the mean for the test column is 2.68, 13 [0.2716667] made it higher so next I went lower to 9[0.278] and still ended up with a higher mean standard so my guess is no.
