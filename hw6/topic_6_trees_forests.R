#Kevin Sun

# load libraries

library(rpart)
library(rpart.plot)
library(tidyverse)
library(caret)
library(randomForest)

# This week's homework will focus on evaluating a decision tree and random forest model to classify credit scores as 'good' or 'bad'.  

######### Import and datatype conversion - 0.5 points

# Load data
credit <- read_csv("https://docs.google.com/spreadsheets/d/1jFkOKgD5NGeD8mDj_42oBNJfFVK42-1cMKk0JxVFxeA/gviz/tq?tqx=out:csv")
set.seed(888)

# QUESTION - Convert all your character strings to factors.  I did this in the rpubs lesson using the mutate_if function

# ANSWER - credit <- credit %>%
credit <- credit %>% mutate_if(is.character, as.factor)

######### Simple decision tree - 1.5 points


# QUESTION - before we starting trying to predict anything, make a decision tree with all the features and credit_score as your target.  Also make a plot of this decision tree.

# ANSWER - 
credit_tree <- rpart(credit_score ~., data = credit)
rpart.plot(credit_tree)


# QUESTION - Are there too many terminal nodes?  Apply a method to reduce the complexity of the tree and plot the results.  Use a cost penalty of 0.05.

# ANSWER - 

#There is alot of terminal nodes to be able to properly read whats going on.
credit_tree_pruned <- prune(credit_tree, cp = 0.05)
rpart.plot(credit_tree_pruned)

# QUESTION - Given the above plot, what are the effects of the different levels of checking account status (amount of money in it)?

# ANSWER - 
# (<0, 0<=X<200) is what it shows as different levels which splits into two (yes/no); 
# it seems like having >=200 has a better chance of having good credit in it


######### Setting up your loop - 1 points


# QUESTION - Now let's split your data into training and test sets using an 80/20 train/test split.  Create 10 folds.

# ANSWER -
split_ <- createDataPartition(credit$credit_score, p = 0.8, list = FALSE, times = 10)
head(split_) #double check


# QUESTION - Create an empty data frame with three columns, one for decision tree error rate, one for logistic regression error rate, and another for fold number.  The data frame should have as many rows as folds you created.  Don't forget to name the columns.

# ANSWER
error_dataf <- data.frame(matrix(ncol = 3, nrow = ncol(split_))) #shows 10 rows, 3 columns
colnames(error_dataf) <- c('tree_error', 'log_error', 'fold_num') #renaming


######### Fitting & testing your tree - 2 points


# QUESTION - Create a for loop that splits your data, then both fits a decision tree and logistic regression model with credit_score as the target and the rest of the columns as your features.  It should also generate predictions and then calculate and store the error rates as well as the fold number. 


# ANSWER - 
for(i in 1:nrow(error_dataf)){
  features_train <- credit[split_[,i], !(names(credit)%in%c('credit_score'))]
  target_train <- credit[split_[,i],"credit_score"]
  
  features_test <-credit[split_[,i], !(names(credit)%in%c('credit_score'))]
  target_test <- credit[split_[,i],"credit_score"]
  
  preprocess_obj <- preProcess(features_train, method = c('scale','center','knnImpute'))
  features_train <- predict(preprocess_obj,features_train)
  features_test <- predict(preprocess_obj,features_test)
  full_train <- cbind(features_train, target_train)
  
  credit_tree <- rpart(credit_score~., data = full_train)
  tree_pred <- predict(credit_tree, newdata = features_test)
  tree_pred <- ifelse(tree_pred>= .5, 1, 0)
  
  log_train <- glm(credit_score~., family = 'binomial', data= full_train)
  log_pred <- predict(log_train, newdata = features_test, type = 'response')
  log_pred <- ifelse(log_pred >= .5,1,0)
  
  c_tree_error <- mean(ifelse(target_test != tree_pred, 1, 0))
  c_log_error <- mean(ifelse(as.numeric(target_test$credit_score) != as.numeric(log_pred), 1, 0))
  
  error_dataf[i, 'fold_number'] <- i
  error_dataf[i, 'tree_error'] <- c_tree_error
  error_dataf[i, 'log_error'] <- c_log_error
}

######### Fitting a random forest - 1 points


# QUESTION - Now fit a random forest model, generate predictions and mean error rate.  No need to do the for loop.  You can either generate a new single split index or just use an entry from your previously created split index

# ANSWER - 
train <- credit[split_[,1],]
features_test <- credit[-split_[,1], !(colnames(credit) %in% c('credit_score'))]
target_test <- credit[-split_[,1], 'credit_score']

credit_rf <- randomForest(credit_score ~. , data = train, mtry = 3)
credit_rf_pred <- predict(credit_rf, newdata = features_test, type = 'class')
credit_rf_error <- ifelse(credit_rf_pred != target_test$credit_score, 1, 0)


######### Calculation error and wrap up - 1.5 points


# QUESTION - Calculate the mean error rate for your decision tree and logistic regression models.  How do those compare to the error rate in your random forest?  Include code to do both below.

# ANSWER

mean(error_dataf$tree_error) #tree
mean(error_dataf$log_error) #log

mean(credit_rf_error) #error rate random forest
confusionMatrix(target_test$credit_score, credit_rf_pred) #error rate table for random forest
#The one with the lowest error is the random forest with only 23.5%  and an accuracy rating of 76.5%.


# QUESITON - You want to create a credit predictor for when someone applies for a credit card.  When doing so they input all the same feature data and then you need the algorithm to return back 'good' or 'bad'.  Which model type would you use for this?

# ANSWER - 
Decision Tree(?)

# QUESTION - Which features were most important in your random forest model.  Give me the top three.  

# ANSWER - 

checking, saving and duration
