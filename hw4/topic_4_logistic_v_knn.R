# Your info

# Classification homework - the goal of this assignment is to predict if someone has 'good' or 'bad' credit.  You're going to use both a logistic regression model as well as a kNN model to try and best predict credit class.  

# NOTE - People get most tripped up at the end in either making sure their predictions are outputted as classes (or converted to them) and comparing them to the true test targets. 

# Load packages
library(tidyverse)
library(caret)

#############  Import and Explore - 0.5 points

# Bring in data
credit <- read_csv("https://docs.google.com/spreadsheets/d/1jFkOKgD5NGeD8mDj_42oBNJfFVK42-1cMKk0JxVFxeA/gviz/tq?tqx=out:csv")


# QUESTION - As always, take some time to explore your data.  What levels are present in the character columns?  

# ANSWER - 
glimpse(credit) 

# was not sure if I was suppose to list everything
unique(credit$checking_status)
unique(credit$credit_history)
unique(credit$purpose)
unique(credit$savings_status)
unique(credit$employment)
unique(credit$personal_status)
unique(credit$other_parties)
unique(credit$property_magnitude)
unique(credit$other_payment_plans)
unique(credit$housing)
unique(credit$own_telephone)
unique(credit$foreign_worker)
unique(credit$credit_score)

# QUESTION - Based on your exploration, pick two columns and describe how you think they might be related to credit score.  What levels within these features will have what effect on the target

# ANSWER

#Two columns I picked within the credit dataset is credit_history and checking_status. I believe credit history will more then likely have a highest impact to credit score because of the levels presented (critical, existing paid, etc.). 
#Its something that shows if individuals have been reliable in the past with their credit. To me, checking status is another column of importance just because through the levels it shows, the column is comparing x to a number on both sides which Im guessing is the balance of that accounts that they currently hold.

############ Preprocessing - 1.5 points


# QUESTION - Start by creating your dummy variables.  

# ANSWER  -
credit2 <- credit
credit_dummy <- dummyVars(credit_score ~ ., data = credit2, fullRank = TRUE)
credit2 <- predict(credit_dummy, newdata = credit2)
credit2 <- data.frame(credit2)
scores_c <- credit %>% 
  select(credit_score)
credit2 <- cbind(credit2, scores_c)


# QUESTION - you need to convert your target 'credit_score' to a binary factor.  Make it so that if the value is 'good' that it's replaced with a 1, and if it's 'bad' it's replaced with a 0.  Then convert that to a factor. Do this all while overwriting the original 'credit_score' column so that you don't have two targets. 

# ANSWER - 
credit2$credit_score <- factor(ifelse(credit$credit_score == 'good', 1, 0))


# QUESTION - Now split your data into train and test features and targets.  Use an 80/20 train/test split.


# ANSWER - 
set.seed(888) # run this first and then put your answer below it. 
cred_split <- createDataPartition(credit2$credit_score, p = 0.8, list = FALSE)
features_train <- credit2[ cred_split, !(names(credit2) %in% c('credit_score'))]
features_test <- credit2[ -cred_split, !(names(credit2) %in% c('credit_score'))]
target_train <- credit2[ cred_split, 'credit_score']
target_test <- credit2[ -cred_split, 'credit_score']

# QUESTION - Take a second to verify that your targets and features contain the proper data.  Check the number of rows in them.  Check to make sure the proper columns are in them as well!

# ANSWER - 
nrow(features_train)
ncol(features_train)
#48 columns
# 800 rows
nrow(target_test)
ncol(target_test)
#Null ?


# QUESTION - On to preprocessing.  Preprocess your data using centering, scaling, and the knnImpute within method.  

# ANSWER - 
preprocess_object <- preProcess(features_train, method = c('center', 'scale', 'knnImpute'))
features_train <- predict(preprocess_object, newdata = features_train)
features_test <- predict(preprocess_object, newdata = features_test)
###############  KNN Model - 1.5 points


# QUESTION - Use the formula from the lesson/book to calculate your k value before we fit our kNN model.  Remember to round to an odd value.

# ANSWER
#from book
knn_pred <- predict(preprocess_object, newdata = features_test, type = 'class')
#unsure what value to round it to?

# QUESTION - Fit a kNN model on your training data 


# ANSWER - 
#from book
knn_fit <- knn3(features_train, target_train, k = 5)


# QUESTION - Now use that to predict the credit_scorees of your test data

# ANSWER - 
#from book
knn_pred <- predict(knn_fit, newdata = features_test, type = 'class')

# QUESTION - Make a predictions data frame with your true target values and your knn_pred values

# ANSWER - 
predictions <- cbind(data.frame(target_test, knn_pred))
summary(predictions)


##################  Logistic Regression - 1.5 points

# QUESTION - Now fit a logistic regression.  Remember you have to join your features and target back together to train your model. You'll also have to rename your target back to 'credit_score'

# ANSWER - 
train <- cbind(features_train, target_train)
train <- train %>%rename(credit_score= target_train)

# QUESTION - Check out a summary of your logistic regression model.  Are all features important?  Do the ones you made predictions about earlier pan out?

# ANSWER - 
log_train <- glm(credit_score~., family = 'binomial', data = train)
summary(log_train)

#yes; it seems many of the features arent that significant baed on the summary but credit history is for sure and so is checking.

# QUESTION - Generate your predictions for your test data.  Be sure to look at the data and convert the values to classes if needed.

# ANSWER - 
log_pred <- predict(log_train, newdata = features_test, type = 'response')
summary(log_pred)
glimpse(log_pred)
head(log_pred)

# QUESTION - Add these logistic regression predictions to your predictions data frame as a new column

# ANSWER - 
predictions$log_pred <- factor(log_pred)
summary(predictions)


#############  Error rates - 1 point

# QUESTION - Calculate error rates between our true test values and the predicted values from both models.  Which model did best?


# ANSWER - 
predictions$knn_error <- ifelse(predictions$target_test != predictions$knn_pred, 1, 0)
summary(predictions)
#log_pred did it best

# QUESTION - Make confusion matrices for both models.  Which model had more true positives?  Which had more true negatives?   

# ANSWER - 
knn_conf<-confusionMatrix(predictions$knn_pred, predictions$target_test)
log_conf<-confusionMatrix(predictions$log_pred, predictions$target_test) #data cannot have more levels than the reference error (stuck)_
knn_conf$table
log_conf$table


########### Tweaks and summary - 1.5 points

# QUESTION - Go dial k back to 9 and then rerun the script.  Did that improve model fit?

# ANSWER - 
k_form <- sqrt(length(target_test))
#15
knn_fit_9 <- knn3(features_train, target_train, k = 9)
knn_pred_9 <- predict(knn_fit_9, features_test, type = 'class' )
predictions$knn_pred_9 <- knn_pred_9
predictions$knn_error_9 <- ifelse(predictions$target_test != predictions$knn_pred_9, 1, 0)
summary(predictions)

# QUESTION - If we created a preprocessing object using our full dataset and then applied that to both the training and test data, how would this impact our accuracy on the test data? Would this accuracy hold on truly new and unseen data?  Why or why not and what is this process called?

# ANSWER -  
#data leakage; it would perform well on the test data until it encounters new data


# QUESTION - If you just used a naive model, what would your error rate be if you just predicted every credit score to be 'good.'  Use code to calculate the naive error rate.

# ANSWER - 


# QUESTION - Based on the naive error rate, how much better are our models?  

# ANSWER


