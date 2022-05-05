# Kevin Sun ISTA 321


#### BACKGROUND
# this week's assignment revolves around understanding how weather conditions influence the number of bike rides in a given day. This is real 2018 data from Chicago's DIVVY bikeshare system that I've connected to  2018 weather data.  

# you have the following relevant columns:
# number_rides - number of rides in a given day
# AWND - Average wind speed in the day
# TAVG - Average temperature in Fahrenheit
# TMAX - Max temperature that day
# TMIN - Min temperature that day
# SNOW - Total snowfall in the day
# PRCP - Total rainfall in the day

# load libraries 
library(tidyverse)

# load data
bikes <- read_csv("https://docs.google.com/spreadsheets/d/1DK8ZSmIgvZ1eVVF33NCNLyLxvYFA8t1jeGrJppaBDpI/gviz/tq?tqx=out:csv")

############## EDA - 1.5 points

# QUESTION - Explore your data with a summary.  In a few sentences describe the data, if you think they make sense, and why that is.  

# ANSWER - 
summary(bikes)
glimpse(bikes)

The temperature columns were a bit strange since it seemed to be three of them (min,max, and avg), everything else seems to make sense
since the rest of the data frame were thigns containing, number of rides, mean trip time, mean distance, mean rider age, 
average wind speed, total snowfall, and total rainfall in the day.

# QUESTION - Make some histograms to understand the distribution of the columns mean_trip_time and mean_distance.  Describe a few features you see in each figure?

# ANSWER - 
hist(bikes$mean_trip_time)
hist(bikes$mean_distance)
ggplot(bikes, aes(x=mean_trip_time)) + geom_histogram()
ggplot(bikes, aes(x=mean_distance)) + geom_histogram()


A large porition of inviduals are finishing around 10-20 minutes and the mean distance was between 1 to 1.5 miles.

# QUESTION - Use a method to figure out the weather conditions on the day that had the most rides.  Also, what date did this occur on? 

# ANSWER - 
max(bikes$number_rides)
max_ride<- bikes[bikes$number_rides == 20585,]
bikes[bikes$number_rides == 20585, 6:11]

Weather conditions: 5.59 wind speed, no rainfall or snow, avg temp was 71 and the max temp was 82 while the min was 61.
the day was 07/28/18


# QUESTION - Explore how at least two pairs of features are correlated visually.  In addition to the figures for each, provide a sentence describing the correlation of each. 

# ANSWER -
plot(bikes$mean_rider_age, bikes$mean_distance)

older = less distance; younger= more distance

############## Linear regression - 1.5 points

# QUESTION -  fit a linear regression model using TAVG as a feature to explain your target, number_rides.  Remember the format is lm(targer ~ feature, data = ...)

# ANSWER - 
model <- lm(number_rides ~ TAVG, data = bikes)
model_summary <- summary(model)

Created a linear regression model using TAVG as the feature and number_rides as the 
target in the data frame bikes and assigned it to the object model.

# QUESTION - How much variation is being explained in your target? Please use code to get the answer from the summary and store it in an object. So CAll that object vs. just writing out the answer from a summary.  

# ANSWER - 
model_summary <- summary(model)
answer<- model_summary$r.squared

The adjusted R-squared value of 0.7853 tells us average temperature alone explains 78.5% of the variation in daily ridership.


# QUESTION - Calculate the confidence interval for B1 - Do it in a way that works if model structure or data gets added to or removed!  You can use the model coefficients from the model summary vs. calculating from scratch.

# ANSWER - 
b1 <- model_summary$coefficients[2,1]
b1_ <- model_summary$coefficients[2,2]
b1_upper<- b1 + 1.98*b1_
b1_lower<- b1- 1.98*b1_
confint(model)

r confidence interval is from 237.66 to 264.733

# QUESTION - Interpret your B1 coefficient in 'plain English.'

# ANSWER - 
For every degree increase in temperature theres an estimated 251 (b1) rides increase.


# QUESTION - Calculate the predicted number of rides if the average temperature is 68 degrees

# ANSWER - 
b0 <- model_summary$coefficients[1,1]
b1 <- model_summary$coefficients[2,1]
b0 + b1*68

14105.29 is the predicted number of rides if the average temperature is 68 degrees

# QUESTION - Make a figure showing the relationship between TAVG and number_rides. This is two continuous variables which should tell you what type of plot you need.  You can then make a vector of x-values.  Then extract the coefficients from the model to predict y.  You then have all you need to add a geom_line() to your plot! Also, I know that ggplot can fit this line for you, but please don't do that. The goal is to demonstrate you understand the different parts of a regression model.

# ANSWER - 
x_val <- seq(from = min(bikes$TAVG), to = max(bikes$TAVG), length.out = nrow(bikes))
y_pred <- b0 + b1*x_val
ggplot(bikes, aes(x = TAVG, y = number_rides)) + geom_point()+geom_line(aes(x = x_val, y = y_pred))

############## Comparing two linear regression models - 1.5 points

# QUESTION - Fit another regression model using AWND as a feature but leave number_rides as the target

# ANSWER - 
model2 <- lm(number_rides ~ AWND, data = bikes)
model2_summary <- summary(model2)


# QUESTION - Which is a better model, this one or the first one you fit?  Use two pieces of evidence from the model summaries to justify your answer.

# ANSWER - 
The TAVG model is better; the R^2 is much higher.


############## Multiple regression - 1.5 points

# QUESTION -  fit a multiple regression model with number of rides as the target and then AWND, PRCP, SNOW, and TAVG as features.  Remember, multiple regression models have all these features in a single model.  

# ANSWER - 
model3 <- lm(number_rides ~ TAVG + AWND + PRCP + SNOW, data = bikes)
model3_summary<- summary(model3)

# QUESTION - How much extra variation did you explain by including these other features compared to just the simple linear model with only TAVG as a feature? 

# ANSWER - 
R^2 increased; the old number was .7859 and now its sitting at .8533 because of the extra features

# QUESTION - Were any of the additional features not important?  Use two pieces of evidence to justify your answer.


# ANSWER
confint(model3)
snow <- lm(number_rides ~ SNOW, data = bikes)
summary(snow)

Snow doesnt seem like it was an important add just becuase of the p-value.


############# Making new features - 1.5 points

# Several of these features are correlated.  For example, snow is a form of precipitation, so they're inherently related. On the other hand, some might make more sense if they're aggregated in a unique way. For example, the difference in min and max temperature might be more informative than just the max alone.

# Let's make a temperature variability feature and see how that relates to the number of our rides

# QUESTION - Make a new feature that gets simply the difference between the max temp of the day and the min temp of the day. Call it TVAR. Fit that TVAR feature in addition to the features you fit in your last multiple regression model

# ANSWER - 
TVAR <- (bikes$TMAX - bikes$TMIN)
new_model <- lm(number_rides ~ TVAR + TAVG + AWND + PRCP + SNOW, data = bikes)
n_model_summary<- summary(new_model)

# QUESTION - Was TVAR important?  If so, what does the effect of TVAR suggest on rider behavior?

# ANSWER - 



############# Multiple regression - interaction models - 1.5 points

# finally, we're going to fit an interaction model
# before that, run this line of code that creates a binary snow feature so that it's just a 1 if there's any snow that day, and a zero if there's not

bikes$SNOW_B <- as.factor(ifelse(bikes$SNOW > 0, 1, 0))


# QUESTION - fit an interaction model between TAVG and SNOW_B

# ANSWER - 
model5 <- lm(number_rides ~ TAVG*SNOW_B, data = bikes)
model5_summary <- summary(model5)
model5_ces <- model5_summary$coefficients

# QUESTION - Interaction models are hard to interpret, so make a plot with two fit lines instead.  Remember, make a sequence of X values, then use these to estimate the Y values... one Y vector for if it snowed, the other if it didn't.  Again, do this from scratch and not using ggplot to fit the line itself. 

# ANSWER - 
ggplot(bikes, aes(x = TAVG, y = number_rides)) + geom_point(aes(color = SNOW_B))
x_vals = seq(from = min(bikes$TAVG), to = max(bikes$TAVG), length.out = nrow(bikes))
y_preds_snow <- model5_coefs[1,1] + model5_ces[2,1]*x_vals +model5_ces[3,1]*1 +model5_ces[4,1]*x_vals*1
y_preds_nosnow <- model5_ces[1,1] + model5_ces[2,1]*x_vals +model5_ces[3,1]*0 +model5_ces[4,1]*x_vals*0
ggplot(bikes, aes(x = TAVG, y = number_rides)) +geom_point(aes(color = SNOW_B)) +geom_line(aes(x = x_vals, y = y_preds_nosnow)) +geom_line(aes(x = x_vals, y = y_preds_snow))

# QUESTION -  Based on the plot you created, interpret how snow and temperature interact to influence the number of rides. In other words, how does it snowing or not influence the relationship between temperature and the number of rides in a day?

# ANSWER - 
Snow and temp have a positive correlation to the number of rides because fewer individuals ride bikes on days where the temperature is below freezing(snowing).


############ ONE LAST QUESTION - 1.5 points

# QUESTION - Make a model to determine how average temperature influences the average age of the rider on a given day.

# ANSWER - 
model6 <- lm(mean_rider_age ~ TAVG, data = bikes)
summary(model6)


# QUESTION - Given this model, what can you say about how temperature influences the age demographics of the bikeshare users? 

# ANSWER - 
Younger invidiuals are more likely to ride bikes in extreme temps compared to older indiviudals who prefer lower temperature days.


