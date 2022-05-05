# Kevin Sun

##### Background
#The goal of this assignment is to apply all your preprocessing skills to two new sets of data.  You'll need to explore the datasets, identify the issues, and then fix them. You'll also have to select out the columns and rows you need, drop those you don't.  Finally, there's some imputation, one hot encoding, and creating of dummy variables.  


# You'll need both tidyverse AND the caret package.  Load them up!
library(tidyverse)
library(caret)

###### Dataset # 1 - LA parking tickets - 5 points

# This dataset is a slimmed down version of the 5 million or so parking tickets given in LA in 2018!  There a bunch of real-life errors within that happen when you have thousands of people writing tickets.  

# bring in the data
parking <- read_csv("https://docs.google.com/spreadsheets/d/11ahddH6snm10AuxF51MlOISx2yCsJ2WmPKFdfRFpInU/gviz/tq?tqx=out:csv")

# set a seed for reproducible results

set.seed(888)

# QUESTION - Explore the dataset at the whole dataset level.  What columns might not be useful based off your exploration?

# ANSWER - 
glimpse(parking)
summary(parking)

I personally think the columns that contain the violation itself isnt too useful because of the fact that theres a column filled with the 
code which can be used to identify what kind of voliation it was. The rest of the information seems okay because its used to identify what kind
of car and what type/time/cost the violation was. 

# QUESTION - Explore the individual columns within the dataset.  Specifically what levels are present within make, body style, and color?  Any issues that need to be fixed?

# ANSWER - 
summary(factor(parking$make))
There are a few problems within this column, some noticable ones straight off the bat is the similarity between some of the brands listed. Examples are Toyo/Toyt and Merc/Merz.
They can end up causing confusions or just be simple mispellings of one make which messes up the data. 

summary(factor(parking$body_style))
The data appear to be coming heavily from PA body styles and PU ones. Im not sure if the NA would be an issue that needs to be fixed but it defintely stands out.
  
summary(factor(parking$color))
The colors that are represented often are black, gray and wt (white?). The problem is similar to the other two in where there is some terminology that is confusing and hard to diffenciate
such as SI/SL, WH/WT and even GN/GR.

# QUESTION - Explore the agency, plate and violation_code columns. Are they useful?  Should you drop them? Explain why or why not. And if not useful do it.

# ANSWER
summary(factor(parking$agency))
I dont think this column is too useful because of the fact that we are looking at parking tickets so it doesnt matter which agency they are under.

summary(factor(parking$plate))
I dont think this column is too useful because it determines the state of the plate but we are just looking at LA data.

summary(factor(parking$violation_code))
I do think that this column is useful becuase it shows what kind of violation they have commited (based on the code).

parking <- parking %>%select(-plate,-agency)
glimpse(parking)

# QUESTION - based on your earlier exploration there are too many body styles.  Enter summary(factor(parking$body_style)) into your column to get a list of how many observations there are for each style.  Below filter your dataset so it contains only the top four most common body styles.  Ideally you'll do this in a way so that even if the styles in the top four change the filter will still work (i.e. the styles are not hardcoded).

# ANSWER - 
summary(factor(parking$body_style))
top_body_styles <- parking %>% group_by(body_style) %>% summarize(total_obvs = n()) %>% top_n(4)

The data gave back PA, PU, TK, and VN

# QUESTION - When you looked at the unique values within the make column I hope you saw that there were two labels for two of the car brands (Toyota and Mercedes). Use the summary(factor()) method like you did above to figure out which of the two in each brand is the wrong label.  Then use ifelse to correct it in this data frame.

# ANSWER - 
summary(factor(parking$make))
Mec is wrong while MERZ is correct 
Toyo is wrong while TOYT is correct (because of the number count in each; Im assuming more == real)

parking$make <- ifelse(parking$make == 'MERC', 'MERZ', parking$make)
parking$make <- ifelse(parking$make == 'TOYO', 'TOYT', parking$make)

# QUESTION - Colors have some similar errors in labels such as there being both WH and WT for white.  Do what you did above and correct the two errors in color that you find. 

# ANSWER - 
summary(factor(parking$color))
GR is wrong while GY is right
SI is wrong while SL is right
WH is wrong while WT is right
Same assumtions of larger number == right color. 

parking$color <-ifelse(parking$color == 'WH','WT', parking$color)
parking$color <-ifelse(parking$color == 'GR','GY', parking$color)
parking$color <-ifelse(parking$color == 'SI', 'SL', parking$color)

# QUESTION - Our fine column has several issues.  First, there is the $ sign in the number which prevents us from using it as a numeric column.  Remove the $ from all numbers. After removal convert the column to numeric. Next, there are NA values in the data frame. Use whatever method you like to verify that there are NA values. Then use an ifelse statement to median impute these missing values. 

# ANSWER - 
parking$fine <- parking$fine %>% str_remove('[$]') %>% as.numeric()
summary(factor(parking$fine))
parking$fine <- ifelse(is.na(parking$fine), median(parking$fine, na.rm=TRUE), parking$fine)

# QUESTION - The various levels in the violation column are a mess.  Replace all spaces with an underscore '_'.  Replace all forward slashes with a '-'.  Remove all periods.

# ANSWER - 
parking$violation <- parking$violation %>% str_replace_all('\\s', '_') %>% str_replace_all('/', '-')%>% str_remove_all('[\\.]')

  
Book Info and Online (Personal Tips to Remember when double checking):
DONT FORGET \\s is one space and not some weird slash!!
DONT FORGET \\. is the REGEX for period!!


###########################################################

# Now for part two of our assignment - preprocessing our insurance data. - 5 points

# In this dataset our ultimate goal is to predict insurance costs based on the other features.  Thus the target is the charges column.

# bring in data
costs <- read_csv("https://docs.google.com/spreadsheets/d/1WUD22BH836zFaNp7RM5YlNVgSLzo6E_t-sznxzjVf9E/gviz/tq?tqx=out:csv")

# QUESTION - Explore the whole dataset quickly

# ANSWER - 
glimpse(costs)
summary(costs)

# QUESTION - Remember from our earlier lesson that bodyfat is highly colinear with BMI.  Make a plot that shows this colinear relationship between the two.  

# ANSWER -
ggplot(costs, aes (x= bodyfat, y= bmi)) + geom_point()
used scatterplot because it shows relation

# QUESTION - Given the above, drop the bodyfat column

# ANSWER - 
costs <- costs %>% select(-bodyfat)

# QUESTION - How many levels are present in the region column?  After exploring that, use the dummyVars function to one hot encode everything.  Remember our target is charges and will be dropped after you create the dummy variables, so you'll have to remember to join that back on.  If you're struggling look back at the tutorial!  You should have 9 columns in your final encoded dataframe.

# ANSWER - 
summary(factor(costs$region))
4 levels(?) : northeast, northwest, southeast and southwest

dummies <- dummyVars(charges~. , data = costs, fullRank = TRUE)
my_dummies_pred <- predict(dummies, newdata = costs)
costs<-cbind(costs['charges'],my_dummies_pred) 
? unsure if this is right but since the question mentioned that I needed to only add back in charges that is what I ended up doing.Its the only bind I could create that helped me end up with 9 columns. The others ended up with 15 or more.

# QUESTION - Maybe all that matters is if the individual has kids or not, and not how many kids they have.  Make a binary feature for children and call it children_b in your dataframe.  Drop the original children column afterwards.

# ANSWER-
costs$children_b <- ifelse(costs$children > 0, 1, 0)
costs<- costs %>% select(-children)

# QUESTION - Both age and bmi need to be scaled and centered.  Use the scale function to do this to both and assign back to their existing columns.

# ANSWER - 
age_scale <- scale(costs$age)
bmi_scale <- scale(costs$bmi)
costs$age <- age_scale
costs$bmi <- bmi_scale

# QUESTION - Make a linear regression model with charges as your target and all the other features as your predictors.  What region has the lowest healthcare costs?  How much does having children influence insurance costs?  Given we scaled and centered age and bmi, which one has a bigger effect on costs for a single SD increase in the respective feature?

# ANSWER - 
summary(lm(charges~ age +sexmale + bmi + smokeryes + regionnorthwest + regionsoutheast + regionsouthwest + children_b, data = costs))

? couldnt answer because error in lm.fit?
