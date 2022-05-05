# Kevin Sun

# This week's assignment will focus on using k-means clustering to find out how many groups of wines exist based on a dataset of 178 wines that vary in 13 different features.  Each of these features relate to some aspect of the wine's chemical composition.  Given there are lots of different wine varieties, the question is if we can detect varietal differences based only on these chemical properties.

# Packages and data
library(tidyverse)
library(caret)
wine <- read_csv("https://docs.google.com/spreadsheets/d/1QA96h2A_i35FBKCrlmm8_QPgdcUYSZlTDeNFSUl8sEc/gviz/tq?tqx=out:csv")
# Selected Heading because it wouldnt work otherwise

# QUESTION - Scale your data

# ANSWER
wine_scaled <- data.frame(scale(wine))


# QUESTION -  fit a k-means model with k= 5 and plot the resulting clusters with alcohol on the x and flavanoids on the y.  Be sure to color your points by cluster and make sure cluster isn't a continuous variable. Also, be sure to make a copy of your original data frame to assign your clusters back to as we're going to use the scaled data again.

# ANSWER - 
#straight from book

wine_kmeans_scaled <- kmeans(wine_scaled, centers = 5) #k = 5 ; n= ?? 
wine_kmeans_scaled_clusters <- factor(wine_kmeans_scaled$cluster)
wine$cluster <- wine_kmeans_scaled_clusters
ggplot(wine, aes(x = alcohol, y = flavanoids, color = cluster)) + geom_point() #plot points

# QUESTION - What do you think of the results?  Do the clusters make sense?  Too many or too few?

# ANSWER - 

#I  think the algorithm did an okay job since the clusters are extremely spread apart to where most are mxing and some are just in the middle despite it being 
#different colors completely. I think we have too many clusters and could do better if we removed 1-2 of them to create more groups. 


# QUESTION - Calculate the total within-cluster sum of squares from your above model.

# ANSWER - 

print(wine_kmeans_scaled$withinss)
#[1] 243.2208 201.3560 246.5796 302.9915 104.5911


# QUESTION - We're now going to use the elbow method to figure out our optimal k.  First make your empty data frame with two columns and 10 rows.  One column should be named for the within column variation and the other for the number of k

# ANSWER
k_means <- data.frame(matrix(ncol = 2, nrow = 10))
colnames(k_means) <- c('k', 'within_ss')



# QUESTION - Now use a for loop to fit a model for k values ranging from 1 to 10.  You should fit the model for each value in i, calculate the within-column variation, and then add that value and the k value into your empty data frame

# ANSWER

for(i in 1:10) {
  km <- kmeans(wine_scaled, centers = i)
  within_ss <- km$tot.withinss
  k_means[i, 'k'] <- i
  k_means[i, 'within_ss'] <- within_ss}


# QUESTION - Now make your elbow plot

# ANSWER

ggplot(k_means, aes(x = k, y = within_ss)) +
  geom_line() +
  labs(x = 'number of clusters', y = 'total within-cluster sum of squares') +
  theme_classic()


# QUESTION - Based on this, what value k should you use?  Go and refit the single model with this value, assign clusters back to the data, and replot, coloring by cluster

# ANSWER
#I personally believe we should use a k value of anything that us after 2.5 (maybe 3?) becuase of the fact that it doesnt drop largely after that which the book mentions it being us
#hitting the "elbow".

wine_kmeans_scaled <- kmeans(wine_scaled, centers = 3)
wine_kmeans_scaled_clusters <- factor(wine_kmeans_scaled$cluster)
wine$cluster <- wine_kmeans_scaled_clusters
ggplot(wine, aes(x = alcohol, y = flavanoids, color = cluster)) + geom_point()


# QUESTION - How good of a job did this do clustering?  Give an example of what you could use this model for.

# ANSWER - 

#It became more clutered in a good way in which there isnt a ton of "noise" or mixes of multiple small cluteres which can cause confusion and overall be a pain to look at (k=5). 
