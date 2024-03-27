#
# Author: Empire Realty
# Purpose: Analysis on NYC Airbnb Data
#

# NYC Airbnb data set
# Data as of January 05, 2024
# Link to data set - https://www.kaggle.com/datasets/vrindakallu/new-york-dataset

# Package library
#install.packages("leaflet")
library(leaflet)
#install.packages("maps")
library(maps)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("rnaturalearth")
library(rnaturalearth)
#install.packages("rnaturalearthdata")
library(rnaturalearthdata)
#install.packages("dplyr")
library(dplyr)
#install.packages("tidyr")
library(tidyr) 
#install.packages("readr")
library(readr)
#install.packages("arules")
library(arules)
#install.packages("arulesViz")
library(arulesViz)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("cluster")
library(cluster)
#install.packages("rpart")
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
#install.packages("e1071")
library(e1071)
install.packages("tcltk2")
library(tcltk2)

# Color library
"#606C38"
"#283618"
"#6F1D1B"
"#FEFAE0"
"#FFE6A7"
"#DDA15E"
"#BC6C25"
"#BB9457"
"#99582A"
"#432818"

# Read CSV
airbnb_raw <- read.csv("C:\\Users\\gsgro\\OneDrive\\Desktop\\Syr_MSBA\\Term 3\\Information Visualization\\Final Project\\new_york_listings_2024.csv"
                       , stringsAsFactors = TRUE
                       , header = TRUE)

# Data set size
dimensions <- dim(airbnb_raw)
num_rows <- dimensions[1]
num_col <- dimensions[2]
dataSize <- (num_col*4)*(num_rows/100)

# Analyze raw data set
View(airbnb_raw)
summary(airbnb_raw)
str(airbnb_raw)
colnames(airbnb_raw)
na_count_per_column <- colSums(is.na(airbnb_raw))

# Log of price
airbnb_raw$log_price <- log10(airbnb_raw$price)

# Single Dimensional Plots
par(mfrow = c(2,2))

barplot(table(airbnb_raw$neighbourhood_group)
        , main = "Count of Airbnb Listings by Boruogh"
        , col = c("#995928", "#fce6a7", "#432818", "#626e38", "#6f1d1b")
        , xlab = "NYC Borough"
        , ylab = "Count")

pie(table(airbnb_raw$neighbourhood_group),
    col = c("#995928", "#fce6a7", "#432818", "#626e38", "#6f1d1b"),
    main = "Count of Airbnb Listings by Borough",
    labels = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"))

hist(airbnb_raw$beds, col = "#6F1D1B"
        , main = "Histogram of Bed Distribution"
        , xlab = "Number of Beds")

hist(airbnb_raw$log_price, col = "tan"
        , main = "Histogram of Price Distribution"
        , xlab = "Price (log10)")

plot(airbnb_raw$longitude, airbnb_raw$latitude
        , col = rgb(.4, .6, .2, alpha = 0.3)
        , main = "Airbnb Listing Locations"
        , xlab = "Longitude"
        , ylab = "Latitude"
        , axes = FALSE
        , frame.plot = FALSE
        , ann = FALSE)


# Multi-dimensional Plots

average_price <- aggregate(price ~ neighbourhood_group, data = airbnb_raw, FUN = mean)
ggplot(data = average_price, aes(x = neighbourhood_group, y = price, fill = neighbourhood_group)) +
  geom_bar(stat = "identity") +
  ggtitle("Average Price of Airbnb Listings by Borough") +
  xlab("Borough") +
  ylab("Average Price") +
  scale_fill_manual(values = c("#995928", "#fce6a7", "#432818", "#626e38", "#6f1d1b")) +
  theme_minimal() +
  theme(panel.grid = element_blank())


ggplot(airbnb_raw, aes(x = neighbourhood_group, y = log_price)) +
  geom_boxplot(fill = c("#995928", "#fce6a7", "#432818", "#626e38", "#6f1d1b"), color = "black") +
  labs(title = "Boxplot of Price by Borough", x = "Borough", y = "Price (log10)") +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

ggplot(airbnb_raw, aes(x = neighbourhood_group, fill = room_type)) +
  geom_bar() +
  labs(title = "Stacked Bar Chart of Room Type by Neighborhood", x = "Neighborhood", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Density Map
myColors <- c("Manhattan" = "#432818", 
              "Brooklyn" = "#fce6a7", 
              "Queens" = "#626e38", 
              "Bronx" = "#995928", 
              "Staten Island" = "#6f1d1b")

plot(airbnb_raw$longitude, airbnb_raw$latitude
     , col = myColors[as.numeric(airbnb_raw$neighbourhood_group)]
     , main = "Airbnb Listing Location by Borough"
     , xlab = ""
     , ylab = ""
     , axes = FALSE
     , frame.plot = FALSE
     , ann = FALSE)

# Count the occurrences of each neighborhood
neighborhood_counts <- airbnb_raw %>%
  group_by(neighbourhood) %>%
  summarize(count = n())

# Filter neighborhoods with a count of 50 or greater
neighborhoods_filtered <- neighborhood_counts %>%
  filter(count >= 250) %>%
  pull(neighbourhood)

# Filter the original dataset to include only neighborhoods with a count of 500 or greater
airbnb_filtered <- airbnb_raw %>%
  filter(neighbourhood %in% neighborhoods_filtered)

# Reorder the levels of the 'neighbourhood' variable based on count in reverse order
airbnb_filtered$neighbourhood <- factor(airbnb_filtered$neighbourhood, levels = neighborhood_counts$neighbourhood[order(neighborhood_counts$count)])

# Plot the chart with filtered and sorted neighborhoods
ggplot(data = airbnb_filtered, aes(x = neighbourhood, fill = neighbourhood_group)) +
  geom_bar() +
  ggtitle("Count of Neighborhoods in NYC Dataset") +
  xlab("Neighborhood") +
  ylab("Count") +
  coord_flip() +
  scale_fill_manual(values = c("Manhattan" = "#432818", 
                               "Brooklyn" = "#fce6a7", 
                               "Queens" = "#626e38", 
                               "Bronx" = "#995928", 
                               "Staten Island" = "#6f1d1b")) +
  theme_minimal() +
  theme(panel.grid = element_blank())


airbnb_analysis <- read.csv("C:\\Users\\gsgro\\OneDrive\\Desktop\\Syr_MSBA\\Term 3\\Information Visualization\\Final Project\\new_york_listings_2024.csv"
                            , stringsAsFactors = TRUE
                            , header = TRUE)
(sum(is.na(airbnb_analysis)))

# Making sure there are no duplicates in the data
Dup <- which(duplicated(airbnb_analysis))
length(Dup)

# Remove columns 1(id), 3 (host_id) and 4 (host_name)
airbnb_analysis <- airbnb_analysis[, -c(3:4)]
airbnb_analysis <- airbnb_analysis[, -c(1)]
view(airbnb_analysis)

# Removed column 11 (license)
airbnb_analysis <- airbnb_analysis[, -c(11)]
view(airbnb_analysis)

# Convert char features to factors
airbnb_analysis$name=factor(airbnb_analysis$name)
airbnb_analysis$room_type= factor(airbnb_analysis$room_type)
airbnb_analysis$bedrooms= factor(airbnb_analysis$bedrooms)
airbnb_analysis$baths= factor(airbnb_analysis$baths)
airbnb_analysis$rating= factor(airbnb_analysis$rating)
airbnb_analysis$neighbourhood= factor(airbnb_analysis$neighbourhood)

ggplot(airbnb_analysis, aes(x=neighbourhood)) + geom_bar(color="blue", fill="blue")
ggplot(airbnb_analysis, aes(x=room_type)) + labs(
  
  x = "Neighbourhood Group", y = "Count") + geom_bar(color="red", fill="red")
ggplot(airbnb_analysis, aes(x=price)) + geom_bar(color="purple", fill="purple")
ggplot(airbnb_analysis, aes(x=rating)) + geom_bar(color="orange", fill="orange")
ggplot(airbnb_analysis, aes(x=bedrooms)) + geom_bar(color="yellow", fill="yellow")
ggplot(airbnb_analysis, aes(x=beds)) + geom_bar(color="green", fill="green")
ggplot(airbnb_analysis, aes(x=baths)) + geom_bar(color="black", fill="black")
ggplot(airbnb_analysis, aes(x=number_of_reviews)) + geom_bar(color="pink", fill="pink")
ggplot(airbnb_analysis, aes(x=minimum_nights)) + geom_bar(color="grey", fill="grey")
ggplot(airbnb_analysis, aes(x=last_review)) + geom_bar(color="brown", fill="brown")

ggplot(airbnb_analysis, aes(x=neighbourhood_group, y=room_type, size =price, color="red" )) + labs(title = "Price Based on Room Type by Neighbourhood Group", 
                                                                                                   
                                                                                                   x = "Neighbourhood Group", y = "Room Type") +
  geom_point(alpha=2)


airbnb_model <- read_csv("C:\\Users\\gsgro\\OneDrive\\Desktop\\Syr_MSBA\\Term 3\\Information Visualization\\Final Project\\new_york_listings_2024.csv") 

# Convert non-numeric columns to numeric if appropriate
airbnb_model <- as.data.frame(airbnb_model)  # Convert to data frame if necessary
numeric_data <- sapply(airbnb_model, function(x) {
  if (!is.numeric(x)) {
    as.numeric(as.character(x))  # Convert non-numeric columns to numeric
  } else {
    x  # Leave numeric columns unchanged
  }
})

# Check for missing values
any_missing <- anyNA(numeric_data)

# Handle missing values if necessary
if (any_missing) {
  # Remove rows with missing values
  numeric_data <- na.omit(numeric_data)
}

# Convert numeric_data to a data frame
numeric_data <- as.data.frame(numeric_data)

# Check for missing values again
any_missing <- anyNA(numeric_data)

# If there are still missing values, you may want to handle them appropriately

# Convert non-numeric columns to numeric again (if needed)
numeric_data <- sapply(numeric_data, function(x) {
  if (!is.numeric(x)) {
    as.numeric(as.character(x))  # Convert non-numeric columns to numeric
  } else {
    x  # Leave numeric columns unchanged
  }
})

# Subset dataset to include only numeric columns
numeric_data <- airbnb_model[, c("number_of_reviews", "price")]

# Handle missing values if necessary
numeric_data <- na.omit(numeric_data)

# Set seed for reproducibility
set.seed(20)

# Perform k-means clustering on numeric data
Clusters <- kmeans(numeric_data, 6)

# Get cluster centers
cluster_centers <- Clusters$centers

# Assign cluster labels to original dataset
airbnb_model$cluster <- as.factor(Clusters$cluster)

# Plot results
clusplot(numeric_data, Clusters$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# Check if the dataset 'airbnb_model' exists
if (!exists("airbnb_model")) {
  stop("Dataset 'airbnb_model' not found. Make sure to load or read the dataset.")
}

# Subset dataset to include only numeric columns (if needed)
numeric_data <- airbnb_model[, c("price")]

# Handle missing values if necessary
numeric_data <- na.omit(numeric_data)

# Compute distance matrix (example using Euclidean distance)
distance_matrix <- dist(numeric_data, method = "euclidean")

# Perform hierarchical clustering
hac_result <- hclust(distance_matrix, method = "complete")



# Increase plot size
par(mfrow = c(1, 1), mar = c(5, 4, 4, 10))  # Adjust margins as needed
plot(hac_result, cex = 0.5, main = "Hierarchical Clustering Dendrogram")

# Zoom out by adjusting the height threshold
abline(h = 20, col = "red")  # Example: zoom out to height 20
# Zoom in on specific clusters by specifying height ranges
# Define height ranges for clusters of interest
cluster1_height <- 10  # Example: cluster 1 height range
cluster2_height <- 15  # Example: cluster 2 height range

# Draw rectangles around clusters of interest
rect.hclust(hac_result, h = cluster1_height, border = "blue")
rect.hclust(hac_result, h = cluster2_height, border = "green")

# Set seed for reproducibility
set.seed(20)

# Randomly sample 1% of the observations
sampled_data <- airbnb_model[sample(nrow(airbnb_model), nrow(airbnb_model) * 0.01), ]

# Subset dataset to include only numeric columns
numeric_data <- sampled_data[, c("price", "neighbourhood_group")]

# Handle missing values if necessary
numeric_data <- na.omit(numeric_data)

# Perform hierarchical clustering
hac_result <- hclust(dist(numeric_data), method = "complete")

# Plot dendrogram with labels
plot(hac_result, labels = row.names(numeric_data), main = "HAC Analysis of Price and Neighbourhood Groups",
     xlab = "Listings", ylab = "Distance", sub = NULL)

# Adding labels for 'price' and 'number_of_reviews'
text(hac_result$merge[, 1], -1, labels = numeric_data$price[hac_result$order[1:length(numeric_data)]], col = "blue", srt = 90)
text(hac_result$merge[, 2], -1, labels = numeric_data$neighbourhood_group[hac_result$order[1:length(numeric_data)]], col = "red", srt = 90)

# Set seed for reproducibility
set.seed(20)

# Randomly sample 1% of the observations
sampled_data <- airbnb_model[sample(nrow(airbnb_model), nrow(airbnb_model) * 0.01), ]

# Subset dataset to include only numeric columns
numeric_data <- sampled_data[, c("price", "neighbourhood_group")]

# Handle missing values if necessary
numeric_data <- na.omit(numeric_data)

# Perform hierarchical clustering
hac_result <- hclust(dist(numeric_data), method = "complete")

# Plot dendrogram with labels
plot(hac_result, labels = row.names(numeric_data), main = "HAC Analysis of Price and Neighbourhood Groups",
     xlab = "Listings", ylab = "Distance", sub = NULL)

# Adding labels for 'price' and 'neighbourhood_group'
text(hac_result$merge[, 1], -1, labels = numeric_data$price[hac_result$order], col = "blue", srt = 90)
text(hac_result$merge[, 2], -1, labels = numeric_data$neighbourhood_group[hac_result$order], col = "red", srt = 90)

# Set seed for reproducibility
set.seed(20)

# Randomly sample 1% of the observations
sampled_data <- airbnb_model[sample(nrow(airbnb_model), nrow(airbnb_model) * 0.001), ]

# Subset dataset to include only numeric columns
numeric_data <- sampled_data[, c("price", "neighbourhood_group")]

# Handle missing values if necessary
numeric_data <- na.omit(numeric_data)

# Perform hierarchical clustering with Euclidean distance
hac_result <- hclust(dist(numeric_data, method = "minkowski"), method = "complete")

# Plot dendrogram with labels
plot(hac_result, labels = row.names(numeric_data), main = "HAC Analysis of Price and Neighbourhood Groups (Euclidean Distance)",
     xlab = "Listings", ylab = "Distance", sub = NULL)

# Adding labels for 'price' and 'neighbourhood_group'
text(hac_result$merge[, 1], -1, labels = numeric_data$price[hac_result$order], col = "blue", srt = 90)
text(hac_result$merge[, 2], -1, labels = numeric_data$neighbourhood_group[hac_result$order], col = "red", srt = 90)

# Set seed for reproducibility
set.seed(20)

# Assuming 'airbnb_model' is your dataset, make sure to load it or create it if not done already

# Randomly sample 1% of the observations
sampled_data <- airbnb_model[sample(nrow(airbnb_model), nrow(airbnb_model) * 0.1), ]

# Subset dataset to include only numeric columns
numeric_data <- sampled_data[, c("price", "neighbourhood_group", "number_of_reviews")]

# Handle missing values if necessary
numeric_data <- na.omit(numeric_data)

# Train-test split
set.seed(123) # for reproducibility
train_index <- sample(1:nrow(numeric_data), 0.7 * nrow(numeric_data)) # 70% train data
train_data <- numeric_data[train_index, ]
test_data <- numeric_data[-train_index, ]

# Train SVM model
svm_model <- svm(price ~ neighbourhood_group, data = train_data)

# Predict on test data
predictions <- predict(svm_model, test_data)

# Evaluate the model
accuracy <- sum(predictions == test_data$price) / length(predictions)

print(paste("Accuracy:", accuracy))

# Set seed for reproducibility
set.seed(20)

# Assuming 'airbnb_model' is your dataset, make sure to load it or create it if not done already

# Randomly sample 1% of the observations
sampled_data <- airbnb_model[sample(nrow(airbnb_model), nrow(airbnb_model) * 0.1), ]

# Subset dataset to include relevant features
numeric_data <- sampled_data[, c("price", "reviews_per_month", "neighbourhood_group")]

# Handle missing values if necessary
numeric_data <- na.omit(numeric_data)

# Train-test split
set.seed(123) # for reproducibility
train_index <- sample(1:nrow(numeric_data), 0.7 * nrow(numeric_data)) # 70% train data
train_data <- numeric_data[train_index, ]
test_data <- numeric_data[-train_index, ]

# Train Decision Tree model
decision_tree_model <- rpart(price ~ reviews_per_month + neighbourhood_group, data = train_data)

# Visualize the decision tree (optional)
plot(decision_tree_model)
text(decision_tree_model, pretty = 0.1, cex = 0.6)

# Predict on test data
predictions <- predict(decision_tree_model, test_data)

# Evaluate the model
mae <- mean(abs(predictions - test_data$price))
print(paste("Mean Absolute Error (MAE) for Decision Tree:", mae))


# Train Decision Tree model with increased complexity
decision_tree_model <- rpart(reviews_per_month ~ price + neighbourhood_group, data = train_data,
                             control = rpart.control(cp = 0.001, minsplit = 10, minbucket = 5))

# Visualize the decision tree (optional)
plot(decision_tree_model)
text(decision_tree_model, pretty = 0.1, cex = 0.6)

# Predict on test data
predictions <- predict(decision_tree_model, test_data)

# Evaluate the model
mae <- mean(abs(predictions - test_data$price))
print(paste("Mean Absolute Error (MAE) for Decision Tree:", mae))

# Set seed for reproducibility
set.seed(20)

# Assuming 'airbnb_model' is your dataset, make sure to load it or create it if not done already

# Randomly sample 10% of the observations
sampled_data <- airbnb_model[sample(nrow(airbnb_model), nrow(airbnb_model) * 0.1), ]

# Subset dataset to include relevant features
numeric_data <- sampled_data[, c("price", "reviews_per_month", "neighbourhood_group")]

# Handle missing values if necessary
numeric_data <- na.omit(numeric_data)

# Train-test split
set.seed(123) # for reproducibility
train_index <- sample(1:nrow(numeric_data), 0.7 * nrow(numeric_data)) # 70% train data
train_data <- numeric_data[train_index, ]
test_data <- numeric_data[-train_index, ]

# Train Decision Tree model
decision_tree_model <- rpart(price ~ reviews_per_month + neighbourhood_group, data = train_data)

# Visualize the decision tree (optional)
plot(decision_tree_model)
text(decision_tree_model, pretty = 0.1, cex = 0.6)

# Predict on test data
predictions <- predict(decision_tree_model, test_data)

# Evaluate the model
mae <- mean(abs(predictions - test_data$price))
print(paste("Mean Absolute Error (MAE) for Decision Tree:", mae))


# Train Decision Tree model with increased complexity
decision_tree_model <- rpart(price ~ reviews_per_month + neighbourhood_group, data = train_data,
                             control = rpart.control(cp = 0.001, minsplit = 10, minbucket = 5))

# Visualize the decision tree (optional)
plot(decision_tree_model)
text(decision_tree_model, pretty = 0.1, cex = 0.6)

# Predict on test data
predictions <- predict(decision_tree_model, test_data)

# Evaluate the model
mae <- mean(abs(predictions - test_data$price))
print(paste("Mean Absolute Error (MAE) for Decision Tree:", mae))

# Calculate the average price
average_price <- mean(numeric_data$price)
print(paste("Average Price:", average_price))
# Calculate the range of prices
price_range <- range(numeric_data$price)
min_price <- price_range[1]
max_price <- price_range[2]

print(paste("Minimum Price:", min_price))
print(paste("Maximum Price:", max_price))

# Assuming 'airbnb_model' is your dataset, make sure to load it or create it if not done already

# Select relevant features for clustering
cluster_data <- airbnb_model[, c("price", "number_of_reviews", "neighbourhood_group")]

# Handle missing values if necessary
cluster_data <- na.omit(cluster_data)

# Convert 'neighbourhood_group' to a factor if it's not already
cluster_data$neighbourhood_group <- as.factor(cluster_data$neighbourhood_group)

# Convert 'price' and 'number_of_reviews' to numeric if they're not already
cluster_data$price <- as.numeric(cluster_data$price)
cluster_data$number_of_reviews <- as.numeric(cluster_data$number_of_reviews)

# Check if there are any non-numeric values in 'price' and 'number_of_reviews'
if (any(is.na(cluster_data$price)) || any(is.na(cluster_data$number_of_reviews))) {
  stop("Some values in 'price' or 'number_of_reviews' are not numeric.")
}

# Standardize the features
standardized_data <- scale(cluster_data[, c("price", "number_of_reviews")])

# Determine the optimal number of clusters using the elbow method
wss <- numeric(20)
for (i in 1:20) {
  kmeans_model <- kmeans(standardized_data, centers = i, nstart = 25)
  wss[i] <- sum(kmeans_model$withinss)
}

# Plot the elbow plot
plot(1:20, wss, type = "b", xlab = "Number of Clusters", ylab = "Within-cluster Sum of Squares",
     main = "Elbow Method for Determining Number of Clusters")

# From the plot, determine the optimal number of clusters based on the elbow point

# Perform k-means clustering with the optimal number of clusters
optimal_clusters <- 5  # Example: set based on the elbow point from the plot
kmeans_model <- kmeans(standardized_data, centers = optimal_clusters, nstart = 25)

# Add cluster labels to the original dataset
cluster_data$cluster <- kmeans_model$cluster

# Visualize the clusters using a scatter plot
ggplot(cluster_data, aes(x = price, y = number_of_reviews, color = factor(cluster))) +
  geom_point() +
  labs(title = "K-Means Clustering of Airbnb Rental Properties",
       x = "Price", y = "Number of Reviews", color = "Cluster") +
  theme_minimal()




## Cluster Analysis

airbnb_cluster <- read.csv("C:\\Users\\gsgro\\OneDrive\\Desktop\\Syr_MSBA\\Term 3\\Information Visualization\\Final Project\\new_york_listings_2024.csv"
                           , stringsAsFactors = TRUE
                           , header = TRUE)
str(airbnb_cluster)
(sum(is.na(airbnb_cluster))) 

# Remove columns 1(id), 3 (host_id) and 4 (host_name)
airbnb_cluster <- airbnb_cluster[, -c(3:4)] 
airbnb_cluster <- airbnb_cluster[, -c(1)] 

# Removed column 11 (license) 
airbnb_cluster <- airbnb_cluster[, -c(11)] 

# Convert char features to factors 
airbnb_cluster$room_type= factor(airbnb_cluster$room_type) 
airbnb_cluster$bedrooms= factor(airbnb_cluster$bedrooms) 
airbnb_cluster$baths= factor(airbnb_cluster$baths) 

head(airbnb_cluster)

#associatve rule mining analysis
#preparing dataset so it is in transaction format

transactions <- read.transactions("C:\\Users\\gsgro\\OneDrive\\Desktop\\Syr_MSBA\\Term 3\\Information Visualization\\Final Project\\new_york_listings_2024.csv", sep = ",", format = "basket")

#run the apriori algorithm 
rules <- apriori(transactions, parameter = list(support = 0.1, confidence = 0.5))
summary(rules)
inspect(rules[1:10])

#rule 1

airbnb_cluster = apriori(airbnb_cluster, parameter = list(supp = 0.001, conf = 0.9, maxlen =3))
options(digits=2)
inspect(airbnb_cluster[1:40])
rulesByLift <- head(sort(airbnb_cluster, by="lift"), 5)
plot(rulesByLift, method = "graph", engine = "interactive")
inspect(rulesByLift)

#rule 2, changing confidence to 1
(library(tcltk))
airbnb_cluster = apriori(airbnb_cluster, parameter = list(supp = 0.001, conf = 1, maxlen =3))
options(digits=2)
inspect(airbnb_cluster[1:40])
rulesByLift2 <- head(sort(airbnb_cluster, by="lift"), 5)
plot(rulesByLift2, method = "graph", engine = "interactive")
inspect(rulesByLift2)

#rule 3, Changing support > 0.001 to 0.01

(library(tcltk))
airbnb_cluster = apriori(airbnb_cluster, parameter = list(supp = 0.01, conf = 1, maxlen =3))
options(digits=2)
inspect(airbnb_cluster[1:40])
rulesByLift3 <- head(sort(airbnb_cluster, by="lift"), 5)
plot(rulesByLift3, method = "graph", engine = "interactive")
inspect(rulesByLift3)

