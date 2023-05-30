# Prediction using unsupervised ML
# For the given iris data set, predict the optimum number of clusters and 
# represent it visually. 

# Download required packages/libraries ====
library(readr)
library(tidyverse)
library(dplyr)
library(stats)
library(factoextra)
library(cluster)

# Download data ====
data1 <- read.csv("/Users/macbookair/Desktop/GRIP/Task2/iris.csv")
glimpse(data1)

# Data pre processing ====
# remove Id and species names columns 
data1.1 <- data1 %>% select(SepalLengthCm, SepalWidthCm, PetalLengthCm, PetalWidthCm)


## Data exploration - visualization with correlation matrix ====
cor_matrix_data <- data1.1 %>% 
  select(SepalLengthCm, SepalWidthCm, PetalLengthCm, PetalWidthCm)
cor(cor_matrix_data)

## Scale data ====
# no additional scaling is required as all values are in cm


# Determine the optimal number of clusters ====
# this can be done using several methods. We will try all of them to ensure the 
# number of clusters is correct

## Elbow method ====
fviz_nbclust(data1.1, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)
# The location of a bend (knee) in the plot is generally considered as an indicator 
# of the appropriate number of clusters - in this case it's 3

## Average Silhouette Method ====
fviz_nbclust(data1.1, kmeans, method = "silhouette")
# this method indicates 2 is the optimal number of clusters

## Gap Statistic Method =====
gap_stat <- clusGap(data1.1, kmeans, K.max = 10, B = 500)
fviz_gap_stat(gap_stat)
# according to this method, the optimal number of clusters is 4

# taking an average from methods above, we can assume k = 3



# Compute k-means clustering ====
k <- 3  # optimal number of clusters determined in the previous step
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(data1.1, centers = k, nstart = 25) 
# nstart = 25 means that R will try 25 different random starting assignments 
# and then select the best results corresponding to the one with the lowest within cluster variation. 

print(kmeans_result)
# K-means clustering with 3 clusters of sizes 50, 38, 62

# Analyze and interpret the clustering results ====
## Access the cluster assignments for each data point ====
cluster_labels <- kmeans_result$cluster

## Access the coordinates of the cluster centroids ====
centroids <- kmeans_result$centers

# Visualize the clustering results ====
fviz_cluster(kmeans_result, data1.1,
             main = "Iris cluster plot")


# How to determine what cluster represents what species ====
cluster_labels <- kmeans_result$cluster
species_labels <- data1$Species
contingency_table <- table(cluster_labels, species_labels) 
  # cross-tabulation between the cluster_labels and species_labels variables using 
  # the table() function

print(contingency_table)
# there is some overlap between clusters, but we can still assess the most frequent species
# in each cluster and under this assumption determine cluster/species representation
# 1 - iris-setosa, 2 - iris-virginica,  3 - iris-versicolor


