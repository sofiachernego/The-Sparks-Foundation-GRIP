# Prediction using decision tree algorithm ====
# create a decision tree classifier and visualize it graphically
# predict the species of an iris flower (based on sepal and petal data)
# purpose: if we feed any new data to the classifier, it should predict so accordingly


# Download packages/libraries ====
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(rsample)
library(magrittr)
library(caret)

# Download data ====
tree <- read.csv("/Users/macbookair/Desktop/GRIP/Task2/iris.csv")

# Data cleaning ====
# Remove ID column
tree <- tree %>% select(-Id)

# EDA ====
str(tree)

## Univariate analysis ====
### Sepal length ====
# Sepal length histogram
ggplot(tree, aes(SepalLengthCm))+
  geom_histogram()+
  ggtitle("Sepal length distribution")     #an outlier at 8cm? 


# Sepal length box plot
ggplot(tree, aes(SepalLengthCm))+
  geom_boxplot()+
  ggtitle("Sepal length distribution")
  

### Sepal width ====
# Sepal width histogram
ggplot(tree, aes(SepalWidthCm))+
  geom_histogram()+
  ggtitle("Sepal width distribution")

# Sepal width box plot
ggplot(tree, aes(SepalWidthCm))+
  geom_boxplot()+
  ggtitle("Sepal width distribution")


### Petal length ====
# Petal length histogram
ggplot(tree, aes(PetalLengthCm))+
  geom_histogram()+
  ggtitle("Petal length distribution")

# Petal length box plot
ggplot(tree, aes(PetalLengthCm))+
  geom_boxplot()+
  ggtitle("Petal length distribution")


### Petal width ====
# Petal width histogram 
ggplot(tree, aes(PetalWidthCm))+
  geom_histogram()+
  ggtitle("Petal wifth distribution")

# Petal width box plot
ggplot(tree, aes(PetalWidthCm))+
  geom_boxplot()+
  ggtitle("Petal wifth distribution")

## Panel with multiple histograms ====
# Create a data frame in the long format
tree_long <- tree %>%
  pivot_longer(cols = !Species,
               names_to = "Variable",
               values_to = "Value")

# Create histogram panel using facet wrap 
ggplot(tree_long, aes(x = Value)) +
  geom_histogram(bins = 30, color = "black", fill = "lightblue") +
  facet_wrap(~Variable, scales = "free", ncol = 4) +
  labs(title = "Histograms of Numeric Variables in Iris Dataset",
       x = "Value",
       y = "Frequency") +
  theme_minimal()

## Bivariate analysis ====
### Petal length and width relationship ====
ggplot(tree, aes(PetalLengthCm, PetalWidthCm, color = Species)) + 
  geom_point()+
  ggtitle("The relationship between petal length and width")

# if petal length <2cm -> iris setosa
# if petal length 2.5-4.5cm -> iris versicolor
# if petal length 4.5-6.5cm -> iris virginica

### Sepal length and width relationship =====
ggplot(tree, aes(SepalLengthCm, SepalWidthCm, color = Species)) + 
  geom_point()+
  ggtitle("The relationship between sepal length and width")


# Decision tree modelling ====
## Splitting data into training and test sets ====
set.seed(123)
tree_data_split <- initial_split(tree, prop = 0.8)
tree_train_data <- training(data_split)
tree_test_data <- testing(data_split)

## Train decision tree model ====
# Fit the model onto train data
tree_model <- rpart(Species ~ ., tree_train_data, method = "class")

## Make predictions on training data ====
tree_train_predictions <- predict(tree_model, tree_train_data, type = "class")

## Make predictions on test data ====
tree_test_predictions <- predict(tree_model, tree_test_data, type = "class")

## Visualize the model ====
rpart.plot(tree_model, type = 4, cex = 0.8, box.palette = "auto")

## Visualize predictions ====

## Evaluate model performance ====
### Accuracy of training and test predictions

# Calculate accuracy on training data
train_accuracy <- sum(tree_train_predictions == tree_train_data$Species) / nrow(tree_train_data)
  # training accuracy = 93%

# Calculate accuracy on test data
test_accuracy <- sum(tree_test_predictions == tree_test_data$Species) / nrow(tree_test_data)
  # test accuracy = 96%

