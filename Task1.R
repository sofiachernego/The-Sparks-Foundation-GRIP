library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)

# Prediction using supervised ML ====
# What would be the predicted score if a student studies 9.25hr/day?

url <- "https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv"
data <- read.csv(url)

## Data exploration ====
### Structure of the data set ====
str(data)

### Summary statistics ==== 
summary(data)

### Initial data visualization =====
ggplot(data, aes(Hours, Scores))+   # independent variable on x-axis
  geom_point() +
  labs(title = "The relationship between hours studied and scores")



## Linear regression ====
### Fit the linear regression model ====
model <- lm(Scores ~ Hours, data)

### Predict the score for a student studying 9.25 hours per day ====
new_data <- data.frame(Hours = 9.25)
predicted_score <- predict(model, newdata = new_data)

### Print the predicted score ====
print(predicted_score)

## Evaluate the model ==== 
### Model's summary statistics ====
summary(model)

### Check the residuals and residuals plot ====
residuals <- residuals(model)
plot(model, which = 1)  # Residuals vs Fitted plot
plot(model, which = 2)  # Normal Q-Q plot

### Visualize the regression line on the scatter plot ====
ggplot(data, aes(Hours, Scores))+   # independent variable on x-axis
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red")+
  labs(title = "Fitted linear regression model")



## Option 2: training the algorithm ====
library(caret)
library(magrittr)

### Divide the data into test and training sets ====
set.seed(123)
training.samples <- data$Hours %>%   
  createDataPartition(p = 0.8, list = FALSE)     
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]

### Fit the linear regression model ====
model1 <- lm(Scores ~ Hours, data = train.data)

### Check model summary and diagnostics ====
summary(model1)
plot(model1) # diagnostic plots 

### Predict the scores using the test data ====
predicted_score1 <- predict(model1, newdata = new_data)
print(predicted_score1)

