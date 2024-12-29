### create linear regression to predict sales and profit  merging

###load in the libraries
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readr)
library(reshape2)

### set working directory

setwd("C:/Users/Administrator/Desktop/New folder/Retail program")

###load data

SP<-read_csv("SampleSuperstore.csv")

#### the data has 9994 rows and 13 columns

##explore data
str(SP)

### it has two charcter and numeric

###Summary
summary(SP)

### checking missing values
colSums(is.na(SP))

### it has no missing values

### checking distribution sale

ggplot(SP,aes(Sales))+
  geom_histogram(fill = "red", color = "black", bins = 30) +
  labs(title = "Distribution of sales",
       x = "Distri",
       y = "Count")

###  check distribution for Discount
ggplot(SP,aes(Discount))+
  geom_histogram(fill = "red", color = "black", bins = 30) +
  labs(title = "Distribution of Discount",
       x = "Distri",
       y = "Count")

### check distribution profit
ggplot(SP,aes(Profit))+
  geom_histogram(fill = "red", color = "black", bins = 30) +
  labs(title = "Distribution of profit",
       x = "Distri",
       y = "Count")

###evaluate model
set.seed(123)  # For reproducibility
sample_indices <- sample(1:nrow(SP), size = 0.8 * nrow(SP))
train_data <- SP[sample_indices, ]
test_data <- SP[-sample_indices, ]

#  predictor variables are numeric
SP$Sales <- as.numeric(SP$Sales)
SP$Discount <- as.numeric(SP$Discount)


# Fit the linear regression model
model <- lm(Profit~ Sales+ 
              Discount,data=SP )

###    Display summary of the model

summary(model)

###predict profit
predicted_Profit <- predict(model, newdata = SP)

# Add the predictions to the original dataset for comparison
SP$predicted_Profit<-predicted_Profit

# View the dataset with the predicted prices
head(SP)

ggplot(SP, aes(x = Profit, y =predicted_Profit )) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs. Predictedition",
       x = "prediction",
       y = "Predicted Profit") +
  theme_minimal()

## evalute model accuracy
mean((SP$predicted_Profit- SP$Profit)^2)  # MSE
mean(abs(SP$predicted_Profit - SP$Profit)) # MAE


