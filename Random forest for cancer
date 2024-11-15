# Load required libraries
library(dplyr)
library(tidyverse)
library(readr)
library(reshape2)
library(readxl)
library(randomForest)

# Set working directory
setwd("C:/Users/Administrator/Desktop/New folder/Cancer program")

# Load the dataset
Cv <- read_excel("Cancer.xlsx")
print(Cv)

# Explore the data
str(Cv)
summary(Cv)

# Check for missing values in each column
missing_values <- colSums(is.na(Cv))
print(missing_values)

# Fill or handle missing values
Cv <- Cv %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Rename problematic column (if needed)
names(Cv) <- make.names(names(Cv))  # Converts names to valid syntactic names
# Ensure column names are valid
print(names(Cv))

# Add a random classifier column
set.seed(123)  # For reproducibility
Cv <- Cv %>%
  mutate(Smoking_prevalence_male = sample(c("Low", "Moderate", "High"), 
                                          size = nrow(Cv), 
                                          replace = TRUE))

# Preview the modified dataset
head(Cv)

# Convert the target variable to a factor
Cv$Smoking_prevalence_male <- as.factor(Cv$Smoking_prevalence_male)

# Train-test split (70-30 split)
set.seed(123)
train_indices <- sample(seq_len(nrow(Cv)), size = 0.7 * nrow(Cv))
train_data <- Cv[train_indices, ]
test_data <- Cv[-train_indices, ]

# Random Forest Classifier
set.seed(123)
rf_model <- randomForest(Smoking_prevalence_male ~ ., 
                         data = train_data, 
                         ntree = 100, 
                         importance = TRUE)

# Model summary
print(rf_model)

# Feature importance
importance(rf_model)
varImpPlot(rf_model)

# Predictions on the test set
predictions <- predict(rf_model, test_data)

# Confusion matrix
confusion_matrix <- table(Predicted = predictions, Actual = test_data$Smoking_prevalence_male)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", round(accuracy, 2)))

#### accuracy of the result is 37% for male prevalence 
