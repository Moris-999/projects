### visualize comfirmired case , death rates and recovered cases
### build a model for this case

##load in the libraries
library(ggplot2)
library(tidyverse)
library(readr)
library(reshape2)

### set working directory
setwd("C:/Users/Administrator/Desktop/New folder/Covid USA")

### load dataset
covid<-read.csv("covid_19_data.csv")
covid

### explore data
str(covid)

### the dataset contain int , chr and numbers

## summary
summary(covid)

### checking missing values
colSums(is.na(covid))

## the dataset has no missing values

# Melt data for comparison
covid_long <- melt(covid[, c("Confirmed", "Deaths", "Recovered")])

# Plot all three distributions
ggplot(covid_long, aes(x = value, fill = variable)) +
  geom_histogram(color = "black", bins = 50, position = "identity", alpha = 0.5) +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Distribution of Confirmed, Deaths, and Recovered Cases",
       x = "Count",
       y = "Frequency") +
  theme_minimal()


# Select relevant columns
covid_numeric <- covid[, c("Confirmed", "Deaths", "Recovered")]

# Correlation matrix
cor(covid_numeric)

# Build the model
model <- lm(Deaths ~ Confirmed + Recovered, data = covid)

# Summary of the model
summary(model)

# Add predictions
covid$Predicted_Deaths <- predict(model, newdata = covid)

# Plot actual vs. predicted
ggplot(covid, aes(x = Deaths, y = Predicted_Deaths)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Actual vs Predicted Deaths",
       x = "Actual Deaths",
       y = "Predicted Deaths") +
  theme_minimal()

# Calculate RMSE
rmse <- sqrt(mean((covid$Deaths - covid$Predicted_Deaths)^2))
cat("Root Mean Squared Error (RMSE):", rmse, "\n")


