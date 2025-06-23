###create a model that show unemployment rate after finishing  secondary school.
## 1. completion_rate_secondary_male
## 2. completion_rate_secondary_female
## 3. unemployment_rate

### import labraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readr)
library(reshape2)

## set working directory
setwd("C:/Users/Administrator/Desktop/New folder/Global Education")

###import dataset
GD<-read_csv("Global_Education.csv")

### Global Education has 202 rows and 29 columns

###explore data

str(GD)
### it has two col_character and col_double

summary(GD)

###check missing values
colSums(is.na(GD))
### no missing values

## Data visulazion 

### check distribution of Global Education completion rate
ggplot(GD, aes(x =Completion_Rate_Primary_Male )) +
  geom_histogram(fill = "red", color = "black", bins = 30) +
  labs(title = "Distribition of completion rate primary Male",
       x = "Average",
       y = "Count")

### check distribution of Global Education completion rate
ggplot(GD, aes(x =Completion_Rate_Primary_Female )) +
  geom_histogram(fill = "red", color = "black", bins = 30) +
  labs(title = "Distribition of completion rate primary female Male",
       x = "Average",
       y = "Count")

### check distribution of Global Education completion rate
ggplot(GD, aes(x =GD$Completion_Rate_Primary_Male )) +
  geom_histogram(fill = "red", color = "black", bins = 30) +
  labs(title = "Distribition of completion rate primary Male",
       x = "Average",
       y = "Count")

### check distribution of Global Education completion rate
ggplot(GD, aes(x =GD$Completion_Rate_Lower_Secondary_Male )) +
  geom_histogram(fill = "red", color = "black", bins = 30) +
  labs(title = "Distribition of completion rate primary Male",
       x = "Average",
       y = "Count")


### check distribution of Global Education completion rate
ggplot(GD, aes(x =GD$Completion_Rate_Lower_Secondary_Female )) +
  geom_histogram(fill = "red", color = "black", bins = 30) +
  labs(title = "Distribition of completion rate primary  FeMale",
       x = "Average",
       y = "Count")


### check distribution of Global Education completion rate

ggplot(GD, aes(x =GD$Completion_Rate_Upper_Secondary_Male )) +
  geom_histogram(fill = "red", color = "black", bins = 30) +
  labs(title = "Distribition of completion rate upper secondary male",
       x = "Average",
       y = "Count")

### check distribution of Global Education completion rate
ggplot(GD, aes(x =GD$Completion_Rate_Upper_Secondary_Female )) +
  geom_histogram(fill = "red", color = "black", bins = 30) +
  labs(title = "Distribition of completion rate upper secondary female",
       x = "Average",
       y = "Count")

ggplot(GD, aes(x =Completion_Rate_Upper_Secondary_Male, y = Unemployment_Rate)) +
  geom_point(color = "red", alpha = 0.5) +  # Scatter plot 
  scale_fill_gradient(low = "white", high = "red") +  # Only needed if you use a `fill` aesthetic
  labs(title = "Completion_Rate vs. Completion_Rate_Upper_Secondary_Male",
       x = "Completion_Rate",
       y = "Unemployment_Rate")


ggplot(GD, aes(x =Completion_Rate_Upper_Secondary_Female, y = Unemployment_Rate)) +
  geom_point(color = "red", alpha = 0.5) +  # Scatter plot 
  scale_fill_gradient(low = "white", high = "red") +  # Only needed if you use a `fill` aesthetic
  labs(title = "Completion_Rate vs. Completion_Rate_Upper_Secondary Female",
       x = "Completion_Rate",
       y = "Unemployment_rate")
  


