### Predict global health issues able to acess clean water.
library(dplyr)
library( tidyverse)
library(reshape2)
library(ggplot2)
library(readr)

###set working directory
setwd("C:/Users/Administrator/Desktop/New folder/Global health")

###loading the dataset
GH<-read_csv("global_health.csv")
GH
###Global health dataset has 1880 rows and 29 columns.

### explore the dataset
str(GH)

###the str has two attribute Col_caracter and col_double

### summary
summary(GH)

## checking missing column
colSums(is.na(GH))

## dropping missing values

GH1 <- GH %>%
  drop_na(
    Fertility_Rate, Water_Access_Percent, Unemployment_Rate, Sanitary_Expense_Per_GDP, Life_Expectancy,
    Life_Expectancy_Female, Life_Expectancy_Male, Infant_Deaths, GDP_Per_Capita, Hospital_Beds_Per_1000, Alcohol_Consumption_Per_Capita,
    Immunization_Rate, Sanitary_Expense_Per_Capita, CO2_Exposure_Percent, Air_Pollution,
    Labour_Force_Total, Tuberculosis_Per_100000, Suicide_Rate_Percent,
    Obesity_Rate_Percent, Underweight_Rate_Percent, Overweight_Rate_Percent, Safe_Water_Access_Percent
  )
### we have no missing values in this dataset

###Checking for distribution for Life expentancy

ggplot(GH1,aes(Life_Expectancy))+
  geom_histogram(fill = "red", color = "black", bins = 30) +
  labs(title = "Distribution of Life Expentacy",
       x = "Average",
       y = "Count")

###checking life expentancy of female

ggplot(GH1,aes(Life_Expectancy_Female))+
  geom_histogram(fill = "red", color = "black", bins = 30) +
  labs(title = "Distribution of Life Expentacy for female",
       x = "Expentancy",
       y = "Count")

ggplot(GH1,aes(Life_Expectancy_Male))+
  geom_histogram(fill = "red", color = "black", bins = 30) +
  labs(title = "Distribution of Life Expentacy for male",
       x = "Expentancy",
       y = "Count") 

  
ggplot(GH1,aes(Underweight_Rate_Percent))+
  geom_histogram(fill = "red", color = "black", bins = 30) +
  labs(title = "Distribution of Underweight Rate",
       x = "Expentancy",
       y = "Count")
         
ggplot(GH1,aes(Overweight_Rate_Percent))+
  geom_histogram(fill = "red", color = "black", bins = 30) +
  labs(title = "Distribution of Overweight",
       x = "Expentancy",
       y = "Count") 

ggplot(GH1,aes(Safe_Water_Access_Percent))+
  geom_histogram(fill = "red", color = "black", bins = 30) +
  labs(title = "Distribution of Safe Water Access percent",
       x = "Expentancy",
       y = "Count")

###select numeric column
Global_column<-GH1%>%select("Life_Expectancy", "Life_Expectancy_Female", "Life_Expectancy_Male","Underweight_Rate_Percent", "Overweight_Rate_Percent","Safe_Water_Access_Percent")

# Calculate correlation
corr_matrix <- cor(Global_column, use = "complete.obs")
corr_melted <- melt(corr_matrix)

ggplot(data = corr_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
                       limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  labs(title = "Correlation Heatmap",
       x = " Distribution",
       y = " plot") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

###evaluate model
set.seed(123)  # For reproducibility
sample_indices <- sample(1:nrow(GH1), size = 0.8 * nrow(GH1))
train_data <- GH1[sample_indices, ]
test_data <- GH1[-sample_indices, ]

#  predictor variables are numeric

GH1$Life_Expectancy <- as.numeric(GH1$Life_Expectancy)
GH1$Life_Expectancy_Female <- as.numeric(GH1$Life_Expectancy_Female)
 GH1$Underweight_Rate_Percent<- as.numeric(GH1$Underweight_Rate_Percent)
 GH1$Overweight_Rate_Percent<-as.numeric(GH1$Overweight_Rate_Percent)
 
# Fit the linear regression model
model <- lm(Safe_Water_Access_Percent ~ Life_Expectancy + 
              Life_Expectancy_Female + 
              Life_Expectancy_Male + 
              Underweight_Rate_Percent + 
              Overweight_Rate_Percent, data = GH1)

#### Display summary

summary(model)

# Predict clean water using the fitted model
predicted_Clean <- predict(model, newdata = GH1)

# Add the predictions to the original dataset for comparison
 GH1$predicted_Clean<-predicted_Clean
 
 # View the dataset with the predicted prices
 head(GH1)
 
 ggplot(GH1, aes(x = Safe_Water_Access_Percent, y =predicted_Clean )) +
   geom_point(color = "blue", alpha = 0.5) +
   geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
   labs(title = "Actual vs. Predictedition",
        x = "prediction",
        y = "Predicted Clean water") +
   theme_minimal()
  
## evalute model accuracy
 mean((GH1$predicted_Clean - GH1$Safe_Water_Access_Percent)^2)  # MSE
 mean(abs(GH1$predicted_Clean - GH1$Safe_Water_Access_Percent)) # MAE
 
 
 
 
