### load the library
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(reshape2)
library(randomForest)# for Random Forest model
library(pROC)# for ROC curve and AUC
library(caret)# for data partitioning and evaluation
###setting working directory
setwd("C:/Users/Administrator/Desktop/New folder/diabetes 2")

###load in the dataset
DTB<-read_csv("diabetes_data set.csv")
DTB
####the dataset consit of 4303 rows and 29 columns

##explore diabetes data
str(DTB)
#### explore summary data
summary(DTB)

### checking missing columns
colSums(is.na(DTB))
### diabetes column has 4303 missing column.

### dropping diabetes column with nas
DTB1<-DTB%>%
  select_if(~ !any(is.na(.)))

# Drop rows with missing outcome variable
DTB1 <- DTB1 %>% drop_na(Diabetes)

# Convert Diabetes column to factor (binary classification)
DTB1$Diabetes <- as.factor(DTB1$Diabetes)

# Check class distribution
table(DTB1$Diabetes)

###view DTB1
DTB1
####  summary DTB1
summary(DTB1)

### visualize diabetes dataset
ggplot(DTB1,aes(Age))+
  geom_histogram(fill = "red", color = "black", bins = 30)+
  labs(title = " Age Distribution",
       x="Average",
       y="count")

##### check gender distribution
ggplot(DTB1,aes(Gender))+
  geom_histogram(fill="red",color= "black")+
  labs(title ="Gender Distribution",
       x="Average",
       y="count")

### checking  BMI  Distribution
ggplot(DTB1,aes(BMI))+
  geom_histogram(fill="red",color= "black",bins = 30)+
  labs(title = " BMI Distribution",
       x= "Average",
       y= "count")

### checking SBP (Systolic Blood Pressure)
ggplot(DTB1,aes(`SBP (Systolic Blood Pressure)`))+
  geom_histogram(fill="red",color= "black",bins = 30)+
  labs(title = "SBP (Systolic Blood Pressure)",
       x= "Average",
       y="count")
### checking DBP (Diastolic Blood Pressure)
ggplot(DTB1,aes(`DBP (Diastolic Blood Pressure)`))+
  geom_histogram(fill="red",color= "black",bins = 30)+
  labs(title = "DBP (Diastolic Blood Pressure)",
       x="Average",
       y=" count")

### checking FPG (Fasting Plasma Glucose)
ggplot(DTB1,aes(`FPG (Fasting Plasma Glucose)`))+
  geom_histogram(fill="red",color= "black",bins = 30)+
  labs(title = "FPG (Fasting Plasma Glucose)",
       x="Average",
       y="count")

### checking FFPG (Final Fasting Plasma Glucose)
ggplot(DTB1, aes(`FFPG (Final Fasting Plasma Glucose)`))+
  geom_histogram(fill="red",color= "black",bins = 30)+
  labs(title = "FFPG (Final Fasting Plasma Glucose)",
       x="Average",
       y="count")

#### cholesterol 
ggplot(DTB1,aes(Cholesterol))+
  geom_histogram(fill="red",color= "black",bins = 30)+
  labs(title = "Cholesterol",
       x="Average",
       y= "count")

### Triglyceride   
ggplot(DTB1,aes(Triglyceride))+
  geom_histogram(fill="red",color= "black",bins = 30)+
  labs(title = "Triglyceride",
       x="Average",
       y="count")


### HDL (High-Density Lipoprotein)
ggplot(DTB1,aes(`HDL (High-Density Lipoprotein)`))+
  geom_histogram(fill="red",color= "black",bins = 30)+
  labs(title = "HDL (High-Density Lipoprotein)",
       x="Average",
       y="count")

### LDL (Low-Density Lipoprotein)
ggplot(DTB1,aes(`LDL (Low-Density Lipoprotein)`))+
  geom_histogram(fill="red",color= "black",bins = 30)+
  labs(title = "LDL (Low-Density Lipoprotein)",
       x="Average",
       y="count")

### ALT (Alanine Aminotransferase) 
ggplot(DTB1,aes(`ALT (Alanine Aminotransferase)`))+
  geom_histogram(fill="red",color= "black",bins = 30)+
  labs(title = "ALT (Alanine Aminotransferase)",
       x="Average",
       y="count")

####  BUN (Blood urea nitrogen)
ggplot(DTB1,aes(`BUN (Blood urea nitrogen)`))+
  geom_histogram(fill="red",color= "black",bins = 30)+
  labs(title = "BUN (Blood urea nitrogen)",
       x="Average",
       y="count")

### CCR (Creatinine Clearance)

ggplot(DTB1,aes(`CCR (Creatinine Clearance)`))+
  geom_histogram(fill="red",color= "black",bins = 30)+
  labs(title = " CCR (Creatinine Clearance)",
       x="Average",
       y="count")

### Smoking Status
ggplot(DTB1,aes(DTB1$`Smoking Status: (1: Current Smoker, 2: Ever Smoker, 3: Never Smoker)`))+
  geom_histogram(fill="red",color= "black",bins = 30)+
  labs(title = "Smoking Status",
       x=" Average",
       y="count")

### Drinking status

ggplot(DTB1,aes(Drinking Status: (1: Current Drinker, 2: Ever Drinker, 3: Never Drinker)))+
  geom_histogram(fill="red",color= "black",bins = 30)+
  labs(title = "Drinking Status",
       x="Average",
       y=" count")

#### Family History of Diabetes: (1: Yes, 0: No)
ggplot(DTB1,aes(`Family History of Diabetes: (1: Yes, 0: No)`))+
  geom_histogram(fill="red",color= "black",bins = 30)+
  labs(title = "Family History of diabetes",
       x="Average",
       y="count")
# Clean column names
colnames(DTB1) <- make.names(colnames(DTB1))

# Confirm the 'Diabetes' column is still a factor
DTB1$Diabetes <- as.factor(DTB1$Diabetes)

# Split into train/test again
set.seed(123)
splitIndex <- createDataPartition(DTB1$Diabetes, p = 0.8, list = FALSE)
train_data <- DTB1[splitIndex, ]
test_data <- DTB1[-splitIndex, ]

# Train the Random Forest model
set.seed(123)
rf_model <- randomForest(Diabetes ~ ., data = train_data, ntree = 500, mtry = sqrt(ncol(train_data) - 1), importance = TRUE)

# Model summary
print(rf_model)

# Predict on test data
rf_pred <- predict(rf_model, newdata = test_data)

# Confusion matrix
confusionMatrix(rf_pred, test_data$Diabetes)

# Predict probabilities for ROC
rf_prob <- predict(rf_model, newdata = test_data, type = "prob")[, 2]

# ROC Curve and AUC
roc_obj <- roc(test_data$Diabetes, rf_prob)
plot(roc_obj, col = "blue", main = "ROC Curve - Random Forest")
auc(roc_obj)

# Variable Importance Plot
varImpPlot(rf_model, main = "Variable Importance (Random Forest)")
### Diabetes random forest accuracy  is 95%

