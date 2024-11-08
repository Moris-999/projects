##Our objective is to find a way to estimate the value in the "charges" column using the values in the other columns. if we can do so for the historical data, then we should able to estimate charges for new customers too, simply by asking for information like their age, sex, BMI, no. of children, smoking habits and region.

#### import R libraries
library(dplyr)
library(tidyverse)
library(readr)
library(reshape2)

### setting working directory
setwd("C:/Users/Administrator/Desktop/New folder/r program")

###load the dataset
medical<-read_csv("Health_insurance.csv")
medical

###Explore data
str(medical)

##summary
summary(medical)

### checking missing values
colSums(is.na(medical))

#Plot some distributions to understand variables better of each column
ggplot(medical, aes(x = charges)) +
  geom_histogram(fill = "red", color = "black", bins = 30) +
  labs(title = "Distribution of Charges",
       x = "Charges",
       y = "Count")

ggplot(medical,aes(x=age))+
  geom_histogram(fill="red",color="black",bins = 30)+
  labs(title = "Distribution of Age",
       x="Age",
       y="count")

ggplot(medical,aes(x=bmi))+
  geom_histogram(fill="red",color="black",bins=30)+
  labs(title = "Distribution of bmi",
       x= "bmi",
       y="count")

ggplot(medical,aes(x=children))+
  geom_histogram(fill="red",color="black",bins = 30)+
  labs(title = "Distribution of children",
       x="sex",
       y="count")

###checking distribution between male and female
ggplot(data = medical ,aes(sex))+
  geom_bar()



##create scatterplot  distribution of charges

ggplot(medical, aes(x = age, y = charges)) +
  geom_point(color = "red", alpha = 0.5) +  # Scatter plot of age vs. charges
  scale_fill_gradient(low = "white", high = "red") +  # Only needed if you use a `fill` aesthetic
  labs(title = "Age vs. Charges",
       x = "Age",
       y = "Charges")


ggplot(medical, aes(x = sex, y = charges)) +
  geom_point(color = "red", alpha = 0.5) +  # Scatter plot of age vs. charges
  scale_fill_gradient(low = "white", high = "red") +  # Only needed if you use a `fill` aesthetic
  labs(title = "sex vs. Charges",
       x = "sex",
       y = "Charges")

ggplot(medical, aes(x = bmi, y = charges)) +
  geom_point(color = "red", alpha = 0.5) +  # Scatter plot of age vs. charges
  scale_fill_gradient(low = "white", high = "red") +  # Only needed if you use a `fill` aesthetic
  labs(title = "bmi vs. Charges",
       x = "bmi",
       y = "Charges")

ggplot(medical, aes(x = children, y = charges)) +
  geom_point(color = "red", alpha = 0.5) +  # Scatter plot of age vs. charges
  scale_fill_gradient(low = "white", high = "red") +  # Only needed if you use a `fill` aesthetic
  labs(title = "chidren vs. Charges",
       x = "children",
       y = "Charges")

ggplot(medical, aes(x = smoker, y = charges)) +
  geom_point(color = "red", alpha = 0.5) +  # Scatter plot of age vs. charges
  scale_fill_gradient(low = "white", high = "red") +  # Only needed if you use a `fill` aesthetic
  labs(title = "Smoker vs. Charges",
       x = "smoker",
       y = "Charges")

ggplot(medical, aes(x = region, y = charges)) +
  geom_point(color = "red", alpha = 0.5) +  # Scatter plot of age vs. charges
  scale_fill_gradient(low = "white", high = "red") +  # Only needed if you use a `fill` aesthetic
  labs(title = "region vs. Charges",
       x = "region",
       y = "Charges")

### create a heatmap

# Select numeric columns only
numeric_columns <- medical %>% select(age, bmi, children, charges)

# Calculate correlation
corr_matrix <- cor(numeric_columns, use = "complete.obs")
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

##### create a linear regression

medical$sex <- as.factor(medical$sex)
medical$smoker <- as.factor(medical$bmi)
medical$region <- as.factor(medical$region)

# Fit the linear model
model <- lm(charges ~ age + bmi + children + sex + smoker + region, data = medical)

### summary the model

summary(model)

# Predict charges
medical$predicted_charges <- predict(model, medical)

# Plot actual vs. predicted values
ggplot(medical, aes(x = predicted_charges, y = charges)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Actual vs Predicted Charges",
       x = "Predicted Charges",
       y = "Actual Charges") +
  theme_minimal()


