### visualize comfirmired case , death rates and recovered cases
### build a  model for this case

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

top_countries <- covid %>%
  group_by(Country.Region) %>%
  summarise(Deaths = sum(Deaths, na.rm = TRUE)) %>%
  arrange(desc(Deaths)) %>%
  slice(1:25)

ggplot(top_countries, aes(x = reorder(Country.Region, Deaths), y = Deaths, fill = Country.Region)) +
  geom_col(show.legend = FALSE) +
  theme_light() +
  labs(x = "Country", y = "Deaths") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 14))

### usa is leading with number of deaths with Netherlands with the list top countries
### 25

##Top countries with confirmed cases
top_countries <- covid %>%
  group_by(Country.Region) %>%
  summarise(Confirmed = sum(Confirmed, na.rm = TRUE)) %>%
  arrange(desc(Confirmed)) %>%
  slice(1:25)


ggplot(top_countries, aes(x = reorder(Country.Region,Confirmed), y = Confirmed, fill = Country.Region)) +
  geom_col(show.legend = FALSE) +
  theme_light() +
  labs(x = "Country", y = "Confirmed Cases") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 14))

###top 25 countries usa is still leading with confirmed cases and belgium at the end

  ### top 25 countries with recovered cases
top_countries <- covid %>%
  group_by(Country.Region) %>%
  summarise(Recovered = sum(Recovered, na.rm = TRUE)) %>%
  arrange(desc(Recovered)) %>%
  slice(1:25)

ggplot(top_countries, aes(x = reorder(Country.Region,Recovered), y = Recovered, fill = Country.Region)) +
  geom_col(show.legend = FALSE) +
  theme_light() +
  labs(x = "Country", y = "Recovered Cases") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 14))
### India has a higher number of people how have recovered from covid

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


