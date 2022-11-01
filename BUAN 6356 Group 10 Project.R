##Preparation
library(dplyr)
library(ggplot2)
library(tidyverse)
# install.packages("lubridate")
#install.packages("eeptools")
library(lubridate)

library(eeptools)
df <- read_csv('C:/Users/shahn/Documents/GitHub/Business-Analytics-in-R-Assignment/fraudTrain.csv', show_col_types = FALSE)

##Clean the data
df <- subset(df, select = -c(...1, cc_num, first, last, gender, street, zip, unix_time))

##Summary
summary(df)
sum(is.na(df))

##Analyze the relation among category, amount and fraud
purchase <- df[, c(3,4,15)]
colnames(purchase)
#Create histogram of category of merchants and number of frauds
ggplot(purchase, aes(x = category)) + geom_bar(aes(fill = is_fraud), position = "dodge") + theme_minimal() + labs(title = "Histogram of Category of Merchants and Number of Fraud", x = "Category", y = "Number of Fraud")
##Create boxplot of category of merchants and amount involved in the fraud
ggplot(purchase, aes(x = category, y = amt)) + geom_boxplot(aes(fill = is_fraud), position = "dodge") + theme_minimal() + labs(title = "Boxplot of Category of Merchants and Amount Involved in the Fraud", x = "Category", y = "Amount Involved in the Fraud")



##Create histogram of age of customers and quantity of frauds

# Converting the DoB column to a Date format
df$dob <- as.Date(df$dob)

# Getting age from DoB
df$Age <- floor(as.numeric(difftime(Sys.Date(), df$dob, units = "days"))/365.25)
age <- df[, c("Age", "is_fraud")] # Selecting only the Age and is_fraud columns
# Creating Bin for the Age Columns
age_bins=data.frame(age$Age,bin=cut(age$Age,c(0,1,2,3,4,5,6,7,8,9),include.lowest=TRUE))
ggplot(age, aes(x = Age)) + geom_histogram(aes(fill = is_fraud), position = "dodge") + theme_minimal() + labs(title = "Histogram of Age of Customers and Quantity of Fraud", x = "Age", y = "Quantity of Fraud")


##Create scatterplot of age of customers and amount involved in the fraud
ggplot(df, aes(x = Age, y = amt)) + geom_point(aes(color = is_fraud)) + theme_minimal() + labs(title = "Scatterplot of Age of Customers and Amount Involved in the Fraud", x = "Age", y = "Amount Involved in the Fraud")
