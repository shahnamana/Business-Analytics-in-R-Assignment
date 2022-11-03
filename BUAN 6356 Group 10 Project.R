##Preparation
library(dplyr)
library(ggplot2)
library(tidyverse)
# install.packages("lubridate")
#install.packages("eeptools")
# install.packages("writexl")
library(lubridate)
library("writexl")

library(eeptools)
df <- read_csv('C:/Users/shahn/OneDrive - The University of Texas at Dallas/BUAN 6359 Business Analytics with R Zhe Zang/Group Project/archive/fraudTrain.csv', show_col_types = FALSE)

##Clean the data
df <- subset(df, select = -c(...1, cc_num, first, last, gender, street, zip, unix_time))
# Filtering only the fraud transactions
df <- df %>% filter(is_fraud == 1)
##Summary
summary(df)
sum(is.na(df))

##Analyze the relation among category, amount and fraud
purchase <- df[, c(3,4,15)]
colnames(purchase)
#Create histogram of category of merchants and number of frauds
ggplot(purchase, aes(x = category)) + geom_bar(aes(fill = is_fraud), position = "dodge") + theme(legend.position="none") + labs(title = "Quantity of Fraud per Type of Merchant", x = "Category of merchant", y = "Quantity of frauds")
##Create boxplot of category of merchants and amount involved in the fraud
ggplot(purchase, aes(x = category, y = amt)) + geom_boxplot(aes(fill = is_fraud), position = "dodge") + theme(legend.position="none") + labs(title = "Amount Involved in the Fraud per Type of Merchant", x = "Category of merchant", y = "Amount involved in the fraud")

##Create histogram of age of customers and quantity of frauds

# Converting the DoB column to a Date format
df$dob <- as.Date(df$dob)

# Getting age from DoB
df$Age <- floor(as.numeric(difftime(Sys.Date(), df$dob, units = "days"))/365.25)
age <- df[, c("Age", "is_fraud", 'amt')] # Selecting only the Age and is_fraud columns
# Creating Bin for the Age Columns
ggplot(age, aes(x = Age)) + geom_histogram(binwidth = 5, aes(fill = is_fraud), colour = 4, fill = "white") + scale_x_continuous(breaks = seq(10,100,10)) + theme(legend.position="none") + labs(title = "Quantity of Fraud per Age of the Customers", x = "Age of the customers", y = "Quantity of frauds")


##Create scatterplot of age of customers and amount involved in the fraud
new_df = filter(age, is_fraud == 1)
age_bins = data.frame(df$Age, bin = cut(df$Age, c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), include.lowest = TRUE))

ggplot(df, aes(x = Age, y = amt)) + geom_point(aes(color = is_fraud), colour = 4, fill = "white") + scale_x_continuous(breaks = seq(10,100,10)) + theme(legend.position="none") + labs(title = "Scatterplot of Age of Customers and Amount Involved in the Fraud", x = "Age", y = "Amount Involved in the Fraud")
