##Preparation
library(dplyr)
library(ggplot2)
library(tidyverse)
df <- read_csv('fraudTrain.csv', show_col_types = FALSE)

##Clean the data
df <- subset(df, select = -c(...1, cc_num, first, last, gender, street, zip, unix_time))

##Summary
summary(df)
sum(is.na(df))

##Analyze the relation among category, amount and fraud
purchase <- df[, c(3,4,15)]
purchase
#Create histogram of category of merchants and number of frauds
##Create boxplot of category of merchants and amount involved in the fraud



##Create histogram of age of customers and quantity of frauds
##Create scatterplot of age of customers and amount involved in the fraud