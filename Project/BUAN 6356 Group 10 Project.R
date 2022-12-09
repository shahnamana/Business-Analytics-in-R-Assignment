##Preparation
library(dplyr)
library(ggplot2)
library(tidyverse)
install.packages("lubridate")
install.packages("eeptools")
install.packages("writexl")
library(lubridate)
library("writexl")
library(eeptools)

##Import data
df <- read.csv("C:/Users/shahn/OneDrive - The University of Texas at Dallas/BUAN 6359 Business Analytics with R Zhe Zang/Group Project/archive/fraudTest.csv", show_col_types = FALSE)

##Clean the data
df <- subset(df, select = -c(...1, cc_num, first, last, gender, street, zip, unix_time))

##Filtering only the fraud transactions
df <- df %>% filter(is_fraud == 1)
df <- df[df["is_fraud"] == 1, ]


##Summary
summary(df)
sum(is.na(df))

##Analyze the relation among category, amount and fraud
purchase <- df[, c(3,4,15)]
colnames(purchase)

##Create histogram of category of merchants and number of frauds
ggplot(purchase, aes(x = category)) + geom_bar(aes(fill = is_fraud), position = "dodge") + theme(legend.position="none") + labs(title = "Quantity of Fraud per Type of Merchant", x = "Category of merchant", y = "Quantity of frauds")

##Create boxplot of category of merchants and amount involved in the fraud
ggplot(purchase, aes(x = category, y = amt)) + geom_boxplot(aes(fill = is_fraud), position = "dodge") + theme(legend.position="none") + labs(title = "Amount Involved in the Fraud per Type of Merchant", x = "Category of merchant", y = "Amount involved in the fraud")

##Create histogram of age of customers and quantity of frauds

##Converting the DoB column to a Date format
df$dob <- as.Date(df$dob)

##Getting age from DoB
df$Age <- floor(as.numeric(difftime(Sys.Date(), df$dob, units = "days"))/365.25)
age <- df[, c("Age", "is_fraud", 'amt')] # Selecting only the Age and is_fraud columns

##Creating Bin for the Age Columns
ggplot(age, aes(x = Age)) + geom_histogram(binwidth = 5, aes(fill = is_fraud), colour = 4, fill = "white") + scale_x_continuous(breaks = seq(10,100,10)) + theme(legend.position="none") + labs(title = "Quantity of Fraud per Age of the Customers", x = "Age of the customers", y = "Quantity of frauds")

##Create scatterplot of age of customers and amount involved in the fraud
new_df = filter(age, is_fraud == 1)
age_bins = data.frame(df$Age, bin = cut(df$Age, c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), include.lowest = TRUE))
ggplot(df, aes(x = Age, y = amt)) + geom_point(aes(color = is_fraud), colour = 4, fill = "white") + scale_x_continuous(breaks = seq(10,100,10)) + theme(legend.position="none") + labs(title = "Age of Customers vs Amount Involved in the Fraud", x = "Age of the customers", y = "Amount involved in the fraud")

##Prepare for prediction
install.packages("aod")
install.packages("neuralnet")
install.packages("randomForest")
install.packages("rpart.plot")
library(rpart.plot)
library(randomForest)
library(neuralnet)
library(aod)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caret)
library(ISLR)
library(dplyr)
library(tidyr)
library(InformationValue)

##Import training data set
mydata <- read.csv("fraudTrain.csv", nrows=10000)

##Recreate age column
mydata$dob <- as.Date(mydata$dob)
mydata$age <- floor(as.numeric(difftime(Sys.Date(), mydata$dob, units = "days"))/365.25)

##View the first few rows of the data
head(mydata)

##Delete unnecessary data
mydata <- subset(mydata, select = c(cc_num, category, amt, gender, zip, lat, long, city_pop, unix_time, merch_lat, merch_long, is_fraud, age))

##Converting Categorical values to numerical labels
unique_category <- unique(mydata$category)
unique_gender <- unique(mydata$gender)

##Creating a function to use the which function over a column
#Category
pm1 <- function(x) {
  which(unique_category == x)
}
#Genders
pm2 <- function(x) {
  which(unique_gender == x)
}
mydata$category <- sapply(mydata$category, pm1)
mydata$gender <- sapply(mydata$gender, pm2)
head(mydata)

##Creating a decision tree model
set.seed(1)
dt <- rpart(is_fraud ~ ., data=mydata, method="class")
dt

##Plot decision tree
rpart.plot(dt)

##Creating a logistic regression model
set.seed(1)
model <- glm(is_fraud ~ ., data = mydata, family = binomial)
summary(model)

##Testing decision tree model
test <- read.csv("fraudTest.csv", nrows=3000)
test$dob <- as.Date(test$dob)
test$age <- floor(as.numeric(difftime(Sys.Date(), test$dob, units = "days"))/365.25)

head(test)
test <- subset(test, select = c(cc_num, category, amt, gender, zip, lat, long, city_pop, unix_time, merch_lat, merch_long, is_fraud, age))

##Apply the same labels which were used in Training data
test$category <- sapply(test$category, pm1)
test$gender <- sapply(test$gender, pm2)

##Dropping the is_fraud column
test_for_pred <- subset(test, select = -c(is_fraud))

##Predicting values with decision tree ML Model
dt_pred <- predict(dt, test_for_pred, type = "class")

##Predicting values with logistic regression ML Model
lr_pred <- predict(model, test_for_pred, type = "response")

##Here we use the cutoff value as .04 as the training data set has a lot of values which are not fraud.
##Because of which more data will be incorrectly classified as fraud.
##To test this further we try to increase the test data to check if the cutoff value remains the same or not.
lr_pred <- ifelse(lr_pred > .04, 1, 0)

head(dt_pred)

##Prediction by data tree model
length(test$is_fraud[test$is_fraud == 1])
length(dt_pred[dt_pred == 1])

##Prediction by logistic regression model
length(lr_pred[lr_pred == 1])

##Creating a confusion matrix
confusionMatrix(dt_pred, as.factor(test$is_fraud))
confusionMatrix(lr_pred, as.factor(test$is_fraud))