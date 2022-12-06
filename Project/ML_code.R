# install.packages("aod")
# install.packages("neuralnet")
# install.packages("randomForest")

library(aod)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)
library(tidyr)

mydata <- read.csv("C:/Users/shahn/OneDrive - The University of Texas at Dallas/BUAN 6359 Business Analytics with R Zhe Zang/Group Project/archive/fraudTrain1.csv")
## view the first few rows of the data

mydata <- subset(mydata, select = c(-dob, -trans_num, -trans_date_trans_time))
head(mydata)


# Converting Categorical values to numerical labels
unique_category <- unique(mydata$category)
unique_gender <- unique(mydata$gender)

# creating a fucntion to use the which function over a column
# Category
pm1 <- function(x) {
  which(unique_category == x)
}
# Genders
pm2 <- function(x) {
  which(unique_gender == x)
}
mydata$category <- sapply(mydata$category, pm1)
mydata$gender <- sapply(mydata$gender, pm2)
head(mydata)


# # Creating a decision tree model
set.seed(1)
dt <- rpart(is_fraud ~ ., data=mydata, method="class")
dt



# Developing a logistic regression model
# training on mydata
set.seed(1)
# Creating the model
model <- glm(is_fraud ~ ., data = mydata, family = binomial)
summary(model)




## Testing the model on Test Dataset 
# reading test data
test <- read.csv("C:/Users/shahn/OneDrive - The University of Texas at Dallas/BUAN 6359 Business Analytics with R Zhe Zang/Group Project/archive/fraudTest11.csv", nrows = 3000)

test <- subset(test, select = c(-dob, -trans_num, -trans_date_trans_time))
head(test)

# Using the same labels which were used in Training data
test$category <- sapply(test$category, pm1)
test$gender <- sapply(test$gender, pm2)

# Dropping the is_fraud column
test_for_pred <- subset(test, select = -c(is_fraud))

# predicting the test data

# preicting values with decision tree ML Model
pred <- predict(dt, test_for_pred, type = "class")

# # predicting values with logstic regression ML Model

# # specifying the threshold value for the link function
pred1 <- predict(model, test_for_pred, type = "response")
# here we use the cutoff value as .04 as the training dataset has a lot of values which are nto fraud.
# Because of which more data will be incorrectly classified as fraud.
# to test this further we try to increase the test data to check if the cutoff value remains the same or not.
pred1 <- ifelse(pred1 > .04, 1, 0)


head(pred)

length(test$is_fraud[test$is_fraud == 1])
length(pred[pred == 1])

# Prediction by logistic regression model
length(pred1[pred1 == 1])
# Creating a confusion matrix
confusionMatrix(pred1, test$is_fraud)
