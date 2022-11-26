# install.packages("aod")
# install.packages("neuralnet")
# install.packages("randomForest")

library(randomForest)
library(neuralnet)
library(aod)
library(ggplot2)

mydata <- read.csv("C:/Users/shahn/OneDrive - The University of Texas at Dallas/BUAN 6359 Business Analytics with R Zhe Zang/Group Project/archive/fraudTrain1.csv")
## view the first few rows of the data
# head(mydata)
# summary(mydata)
# colnames(mydata)


# # Creating a logistic regression model
# model <- glm(is_fraud ~ ., data = mydata, family = binomial)
# summary(model)





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


# # Creating a neural network model with 1 hidden layer and logistic regression
# set.seed(1)
# nn <- neuralnet(is_fraud ~ ., data=mydata, hidden=3, linear.output=FALSE, act.fct="logistic")

# # display weights
# nn$weights

# # display predictions
# prediction(nn)

# # plot network
# plot(nn, rep="best")






#  Crating a random forest model
set.seed(1)
rf <- randomForest(is_fraud ~ ., data=mydata, ntree=1000, importance=TRUE)
rf

# plot the importance of each variable
varImpPlot(rf)

# Plotting the tree
plot(rf)



## Testing the model on Test Dataset 
# reading test data
test <- read.csv("C:/Users/shahn/OneDrive - The University of Texas at Dallas/BUAN 6359 Business Analytics with R Zhe Zang/Group Project/archive/fraudTest1.csv")
# Using the same labels which were used in Training data
test$category <- sapply(test$category, pm1)
test$gender <- sapply(test$gender, pm2)

# Dropping the is_fraud column
test_for_pred <- subset(test, select = -c(is_fraud))

# predicting the test data
pred <- predict(rf, test_for_pred, type = "class")

# printing the confusion matrix
table(pred, test$is_fraud)
confusionMatrix(pred, test$is_fraud)
