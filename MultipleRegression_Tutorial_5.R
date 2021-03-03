#Tutorial-5 Solution:

# Multiple Linear Regression 

# Importing the dataset 
dataset = read.csv('F:/WORK/rWork/rProjects/Analytics_Edge/Analytics_Edge_R/data/50_Startups.csv')

View(dataset)
summary(dataset)
str(dataset)

#Encoding categorical data 
#Dummy Var for State 
table(dataset$State)/nrow(dataset)
# Avg Premium by state# To check Plan Subscription ratio in dataset
library(sqldf)
sqldf("select State, COUNT (*) as obs, avg(Profit) from dataset GROUP BY 1")
#Create dummy variables for Type:
#Since here out of 3 States types all 3 have large/significant no.of obs,
#Hence, the no. of dummy var here, will be 3-1 = 2.
dataset_1 <-dataset
dataset_1$State1 <- ifelse(dataset_1$State=='California',1,0)
dataset_1$State2 <- ifelse(dataset_1$State=='New York',1,0)
View(dataset_1)
str(dataset_1)

#Removing column: "State" bcz created Dummy Var columns for it.
dataset_1 <- dataset_1[,-4]

# Splitting the dataset into the Training set and Test set 
trainDataIndex <- sample(1:nrow(dataset_1),0.7*nrow(dataset_1), replace = F)
trainData <-dataset_1[trainDataIndex, ]
testData <- dataset_1[-trainDataIndex, ]
View(trainData)
View(testData)

# Fitting Multiple Linear Regression to the Training set 
regressor = lm(Profit ~ ., 
               data = trainData) 
summary(regressor)

regressor1 = lm(Profit ~ R.D.Spend#+Administration+
                 #Marketing.Spend#+State1
                ,data = trainData) 
summary(regressor1)

#On building the model, we see that only var "R&D spend" comes out to be significant enough to predict the val of Dependent var "Profit".

# Predicting the Test set results 
y_pred = predict(regressor1, newdata = testData)
testData$Pred_Profit = y_pred

#Accuracy of the Model:
#MAPE(MeanAbsolutePercentageError): 
#Lower its value better is the accuracy of the model.

#MAPE Calculation:
mape <- mean(abs((testData$Pred_Profit - testData$Profit))/testData$Profit)
mape

# Mape using mape function
#install.packages("Metrics")
library(Metrics)
mape(testData$Pred_Profit,testData$Profit)

#Since the error term is around 0.06, 
#it means there is only 6% error in our model's prediction.

