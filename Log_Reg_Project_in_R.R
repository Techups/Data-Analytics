library(dplyr)
data_adult = read.csv("annual_income.csv")
View(data_adult)


data_adult[data_adult == '?'] = NA
summary(data_adult)

sum(is.na(data_adult))

da_1 = data_adult[complete.cases(data_adult),]
str(da_1)
summary(da_1)
View(da_1)


continuous <- select_if(da_1, is.numeric)
summary(continuous)

library(ggplot2)
ggplot(continuous, aes(x=hours.per.week)) +
  geom_density(alpha=.2, fill='#FF6666')


boxplot(data_adult$hours.per.week)

top_one_percent = quantile(data_adult$hours.per.week, .99)
top_one_percent


data_adult_drop <- da_1 %>%
  filter(hours.per.week<top_one_percent)
dim(data_adult_drop)
View(data_adult_drop)


boxplot(data_adult_drop$hours.per.week)

continuous <- select_if(data_adult_drop, is.numeric)
summary(continuous)


data_adult_rescale <- data_adult_drop %>%
  mutate_if(is.numeric, funs(as.numeric(scale(.))))
head(data_adult_rescale)
View(data_adult_rescale)

str(data_adult_rescale)

char = data.frame(select_if(data_adult_rescale,is.character))
ncol(char)

graph = lapply(names(char),
               function(x)
                 ggplot(char,aes(get(x)))+
                 geom_bar()+
                 theme(axis.text.x=element_text(angle=90)))
graph

recast_data = data_adult_rescale %>%
  mutate(education = factor(ifelse(education == "Preschool" | education == "10th" | 
                                     education == "11th" | education == "12th" | 
                                     education == "1st-4th" | education == "5th-6th" | 
                                     education == "7th-8th" | education == "9th", "dropout", 
                                   ifelse(education == "HS-grad", "HighGrad",
                                          ifelse(education == "Some-college" | 
                                                   education == "Assoc-acdm" | 
                                                   education == "Assoc-voc", "Community",
                                                 ifelse(education == "Bachelors", "Bachelors",
                                                        ifelse(education == "Masters" | 
                                                                 education == "Prof-school", "Master", "PhD")))))))

#check the number of individuals within each group of education.
table(recast_data$education)

#Recast Marital-status: creating lower levels for the marital status
#Change level in marital status var
recast_data <- recast_data %>%
  mutate(marital.status = factor(ifelse(marital.status == "Never-married" | 
                                          marital.status == "Married-spouse-absent", "Not_married", 
                                        ifelse(marital.status == "Married-AF-spouse" | 
                                                 marital.status == "Married-civ-spouse", "Married", 
                                               ifelse(marital.status == "Separated" | 
                                                        marital.status == "Divorced", "Separated", "Widow")))))
#check the number of individuals within each group.
table(recast_data$marital.status)

#Summary Statistic----
#Here, we check some statistics about our target variables.
# Plot gender income
ggplot(recast_data, aes(x = gender, fill = income)) +
  geom_bar(position = "fill") +
  theme_classic()

#checking if race of individual affects their earning.
# Plot race with income
ggplot(recast_data, aes(x = race, fill = income)) +
  geom_bar(position = "fill") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))

# Plot number of hours work with gender.
# box plot gender with working time
ggplot(recast_data, aes(x = gender, y = hours.per.week)) +
  geom_boxplot() +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               color = "steelblue") +
  theme_classic()

#Split the dataset in Train/Test data----
#Prep Training and Test data.----
View(recast_data)
trainDataIndex <- sample(1:nrow(recast_data),0.7*nrow(recast_data), replace = F)
trainData <-recast_data[trainDataIndex, ]
testData <- recast_data[-trainDataIndex, ]
View(trainData)
View(testData)

# Income distribution of train data
table(trainData$income)

# Change Y values to 1's and 0's
trainData$income <- ifelse(trainData$income == ">50K", 1, 0)
str(trainData)
trainData$income <- factor(trainData$income, levels = c(0, 1))
str(trainData)

#Build the regression model----
logit <- glm(income~., data = trainData, family = 'binomial')
summary(logit)

logit <- glm(income~
               age+
               #workclass+
               education+educational.num+
               marital.status+ 
               gender+hours.per.week, data = trainData, family = 'binomial')
summary(logit)

#Change Dependent Var "Income" values to 1's and 0's----
testData$income <- ifelse(testData$income == ">50K", 1, 0)

#----------Predict--------------------
testData$Pred_Income <- predict(logit,testData,type =c("response"))
View(testData)
table(testData$income)/nrow(testData)

#from above cmd, we know that in the test dataset, 
#count of "0" in column "Income" is 75% and count of "1" is 25%.
#Hence, we try to see the data in column "Pred_Income", using quantile fxn.
#And notice the values at quantile level 75%, i.e., between 70% to 80%
quantile(testData$Pred_Income, probs = seq(0,1,0.05))
table(testData$income)
#Since, the value at quantile level at 75% is 4.058485e-01,
#Hence, we consider all the values greater than this value as 1 and others as 0 in our Predicted values column.
testData$Pred_Income <- ifelse(testData$Pred_Income > 0.4,1,0)
View(testData)

#Checking Accuracy of Model using: Confusion Matrix----
table(testData$Pred_Income)/nrow(testData)
table_mat<-table(testData$Pred_Income,testData$income)
table_mat
##From the above table cmd, we have got confusion matrix, stating that:
#No. of "0" present in training dataset is equal to no. of "0" predicted in test dataset , for 9031 enteries.
#No. of "1" present in training dataset is equal to no. of "1" predicted in test dataset for 2144 enteries.
#Hence, Accuracy will be = (9031+2144)/(9031+2144+1211+1279) = 81.7%
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test
