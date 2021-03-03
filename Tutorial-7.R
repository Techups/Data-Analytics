hr_data = read.csv('Data/HR_Data.csv')
str(hr_data)
summary(hr_data)

sum(is.na(hr_data))

View(hr_data)

library(ggplot2)
ggplot(hr_data,aes(x=exp_in_company)) + 
  geom_density(alpha=.2,fill='#FF6666')

boxplot(hr_data$exp_in_company)
#outliers in exp_in_company



hr_data$Work_accident = as.factor(hr_data$Work_accident)
hr_data$left = as.factor(hr_data$left)
hr_data$promotion_last_5years = as.factor(hr_data$promotion_last_5years)


new_hr_data = hr_data

table(hr_data$salary)
new_hr_data$low_salary = ifelse(new_hr_data$salary=='low',1,0)
new_hr_data$medium_salary = ifelse(new_hr_data$salary=='medium',1,0)


table(new_hr_data$role)

new_hr_data$role_sales = ifelse(new_hr_data$role=='sales',1,0)
new_hr_data$role_technical = ifelse(new_hr_data$role=='technical',1,0)
new_hr_data$role_support = ifelse(new_hr_data$role=='support',1,0)
new_hr_data$role_IT = ifelse(new_hr_data$role=='IT',1,0)
new_hr_data$role_product_mng = ifelse(new_hr_data$role=='product_mng',1,0)
new_hr_data$role_marketing = ifelse(new_hr_data$role=='marketing',1,0)
new_hr_data$role_RD = ifelse(new_hr_data$role=='RandD',1,0)
new_hr_data$role_accounting = ifelse(new_hr_data$role=='accounting',1,0)

View(new_hr_data)

new_hr_data = new_hr_data[ ,-c(9,10)]
View(new_hr_data)

str(new_hr_data)

new_hr_data = transform(new_hr_data,
                        low_salary = as.factor(low_salary),
                        medium_salary = as.factor(medium_salary),
                        role_sales = as.factor(role_sales),
                        role_technical = as.factor(role_technical),
                        role_support = as.factor(role_support),
                        role_IT = as.factor(role_IT),
                        role_product_mng = as.factor(role_product_mng),
                        role_marketing = as.factor(role_marketing),
                        role_RD = as.factor(role_RD),
                        role_accounting = as.factor(role_accounting))
str(new_hr_data)

traindataindex_hr = sample(1:nrow(new_hr_data),0.7*nrow(new_hr_data),replace = F)
traindata_hr = new_hr_data[traindataindex_hr, ]
testdata_hr = new_hr_data[-traindataindex_hr, ]
View(traindata_hr)
View(testdata_hr)

hr_reg = glm(left~., data = traindata_hr,family = 'binomial')
summary(hr_reg)

hr_reg_predict = glm(left~ satisfaction_level
                     #+ last_evaluation
                     + number_project 
                     + average_montly_hours    
                     + exp_in_company          
                     + Work_accident
                     + promotion_last_5years
                     + low_salary
                     + medium_salary
                     #+ role_sales
                     #+ role_technical
                     #+ role_support
                     #+ role_IT
                     #+ role_product_mng
                     #+ role_marketing
                     + role_RD
                     #+ role_accounting
                     ,data=testdata_hr,family = 'binomial')
summary(hr_reg_predict)

testdata_hr$predicted_attrition = predict(hr_reg_predict,testdata_hr,type = c('response'))
View(testdata_hr)

table(testdata_hr$left)/nrow(testdata_hr)

quantile(testdata_hr$predicted_attrition,probs = seq(0,1,0.05))

testdata_hr$predicted_attrition = ifelse(testdata_hr$predicted_attrition>0.39,1,0)
View(testdata_hr)

table_mat_hr<-table(testdata_hr$predicted_attrition,testdata_hr$left)
table_mat_hr

accuracy_hr = sum(diag(table_mat_hr))/sum(table_mat_hr)
accuracy_hr
      