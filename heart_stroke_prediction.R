library('dplyr')
library('VIM')

#reading data
data <- read.csv('Data/healthcare-dataset-stroke-data.csv')

#creating a view of data
View(data)

#changing 'N/A' values to NA 
data[data=='N/A'] = NA

#total na values
sum(is.na(data))

#dimensions of data
dim(data)

#na values are less than 10% so we will drop all these na values
cleaned_data = na.omit(data)
sum(is.na(cleaned_data))
dim(cleaned_data)

View(cleaned_data)

#We see there are unknown values in smoking status variable so we have to deal with that now
table(cleaned_data$smoking_status)
cleaned_data[cleaned_data=='Unknown'] = NA

summary(cleaned_data)

#Doing knn imputation
knn1 = kNN(cleaned_data, variable='smoking_status',k=7)

summary(knn1)

dim(knn1)

new_dataset = subset(knn1,select=id:stroke)

str(new_dataset)

#Now lets fix our variables

#dummy variables for gender
table(new_dataset$gender)
new_dataset$gender_female = ifelse(new_dataset$gender=='Female',1,0)

#dummy variables for ever married 
table(new_dataset$ever_married)
new_dataset$ever_married_Yes = ifelse(new_dataset$ever_married=='Yes',1,0)

#dummy variables for work type
table(new_dataset$work_type)

new_dataset$work_type_Private = ifelse(new_dataset$work_type=='Private',1,0)
new_dataset$work_type_Self_employed = ifelse(new_dataset$work_type=='Self-employed',1,0)
new_dataset$work_type_children = ifelse(new_dataset$work_type=='children',1,0)
new_dataset$work_type_Govt_job = ifelse(new_dataset$work_type=='Govt_job',1,0)

#dummy variables for residence type
table(new_dataset$Residence_type)

new_dataset$Residence_type_urban = ifelse(new_dataset$Residence_type=='Urban',1,0)

#dummy variables for smoking status
table(new_dataset$smoking_status)

new_dataset$never_smoked = ifelse(new_dataset$smoking_status=='never smoked',1,0)
new_dataset$formerly_smoked = ifelse(new_dataset$smoking_status=='formerly smoked',1,0)

#now we are done with dummy variables lets now convert the variables to suitable data types
str(new_dataset)

new_dataset = transform(new_dataset,
                        stroke = as.factor(stroke),
                        gender_female = as.factor(gender_female),
                        ever_married_Yes = as.factor(ever_married_Yes),
                        work_type_Private = as.factor(work_type_Private),
                        work_type_Self_employed = as.factor(work_type_Self_employed),
                        work_type_children = as.factor(work_type_children),
                        work_type_Govt_job = as.factor(work_type_Govt_job),
                        Residence_type_urban = as.factor(Residence_type_urban),
                        never_smoked = as.factor(never_smoked),
                        formerly_smoked = as.factor(formerly_smoked),
                        bmi = as.double(bmi),
                        hypertension = as.factor(hypertension),
                        heart_disease = as.factor(heart_disease))
str(new_dataset)

#now lets remove the extra or original columns
new_dataset = new_dataset[,-c(2,6,7,8,11)] 
str(new_dataset)

dataindex_stroke = sample(1:nrow(new_dataset),0.7*nrow(new_dataset),replace = F)
stroke_train = new_dataset[dataindex_stroke, ]
stroke_test = new_dataset[-dataindex_stroke, ]

stroke_reg = glm(stroke~. , data = stroke_train,family = 'binomial')
summary(stroke_reg)

stroke_reg_predict = glm(stroke ~ #id+
                           age+
                           #hypertension+
                           #heart_disease+
                           avg_glucose_level+
                           #bmi+
                           #gender_female+
                           #ever_married_Yes+
                           #work_type_Private+
                           #work_type_Self_employed+
                           #work_type_Govt_job+
                           #work_type_children+
                           #Residence_type_urban+
                           never_smoked+
                           formerly_smoked,data=stroke_test,
                         family = 'binomial')
summary(stroke_reg_predict)

stroke_test$prediction = predict(stroke_reg_predict,stroke_test,type=c('response'))

table(stroke_test$stroke)/nrow(stroke_test)

quantile(stroke_test$prediction,probs = seq(0,1,0.05))

stroke_test$prediction = ifelse(stroke_test$prediction>0.78,1,0)

table_mat_stroke = table(stroke_test$prediction,stroke_test$stroke)
table_mat_stroke

accuracy_stroke = sum(diag(table_mat_stroke))/sum(table_mat_stroke)
accuracy_stroke
