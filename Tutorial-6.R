cars = read.csv("data/Luxury_Cars.csv")

View(cars)
sum(is.na(cars))

str(cars)

table(cars$Origin)
cars$Origin_Asia = ifelse(cars$Origin == 'Asia',1,0)
cars$Origin_USA = ifelse(cars$Origin == 'USA',1,0)

table(cars$Type)
cars$Type_Sedan = ifelse(cars$Type == 'Sedan',1,0)
cars$Type_SUV = ifelse(cars$Type == 'SUV',1,0)
cars$Type_Sports = ifelse(cars$Type == 'Sports',1,0)
cars$Type_Truck = ifelse(cars$Type == 'Truck',1,0)
cars$Type_Wagon = ifelse(cars$Type == 'Wagon',1,0)

table(cars$DriveTrain)
cars$DriveTrain_Front = ifelse(cars$DriveTrain == 'Front',1,0)
cars$DriveTrain_Rear = ifelse(cars$DriveTrain == 'Rear',1,0)

View(cars)

new_cars = cars[ ,c(-1,-2,-3,-4,-5)]
View(new_cars)
str(new_cars)

traindataind = sample(1:nrow(new_cars),0.7*nrow(new_cars),replace = F)
cars_train = new_cars[traindataind, ]
cars_test = new_cars[-traindataind, ]
View(cars_train)
View(cars_test)

cars_regressor = lm(MPG_Mileage ~ .,
                    data = cars_train)
summary(cars_regressor)

cars_regressor1 = lm(MPG_Mileage ~ #Engine_Size +
                       Horsepower + 
                       Cylinders+ Weight_LBS #+ Wheelbase_inch 
                        #+Length_inch
                       #+ Origin_Asia #+Origin_USA
                       + Type_Sedan+
                       Type_SUV+ Type_Sports+ Type_Truck+
                       Type_Wagon+ DriveTrain_Front,#+ DriveTrain_Rear
                     data = cars_train)
summary(cars_regressor1)

mpg_pred = predict(cars_regressor1,newdata = cars_test)
cars_test$MPG_prediction = mpg_pred

View(cars_test)

cars_mape <- mean(abs((cars_test$MPG_prediction - cars_test$MPG_Mileage))/cars_test$MPG_Mileage)
cars_mape

library(Metrics)
mape(cars_test$MPG_prediction,cars_test$MPG_Mileage)
