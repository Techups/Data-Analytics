library(ggplot2)
#install.packages('ggthemes')
#install.packages('lubridate')
#install.packages('tidyr')
#install.packages('DT')
#install.packages('scales')
library(lubridate)
library(ggthemes)
library(dbplyr)
library(tidyr)
library(DT)
library(scales)
library(dplyr)

setwd('G:/R_projects/Analytics_basics/Abhinav_Analytics/Data/Uber-dataset')

colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840","#0683c9", "#e075b0")
apr_data <- read.csv("uber-raw-data-apr14.csv")
may_data <- read.csv("uber-raw-data-may14.csv")
jun_data <- read.csv("uber-raw-data-jun14.csv")
jul_data <- read.csv("uber-raw-data-jul14.csv")
aug_data <- read.csv("uber-raw-data-aug14.csv")
sep_data <- read.csv("uber-raw-data-sep14.csv")

data_2014 <- rbind(apr_data,may_data, jun_data, jul_data, aug_data, sep_data)

data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")

data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")

data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)

data_2014$day <- factor(day(data_2014$Date.Time))
data_2014$month <- factor(month(data_2014$Date.Time, label = TRUE))
data_2014$year <- factor(year(data_2014$Date.Time))
data_2014$dayofweek <- factor(wday(data_2014$Date.Time, label = TRUE))

data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))

hour_data <- data_2014 %>%
  group_by(hour) %>%
  dplyr::summarize(Total = n()) 
datatable(hour_data)
View(hour_data)

ggplot(hour_data,aes(hour,Total)) + 
  geom_bar(stat = 'identity',fill = 'steelblue',color= 'red') +
  ggtitle('Trips Every Hour') + theme(legend.position = 'none')+
  scale_y_continuous(labels=comma)

month_hour <- data_2014 %>% 
  group_by(month,hour) %>%
  dplyr::summarise(Total=n())

ggplot(month_hour,aes(hour,Total,fill=month)) +
  geom_bar(stat='identity') + 
  ggtitle('Trips by Hour and Month') +
  scale_y_continuous(labels=comma)

day_group <- data_2014 %>% 
  group_by(day) %>% 
  dplyr::summarise(Total=n())
datatable(day_group)

ggplot(day_group, aes(day,Total)) +
  geom_bar(stat="identity",fill="steelblue")+
  ggtitle("Trips Every Day")+
  theme(legend.position="none")+
  scale_y_continuous(labels=comma)

day_month_group <- data_2014 %>%
  group_by(month,day) %>%
  dplyr::summarize(Total=n())

ggplot(day_month_group,aes(day,Total,fill=month))+
  geom_bar(stat="identity")+
  ggtitle("Trips by Day and Month")+
  scale_y_continuous(labels=comma)+
  scale_fill_manual(values = colors)

month_group <- data_2014 %>% 
  group_by(month) %>% 
  dplyr::summarize(Total=n())
datatable(month_group)

ggplot( month_group, aes(month,Total,fill=month))+
  geom_bar(stat="identity")+
  ggtitle("Trips by Month")+
  theme(legend.position = "none")+
  scale_y_continuous(labels=comma)+
  scale_fill_manual(values=colors)

month_weekday <- data_2014 %>%
  group_by(month,dayofweek) %>%
  dplyr::summarize(Total=n())

ggplot(month_weekday,aes(month,Total,fill=dayofweek))+
         geom_bar(stat="identity",position = "dodge")+
         ggtitle("Trips by Day and Month")+
         scale_y_continuous(labels=comma)+
         scale_fill_manual(values=colors)

ggplot(data_2014,aes(Base)) +
  geom_bar(fill="darkblue")+
  scale_y_continuous(labels=comma)+
  ggtitle("Trips by bases")

ggplot(data_2014, aes(Base, fill = month)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and Month") +
  scale_fill_manual(values = colors)

day_and_hour <- data_2014 %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total = n())
datatable(day_and_hour)

ggplot(day_and_hour, aes(day, hour, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Hour and Day")

ggplot(month_weekday, aes(dayofweek, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day of Week")
