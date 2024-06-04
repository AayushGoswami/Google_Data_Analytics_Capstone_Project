#####Install and Load required packages#####
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("markdown")
library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)
library(dplyr)
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")


#####Load the data#####

q1_19 <- read.csv("2019_Q1.csv")
q2_19 <- read.csv("2019_Q2.csv")
q3_19 <- read.csv("2019_Q3.csv")
q4_19 <- read.csv("2019_Q4.csv")

####View data####
View(q1_19)
View(q2_19)
View(q3_19)
View(q4_19)

####Compare the column names of all the datasets####
colnames(q1_19)
colnames(q2_19)
colnames(q3_19)
colnames(q4_19)

####Renaming of column names of q2_19 to maintain consistency####

q2_19 <- q2_19 %>%
  rename(trip_id = X01...Rental.Details.Rental.ID,
  start_time = X01...Rental.Details.Local.Start.Time,
  end_time = X01...Rental.Details.Local.End.Time, 
  bikeid = X01...Rental.Details.Bike.ID,
  tripduration = X01...Rental.Details.Duration.In.Seconds.Uncapped,
  from_station_id = X03...Rental.Start.Station.ID,
  from_station_name = X03...Rental.Start.Station.Name,
  to_station_id = X02...Rental.End.Station.ID,
  to_station_name = X02...Rental.End.Station.Name,
  usertype = User.Type,
  gender = Member.Gender,
  birthyear = X05...Member.Details.Member.Birthday.Year)

####Append all the 4 quarter trip datasets to new big yearly (2019) trip data frame####
trip_data_2019 <- rbind(q1_19,q2_19,q3_19,q4_19)
##alt_trip_data_2019 <- bind_rows(q1_19,q2_19,q3_19,q4_19)
View(trip_data_2019)
##View(alt_trip_data_2019)
####Remove the columns with the values that we don't need or may have some inconsistency and can be created later through calculations#####
trip_data_2019 <- trip_data_2019 %>% 
  select(-c(gender,tripduration,birthyear))


####Inspect the new data frame that has been created####
colnames(trip_data_2019)
nrow(trip_data_2019)
dim(trip_data_2019)
head(trip_data_2019)
tail(trip_data_2019)
str(trip_data_2019)
summary(trip_data_2019)
table(trip_data_2019$usertype)
table(trip_data_2019_v2$from_station_name)
table(trip_data_2019_v2$to_station_name)


####Perform Required Transformations for Analysis####

#Add a date column of format yyyy-mm-dd
trip_data_2019$date <- as.Date(trip_data_2019$start_time)

#Add a month, day and day_of_week column
trip_data_2019$month <- format(as.Date(trip_data_2019$date), "%m")
trip_data_2019$day <- format(as.Date(trip_data_2019$date), "%d")
trip_data_2019$day_of_week <- format(as.Date(trip_data_2019$date), "%A")

#Add a trip_duration column by calculation
trip_data_2019$trip_duration <- difftime(trip_data_2019$end_time,trip_data_2019$start_time)

#Change the format of trip_duration tab from difftime to numeric
trip_data_2019$trip_duration <- as.numeric(as.character(trip_data_2019$trip_duration))
is.numeric(trip_data_2019$trip_duration)

#Make a new data frame trip_data_2019_v2 by deleting "bad data"
trip_data_2019_v2 <- trip_data_2019[!(trip_data_2019$trip_duration < 0),]
trip_data_2019_v2 <- trip_data_2019_v2[!(trip_data_2019_v2$to_station_name == "HUBBARD ST BIKE CHECKING (LBS-WH-TEST)" | trip_data_2019_v2$to_station_name == "DIVVY Map Frame B/C Station" | trip_data_2019_v2$to_station_name == "DIVVY CASSETTE REPAIR MOBILE STATION" | trip_data_2019_v2$to_station_name == "TS ~ DIVVY PARTS TESTING"),]
trip_data_2019_v2 <- trip_data_2019_v2[!(trip_data_2019_v2$from_station_name == "HUBBARD ST BIKE CHECKING (LBS-WH-TEST)" | trip_data_2019_v2$from_station_name == "DIVVY Map Frame B/C Station" | trip_data_2019_v2$from_station_name == "DIVVY CASSETTE REPAIR MOBILE STATION"),]
View(trip_data_2019_v2)


####Conduct Descriptive Analysis####

##Descriptive analysis on the trip_duration

#Mean of Trip Duration
mean(trip_data_2019_v2$trip_duration)

#Median of Trip Duration
median(trip_data_2019_v2$trip_duration)

#Maximum of the Trip Duration
max(trip_data_2019_v2$trip_duration)

#Minimum of the Trip Duration
min(trip_data_2019_v2$trip_duration)

#All the above data can be viewed all at once by the summary() function for the Trip Duration
summary(trip_data_2019_v2$trip_duration)


##Compare Subscribers and Customers based on different parameters

#Average Trip Duration
aggregate(trip_data_2019_v2$trip_duration ~ trip_data_2019_v2$usertype, FUN = mean)

#Median of Trip Duration
aggregate(trip_data_2019_v2$trip_duration ~ trip_data_2019_v2$usertype, FUN = median)

#Longest Trip Duration
aggregate(trip_data_2019_v2$trip_duration ~ trip_data_2019_v2$usertype, FUN = max)

##Shortest Trip Duration
aggregate(trip_data_2019_v2$trip_duration ~ trip_data_2019_v2$usertype, FUN = min)

#Total Trip Duration
aggregate(trip_data_2019_v2$trip_duration ~ trip_data_2019_v2$day_of_week, FUN = sum)

#Trips on each day of the Week
aggregate(trip_data_2019_v2$trip_duration ~ trip_data_2019_v2$usertype + trip_data_2019_v2$day_of_week, FUN = mean)
#The days of Week are out of order. Lets fix that and view the total number of trips on each day of week again with the days of the week arranged in order from Sunday - Saturday
trip_data_2019_v2$day_of_week <- ordered(trip_data_2019_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(trip_data_2019_v2$trip_duration ~ trip_data_2019_v2$usertype + trip_data_2019_v2$day_of_week, FUN = mean)

##Other sub-data sets created for further help in analysis

total_usertypes <- trip_data_2019_v2 %>% 
  group_by(usertype) %>% 
  summarise(total_users = n())

total_rides_by_day <- trip_data_2019_v2 %>% 
  group_by(day_of_week) %>% 
  summarise(total_rides = n())

rider_weekday <- trip_data_2019_v2 %>% 
  group_by(day_of_week, usertype) %>% 
  summarise(number_of_rides = n(), mean_ride = mean(trip_duration))
#ggplot(trip_data_2019_v2) + geom_bar(mapping = aes(x = usertype, fill = day_of_week ))

####Visualizations of the Data####

#Total number of rides per Weekday plot
trip_data_2019_v2 %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(trip_duration)) %>% 
  arrange(usertype, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = usertype)) + geom_col(position = "dodge")
#Here we can see that the plot does not look so presentable without proper title and labeling of the axes and the legend. also the values of the y-axis are truncated. lets fix it with aesthetic modifications on the plot

#Total number of rides per Weekday plot with proper visual modifications applied
trip_data_2019_v2 %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(trip_duration)) %>% 
  arrange(usertype, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = usertype)) + geom_col(position = "dodge") + scale_y_continuous(labels = scales::comma) + labs(x = "Day of Week", y = "Number of Rides", title = "Number of Rides on each day of the Week in 2019") + scale_fill_discrete(name = "Type of User")

#Total number of rides per Weekday faceted on type of user
trip_data_2019_v2 %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(trip_duration)) %>% 
  arrange(usertype, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = usertype)) + geom_col(position = "dodge") + scale_y_continuous(labels = scales::comma) + facet_wrap(~usertype) + labs(x = "Day of Week", y = "Number of Rides", title = "Number of Rides on each day of the Week in 2019", subtitle = "Seperated by Type of User") + scale_fill_discrete(name = "Type of User")

#Average Ride duration per Weekday of each type of User
trip_data_2019_v2 %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(trip_duration)) %>% 
  arrange(usertype, weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = usertype)) + geom_col(position = "dodge") + facet_wrap(~usertype) + labs(y = "Average Trip Duration (mins)", x = "Day of Week", title = "Average Trip Duration of Different types of Users in 2019") + scale_fill_discrete(name = "Type of User")

#Total number of Rides per Month of 2019
trip_data_2019_v2 %>% 
  mutate(month_name = month(start_time, label = TRUE)) %>% 
  group_by(usertype, month_name) %>% 
  summarise(number_of_rides = n(), avg_duration = mean(trip_duration)) %>% 
  arrange(usertype, month_name) %>% 
  ggplot() + geom_col(aes(x = month_name, y = number_of_rides, fill = usertype), position = "dodge")  + scale_y_continuous(labels = scales::comma) + labs(x = "Month", y = "Number of Rides", title = "Number of Rides per Month in 2019") + scale_fill_discrete(name = "Type of User")

#Total number of Rides each hour of the day whole year 2019
trip_data_2019_v2 %>% 
  mutate(hours = hour(start_time)) %>% 
  group_by(usertype, hours) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(usertype, hours) %>% 
  ggplot() + geom_col(aes(x = hours, y = number_of_rides, fill = usertype), position = "dodge") + scale_y_continuous(labels = scales::comma) + labs(x = "Hour of the Day", y = "Number of Rides", title = "Number of Rides per Hour", subtitle = "for whole of the year 2019") + scale_fill_discrete(name  = "Type ofUser")
