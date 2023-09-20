'This is a step-wise study and analysis of the Divvy Bike Share data from the last 12months.
The most recent data being ridership data from May 2022.

1. Data was provided in structured csv files
2. We will be reading the data into R using the tidyverse package and its library of functions.
3. We will be using the lubridate package and its library of functions to handle date operations.'

#Step 1. Organization, setting up working firectory for my R scripts, Markdowns, and Graphics
getwd()
setwd('/Users/Olumedey/Documents/Career/Google Data Analytics/Capstone/R_Scripts_Markdowns')

#Step 2: Load the libraries you plan to use for Data manipulation and Analysis
library(tidyverse)
library(lubridate)
library(ggplot2)
library(skimr)
library(dplyr)
library(janitor)
library(hms)
#Step 3: Import and Inspect all 12 data sets.
Jun2021<- read.csv('/Users/Olumedey/Documents/Career/Google Data Analytics/Capstone/Capstone Data set/csv files/202106-divvy-tripdata.csv')%>% 
  select(-c(X, X.1, X.2)) #Clean trailing empty columns
Jul2021<- read.csv('/Users/Olumedey/Documents/Career/Google Data Analytics/Capstone/Capstone Data set/csv files/202107-divvy-tripdata.csv')
Aug2021<- read.csv('/Users/Olumedey/Documents/Career/Google Data Analytics/Capstone/Capstone Data set/csv files/202108-divvy-tripdata.csv')
Sep2021<- read.csv('/Users/Olumedey/Documents/Career/Google Data Analytics/Capstone/Capstone Data set/csv files/202109-divvy-tripdata.csv')
Oct2021<- read.csv('/Users/Olumedey/Documents/Career/Google Data Analytics/Capstone/Capstone Data set/csv files/202110-divvy-tripdata.csv')
Nov2021<- read.csv('/Users/Olumedey/Documents/Career/Google Data Analytics/Capstone/Capstone Data set/csv files/202111-divvy-tripdata.csv')
Dec2021<- read.csv('/Users/Olumedey/Documents/Career/Google Data Analytics/Capstone/Capstone Data set/csv files/202112-divvy-tripdata.csv')
Jan2022<- read.csv('/Users/Olumedey/Documents/Career/Google Data Analytics/Capstone/Capstone Data set/csv files/202201-divvy-tripdata.csv')
Feb2022<- read.csv('/Users/Olumedey/Documents/Career/Google Data Analytics/Capstone/Capstone Data set/csv files/202202-divvy-tripdata.csv')
Mar2022<- read.csv('/Users/Olumedey/Documents/Career/Google Data Analytics/Capstone/Capstone Data set/csv files/202203-divvy-tripdata.csv')
Apr2022<- read.csv('/Users/Olumedey/Documents/Career/Google Data Analytics/Capstone/Capstone Data set/csv files/202204-divvy-tripdata.csv')
May2022<- read.csv('/Users/Olumedey/Documents/Career/Google Data Analytics/Capstone/Capstone Data set/csv files/202205-divvy-tripdata.csv')

colnames(Jun2021)
colnames(Jul2021)
colnames(Aug2021)
colnames(Sep2021)
colnames(Oct2021)
colnames(Nov2021)
colnames(Dec2021)
colnames(Jan2022)
colnames(Feb2022)
colnames(Mar2022)
colnames(Apr2022)
colnames(May2022)
#Step 4: Rename Column names
#Since all files have the same column, we can execute the same renaming statement for all data frames.Loop through??
rename_func<- function(Dataset){
  rename(Dataset,Trip_Id = Ride_Id
         , BikeId = Rideable_Type
         , Start_Time = Started_At
         , End_Time = Ended_At
         , From_Station_Name = Start_Station_Name
         , From_Station_Id = Start_Station_Id
         , To_Station_Name = End_Station_Name
         , To_Station_Id = End_Station_Id
         , Usertype = Member_Casual
  )
} #Function to rename columns using dplyr rename()

Jun2021<- rename_func(Jun2021)
Jul2021<- rename_func(Jul2021)
Aug2021<- rename_func(Aug2021)
Sep2021<- rename_func(Sep2021)
Oct2021<- rename_func(Oct2021)
Nov2021<- rename_func(Nov2021)
Dec2021<- rename_func(Dec2021)
Jan2022<- rename_func(Jan2022)
Feb2022<- rename_func(Feb2022)
Mar2022<- rename_func(Mar2022)
Apr2022<- rename_func(Apr2022)
May2022<- rename_func(May2022)

#Step 5: Combine all data sets into a 12month data frame
All_Trips<- bind_rows(Jul2021, Aug2021, Sep2021, Oct2021, Nov2021, Dec2021, Jan2022, Feb2022, Mar2022, Apr2022, May2022)
colnames(All_Trips)
nrow(All_Trips) #Number of trips over the past year
dim(All_Trips)
str(All_Trips)
All_Trips<- All_Trips[-c(17)] #Remove trailing column <https://www.listendata.com/2015/06/r-keep-drop-columns-from-data-frame.html>
summary(All_Trips)
#The concatenated data set has 5,131,181 rows and 16 columns
#Summary shows 
str(All_Trips$Usertype)
skim_without_charts(All_Trips$Usertype) #gives the number of unique observations for each column: 2 types of users
table(All_Trips$Usertype) #Provides the number of distinct observations under the Usertype variable.
skim_without_charts(All_Trips$From_Station_Id)
distinct(as_tibble(All_Trips$From_Station_Id)) #Capture number of Stations

#Step 6: Create more Date columns(variables) to capture year, month and days effectively
All_Trips$Date<- as.Date(All_Trips$Start_Time)
All_Trips$Month<- format(All_Trips$Date, format= "%B")
All_Trips$Year<- format(All_Trips$Date, format= "%Y")
All_Trips$Day<- format(All_Trips$Date, format= "%d")
All_Trips$DoW2<- format(All_Trips$Date, format= "%A")

All_Trips2$DateTime<- fast_strptime(All_Trips2$Start_Time, format = '%Y-%m-%d %H:%M', tz ="America/Chicago")
All_Trips2$ToD<- format(All_Trips2$DateTime, format = '%H:%M')
'All_Trips2<- All_Trips2[,!(names(All_Trips2) %in% "DateTime")]'
str(parse_date_time(All_Trips2$ToD, "HM"))
All_Trips2$ToD2<- hms::as_hms(parse_date_time(All_Trips2$DateTime, "HM"))
All_Trips2$ToD3<- hour(All_Trips2$DateTime)
All_Trips2$ToD3<- as.numeric(substr(All_Trips2$ToD2,1,2))
All_Trips2$ToD4<- ifelse(All_Trips2$ToD3<=8,"12am to 8am",ifelse(All_Trips2$ToD3>8 & All_Trips2$ToD3<=12,"9am to 12pm",ifelse(All_Trips2$ToD3>12 & All_Trips2$ToD3<=15,"1pm to 3pm",
                  ifelse(All_Trips2$ToD3>15 & All_Trips2$ToD3<=18,"4pm to 6pm",ifelse(All_Trips2$ToD3>18 & All_Trips2$ToD3<=21,"7pm to 9pm",ifelse(All_Trips2$ToD3>21 & All_Trips2$ToD3<=24,"10pm to 11pm","N/A"))))))

#Step 7: Create a variable to calculate Ride_length in seconds
All_Trips<- mutate(All_Trips, Ride_Length = difftime(All_Trips$End_Time,All_Trips$Start_Time, units = "secs"))
All_Trips$Ride_Length<- as.numeric(as.character(All_Trips$Ride_Length))
is.numeric(All_Trips$Ride_Length) #Check to ensure values in Ride_length are numeric so we can perform calculations on it

str(All_Trips)

#Step 8: Removing bad/noisy data
all_trips1<- All_Trips[(All_Trips$Ride_Length < 0),] #83 observations with negative Ride_Length
all_trips2<- All_Trips[(All_Trips$From_Station_Name=="" | All_Trips$To_Station_Name==""),] #1,072,613 observations are missing the Start or End station information
All_Trips2<- All_Trips[!(All_Trips$Ride_Length <0 | All_Trips$From_Station_Name =="" | All_Trips$To_Station_Name ==""),]
str(All_Trips2) #Cleaner Data has 4,058,461 observations/rows
summary(All_Trips2$Ride_Length)

#Step 9: Descriptive Analysis on Ride Lengths and Members
RL_analysis<- c(Mean = mean(All_Trips2$Ride_Length), Median = median(All_Trips2$Ride_Length),
                Max = max(All_Trips2$Ride_Length), Min= min(All_Trips2$Ride_Length))
'Comparing Members and Casual Users'
aggregate(All_Trips2$Ride_Length ~ All_Trips2$Usertype, FUN = mean)
aggregate(All_Trips2$Ride_Length ~ All_Trips2$Usertype, FUN = median)
aggregate(All_Trips2$Ride_Length ~ All_Trips2$Usertype, FUN = min)
aggregate(All_Trips2$Ride_Length ~ All_Trips2$Usertype, FUN = max)
aggregate(All_Trips2$Ride_Length ~ All_Trips2$Usertype, FUN = length)

aggregate(All_Trips2$Ride_Length ~ All_Trips2$Usertype+All_Trips2$DoW, FUN = mean) #Average ride length by user type and day of the week
All_Trips2$DoW<- ordered(All_Trips2$DoW2, levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
All_Trips2$ToD4<- ordered(All_Trips2$ToD4, levels = c('12am to 8am','9am to 12pm','1pm to 3pm','4pm to 6pm','7pm to 9pm','10pm to 11pm'))


All_Trips3<- All_Trips2 %>% 
  mutate(Weekday = wday(Start_Time, label = TRUE)) %>% 
  group_by(Usertype, Weekday) %>% 
  summarise(No_of_rides = n(),Avg_Duration = mean(Ride_Length)) %>% #Calculates the number of rides and average duration(secs) of the rides
  arrange(Usertype, Weekday) #sorts the data

rides_ToD<- aggregate(All_Trips2$Ride_Length ~ All_Trips2$Usertype + All_Trips2$ToD3, FUN = mean) #Average ride length by time of day and usertype
rides_ToD2<- aggregate(All_Trips2$Ride_Length ~ All_Trips2$Usertype + All_Trips2$ToD4, FUN = length) #Number of rides by time of day and usertype
rides_location<- aggregate(All_Trips2$Ride_Length ~ All_Trips2$Usertype+ All_Trips2$From_Station_Name, FUN = length) #Number of rides per Station 
rides_duration<- aggregate(All_Trips2$Ride_Length ~ All_Trips2$Usertype+All_Trips2$DoW, FUN = mean) #Average ride length by Day of week and usertype
rides_ToD3<- aggregate(All_Trips2$Ride_Length ~ All_Trips2$ToD4+All_Trips2$Usertype, FUN = length)
rides_ToD3<- All_Trips2 %>% group_by(ToD4,Usertype) %>% 
  summarise(No_of_Rides = n(),Avg_Duration = mean(Ride_Length)) %>% arrange(ToD4,Usertype) #Gives the number of rides, average ride duration and usertype by Time of day
rides_ToD4<-as.data.frame(rides_ToD3) 


#Step10: Create Visualization: Rider type by DoW, by Time, by Location
#10a. Viz1: Visualizing number of rides per Usertype
All_Trips3 %>% ggplot(aes(x=Weekday, y = No_of_rides, fill = Usertype))+geom_col(position = "dodge")
#10b. Viz2. Visualizing daily average ride length by Usertype
All_Trips3 %>% ggplot(aes(x= Weekday, y= Avg_Duration, fill = Usertype))+ geom_col(position = "dodge")+
  labs(x="Weekday", y = "Average number of Rides", title = "Average Number of Rides by Weekday")
#10c. Viz3. Visualizing the daily number of rides by Usertype
All_Trips3 %>% ggplot(aes(x= Weekday, y= No_of_rides, fill = Usertype))+ geom_col(position = "dodge")+facet_wrap(~Weekday)
#10d. Viz4. Visualizing the number of rides by time of the day
All_Trips2 %>% ggplot()+geom_histogram(bins = 23, mapping = aes(x = ToD2, fill = Usertype, color = Usertype))+
  labs(x="Time of Day (24hr)", y = "No. of Rides", title = "Number of Rides by Time of Day")+scale_fill_brewer(palette = 'Pastel1')
#10d. Viz5. Visualizing the number of rides by time of the day (Bins)
rides_ToD4 %>% ggplot(aes(x=ToD4, y = No_of_Rides, fill = Usertype))+geom_col(position = "dodge")+ 
  labs(x="Time Periods", y = "No. of Rides", title = "Number of Rides by Time Period")

#Step11: Export File for Further Analysis
write.csv(rides_ToD, file = '~/Documents/Career/Google Data Analytics/Capstone/Exports/rides_x_ToD.csv')
write.csv(rides_location, file = '~/Documents/Career/Google Data Analytics/Capstone/Exports/rides_x_location.csv')
write.csv(rides_duration, file = '~/Documents/Career/Google Data Analytics/Capstone/Exports/rides_x_duration.csv')
write.csv(rides_ToD4, file = '~/Documents/Career/Google Data Analytics/Capstone/Exports/rides_x_ToD2.csv')

rmarkdown::render("Capstone Presentation.Rmd")







