# Google-Data-Analytics-Capstone-Project
The Bike Share Case Study
## 1. Introduction

Cyclistic is a bike-share program that offers inclusive options such as reclining bikes, hand tricycles, and cargo bikes, making it accessible to people with disabilities. While most riders choose traditional bikes, about 8% use the assistive options. The program is popular for leisure rides, but around 30% of users also rely on it for daily commuting. Cyclistic aims to shift its marketing focus towards converting casual riders into annual members as they have proven to be more profitable. By analyzing historical bike trip data, the marketing team aims to understand the differences between annual members and casual riders, determine why casual riders would consider a membership, and explore the impact of digital media on marketing strategies.

## 2. Scenario

As a junior data analyst on the marketing team at Cyclistic, a bike-share company in Chicago, the director of marketing believes that the company's success hinges on maximizing annual memberships. Your team's objective is to gain an understanding of how casual riders and annual members utilize Cyclistic bikes differently. With these insights, you will develop a new marketing strategy to convert casual riders into annual members. However, your recommendations must be supported by compelling data insights and professional data visualizations in order to obtain approval from Cyclistic executives.

## 3. Data Analysis Process

In this case study, the six steps of the data analysis process will be used in order to solve this problem. Those 6 steps include:
1. Ask
2. Prepare
3. Process
4. Analyze
5. Share
6. Act

## The Key Questions

So here are the analysis questions that will help in the analysis:

1. How do annual members and casual riders use Cyclistic bikes differently?
2. What are the causes that drive casual riders to buy Cyclistic annual memberships?
3. How can Cyclistic use digital media as a tool to convert casual riders to members?

## The Data Given

**Data Source**:  Cyclistic’s historical trip data from Jan 2021 to Dec 2021 which is a public dataset published by Motivate International Inc. will be used to analyze and identify trends.

**Data Information**: In the data source, there are 12 files in total following the naming convention of *"YYYYMM-divvy-tripdata"*. Each file contains data for a specific month, including other details such as ride ID, bike type, start time, end time, start station, end station, start location, end location, and member status. The corresponding column names are:
- ride_id
- rideable_type
- started_at
- ended_at
- start_station_name
- start_station_id
- end_station_name
- end_station_id
- start_lat
- start_lng
- end_lat
- end_lng
- member_casual


## Processing the Data Using R

###Importing the Libraries

```
#load libraries 
library(tidyverse) #calculations
library(lubridate) #dates 
library(hms) #time
library(data.table) #exporting data frame
```

### Loading the data for 2021 monthwise

```
#load original .csv files, a years worth of data from August 2020 to July 2021
aug08_df <- read_csv("202108-divvy-tripdata.csv") 
sep09_df <- read_csv("202109-divvy-tripdata.csv") 
oct10_df <- read_csv("202110-divvy-tripdata.csv")
nov11_df <- read_csv("202111-divvy-tripdata.csv") 
dec12_df <- read_csv("202112-divvy-tripdata.csv")
jan01_df <- read_csv("202101-divvy-tripdata.csv") 
feb02_df <- read_csv("202102-divvy-tripdata.csv") 
mar03_df <- read_csv("202103-divvy-tripdata.csv")
apr04_df <- read_csv("202104-divvy-tripdata.csv")
may05_df <- read_csv("202105-divvy-tripdata.csv") 
jun06_df <- read_csv("202106-divvy-tripdata.csv") 
jul07_df <- read_csv("202107-divvy-tripdata.csv")
```

### Combining it into a single data file and creating a copy dataframe for operations

```
#merge all of the data frames into one year view
combined_df <- rbind (aug08_df, sep09_df, oct10_df, nov11_df, dec12_df, jan01_df, feb02_df, mar03_df, apr04_df, may05_df, jun06_df, jul07_df)

#remove individual month data frames to clear up space in the environment 
remove(aug08_df, sep09_df, oct10_df, nov11_df, dec12_df, jan01_df, feb02_df, mar03_df, apr04_df, may05_df, jun06_df, jul07_df)

combined_date <- combined_df 
```
### Calculating the ride time for each bike share ride
```
#calculate ride length by subtracting ended_at time from started_at time and converted it to minutes
combined_date$ride_length <- difftime(combined_df$ended_at, combined_df$started_at, units = "mins")
```
### Creating specific columns required for our analysis -
  Weekday
  Month
  Day
  Hour
  Time
  

```
#create columns for: day of week, month, day, year, time, hour
combined_date$date <- as.Date(combined_date$started_at) #default format is yyyy-mm-dd, use start date
combined_date$day_of_week <- wday(combined_date$started_at) #calculate the day of the week 
combined_date$day_of_week <- format(as.Date(combined_date$date), "%A") #create column for day of week
combined_date$month <- format(as.Date(combined_date$date), "%m")#create column for month
combined_date$day <- format(as.Date(combined_date$date), "%d") #create column for day
combined_date$year <- format(as.Date(combined_date$date), "%Y") #create column for year
combined_date$time <- format(as.Date(combined_date$date), "%H:%M:%S") #format time as HH:MM:SS
combined_date$time <- as_hms((combined_date$started_at)) #create new column for time
combined_date$hour <- hour(combined_date$time) #create new column for hour

```
### Creating the column for Seasons

```
#create column for different seasons: Spring, Summer, Fall, Winter
combined_date <-combined_date %>% mutate(season = 
                                             case_when(month == "03" ~ "Spring",
                                                       month == "04" ~ "Spring",
                                                       month == "05" ~ "Spring",
                                                       month == "06"  ~ "Summer",
                                                       month == "07"  ~ "Summer",
                                                       month == "08"  ~ "Summer",
                                                       month == "09" ~ "Fall",
                                                       month == "10" ~ "Fall",
                                                       month == "11" ~ "Fall",
                                                       month == "12" ~ "Winter",
                                                       month == "01" ~ "Winter",
                                                       month == "02" ~ "Winter"))

```
### Creating the column for time of the day classification

```
#create column for different time_of_day: Night, Morning, Afternoon, Evening
combined_date <-combined_date %>% mutate(time_of_day = 
                                             case_when(hour == "0" ~ "Night",
                                                       hour == "1" ~ "Night",
                                                       hour == "2" ~ "Night",
                                                       hour == "3" ~ "Night",
                                                       hour == "4" ~ "Night",
                                                       hour == "5" ~ "Night",
                                                       hour == "6" ~ "Morning",
                                                       hour == "7" ~ "Morning",
                                                       hour == "8" ~ "Morning",
                                                       hour == "9" ~ "Morning",
                                                       hour == "10" ~ "Morning",
                                                       hour == "11" ~ "Morning",
                                                       hour == "12" ~ "Afternoon",
                                                       hour == "13" ~ "Afternoon",
                                                       hour == "14" ~ "Afternoon",
                                                       hour == "15" ~ "Afternoon",
                                                       hour == "16" ~ "Afternoon",
                                                       hour == "17" ~ "Afternoon",
                                                       hour == "18" ~ "Evening",
                                                       hour == "19" ~ "Evening",
                                                       hour == "20" ~ "Evening",
                                                       hour == "21" ~ "Evening",
                                                       hour == "22" ~ "Evening",
                                                       hour == "23" ~ "Evening")
)

```
### Cleaning the Data: We follow the steps given below
  * To delete rows with null values
  * To remove ride data with ride length less than or equal to 0
  * To remove unwanted columns from analysis

```
#clean the data
combined_date <- na.omit(combined_date) #remove rows with NA values
combined_date <- distinct(combined_date) #remove duplicate rows 
combined_date <- combined_date[!(combined_date$ride_length <=0),] #remove where ride_length is 0 or negative
combined_date <- combined_date %>%  #remove columns not needed: ride_id, start_station_id, end_station_id, start_lat, start_long, end_lat, end_lng
  select(-c(start_station_id, end_station_id,start_lat,start_lng,end_lat,end_lng)) 

```
### We view the data and download it as a CSV for EDA and dashboarding

```
#view the final data
View(combined_date)

#download the new data as a .csv file
fwrite(combined_date,"bikeshare_data.csv")

```
### Explorig the cleaned dataset

The total number of rides are given by 
```

#total number of rides
nrow(combined_date)

```

The number of casual rides and member rides on different bike types are

```
#total rides by member type 
combined_date %>%
  group_by(member_casual, rideable_type) %>% 
  count(rideable_type)

```
The different ride types available are

```
combined_date %>%
  group_by(rideable_type) %>% 
  count(rideable_type)
```

The number of rides per each hour of day and the rides by member types are given by

```

#total rides by member type 
combined_date %>%
  group_by(member_casual) %>% 
  count(hour) %>% 
  print(n = 48) #lets you view the entire tibble

#total rides
combined_date %>%
  count(hour) %>% 
  print(n = 24) #lets you view the entire tibble

```

The number of rides per time of day and the rides by member types are given by

```
#-----morning-------
#total rides by member type 
combined_date %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

#total rides
combined_date %>%
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

#-----afternoon-------
#total rides by member type 
combined_date %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

#total rides 
combined_date %>%
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

#-----evening-------
#total rides by member type
combined_date %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

#total rides
combined_date %>%
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

#-----night-------
#number of rides by member type
combined_date %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

#number of rides 
combined_date %>%
  filter(time_of_day == "Night") %>% 
  count(time_of_day)


#---all times of day----
#total rides by member type 
combined_date %>%
  group_by(member_casual) %>% 
  count(time_of_day)

#number of rides
combined_date %>%
  group_by(time_of_day) %>% 
  count(time_of_day)
```


The number of rides per the day of the week and the rides by member types are given by

```
#----------------DAY OF THE WEEK------------------

#total rides by member type
combined_date %>%
  group_by(member_casual) %>% 
  count(day_of_week)

#total rides 
combined_date %>%
  count(day_of_week)
```

The number of rides per the month and the rides by member types are given by

```
#---------------------MONTH-----------------------

#total rides by member type 
combined_date %>%
  group_by(member_casual) %>% 
  count(month) %>% 
  print(n = 24) #lets you view the entire tibble

#total rides
combined_date %>%
  count(month)
```


The number of rides per day of the month and the rides by member types are given by

```
#----------------DAY OF THE MONTH-----------------

#total rides by member type
combined_date %>%
  group_by(member_casual) %>% 
  count(day) %>% 
  print(n = 62) #lets you view the entire tibble

#total rides
combined_date %>%
  count(day) %>% 
  print(n = 31) #lets you view the entire tibble
```

The number of rides per season and the rides by member types are given by
```
#--------------------SEASON-----------------------

#-----spring-------

#total rides by member type 
combined_date %>%
  group_by(member_casual) %>% 
  filter(season == "Spring") %>% 
  count(season)

#total rides
combined_date %>%
  filter(season == "Spring") %>% 
  count(season)
#-----summer-------

#total rides by member type
combined_date %>%
  group_by(member_casual) %>% 
  filter(season == "Summer") %>% 
  count(season)

#total rides
combined_date %>%
  filter(season == "Summer") %>% 
  count(season)

#-----fall-------

#total rides by member type
combined_date %>%
  group_by(member_casual) %>% 
  filter(season == "Fall") %>% 
  count(season)

#total rides
combined_date %>%
  filter(season == "Fall") %>% 
  count(season)

#-----winter-------

#total rides by member type
combined_date %>%
  group_by(member_casual) %>% 
  filter(season == "Winter") %>% 
  count(season)

#total rides 
combined_date %>%
  filter(season == "Winter") %>% 
  count(season)

#-----all seasons-------

#total rides by member type
combined_date %>%
  group_by(season, member_casual) %>% 
  count(season)

#total rides
combined_date %>%
  group_by(season) %>% 
  count(season)


```

The average ride length or duration computed by

```
#------------------------------------AVERAGE RIDE LENGTH-----------------------------------

#average of ride_length
cyclistic_avgRide <- mean(combined_date$ride_length)
print(cyclistic_avgRide)
```

The average ride duration by member type and ride type

```
#------------------MEMBER TYPE--------------------

#average ride_length
combined_date %>% group_by( member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----------------TYPE OF BIKE---------------------

#total rides by member type 
combined_date %>% group_by(member_casual, rideable_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride_length
combined_date %>% group_by(rideable_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))
```

The average ride duration by hour of day

#-----------------------HOUR-------------------------
```
#average ride_length by member type
combined_date %>% group_by(hour, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=48) #lets you view entire tibble

#average ride_length
combined_date %>% group_by(hour) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=24) #lets you view entire tibble
```
The average ride duration by time of day

```
#--------------------TIME OF DAY---------------------

#----morning----

#average ride length by member type
combined_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Morning") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
combined_date %>% 
  filter(time_of_day == "Morning") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----afternoon----

#average ride length by member type
combined_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
combined_date %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----evening----

#average ride length by member type
combined_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Evening") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
combined_date %>% 
  filter(time_of_day == "Evening") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----night----

#average ride length by member type 
combined_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Night") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
combined_date %>% 
  filter(time_of_day == "Night") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#---all times of day---

#average ride length by member type
combined_date %>% 
  group_by(time_of_day, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
combined_date %>% 
  group_by(time_of_day) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))
```

The average ride duration by day of week
```
#-------------------DAY OF THE WEEK-----------------

#average ride_length by member type
combined_date %>% group_by(member_casual, day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride_length 
combined_date %>% group_by(day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))
```

The average ride duration by day of month
```
#-----------------DAY OF THE MONTH------------------

#average ride_length by member type
combined_date %>% group_by(day, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=62)  #lets you view entire tibble

#average ride_length
combined_date %>% group_by(day) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=31)  #lets you view entire tibble
```
The average ride duration by month
```
#---------------------MONTH--------------------------

#average ride_length by member type
combined_date %>% group_by(month, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=24)  #lets you view entire tibble

#average ride_length
combined_date %>% group_by(month) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))
```
The average ride length by Season

```

#----------------------SEASON-------------------------

#-----spring------

#average ride length by member type
combined_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Spring") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
combined_date %>% 
  filter(season == "Spring") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----summer------

#average ride length by member type for summer 
combined_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Summer") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length for summer 
combined_date %>% 
  filter(season == "Summer") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----fall------

#average ride length by member type
combined_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Fall") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
combined_date %>% 
  filter(season == "Fall") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----winter-----

#average ride length by member type
combined_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Winter") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
combined_date %>% 
  filter(season == "Winter") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----all seasons----

#average ride length by member type
combined_date %>% 
  group_by(season, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length

### Using the cleaned data for Visualizations using Power BI


combined_date %>% 
  group_by(season) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))
```

## Using Power BI for visualization we could observe

### Summary

![image](https://github.com/paulalias452/Google-Data-Analytics-Capstone-Project/assets/88573857/5f2c1342-7ddc-43a2-9092-abb6da65c547)

We could see that after cleaning the data, the number of rides we had in the year 2021 was 4.51 Mn with average duration being 21.94 minutes. The Summers were the busiest and users preferred to ride in the afternoon and Saturday was the weekday with highest rides. 

![image](https://github.com/paulalias452/Google-Data-Analytics-Capstone-Project/assets/88573857/491bd2a1-b599-4987-a6cd-76d8a9940006)

1) The total number of rides follow a pattern where it peaks over the summer season and falls over the winters implying the effect of weather and climate on the app usage.

![image](https://github.com/paulalias452/Google-Data-Analytics-Capstone-Project/assets/88573857/04e7c4e7-4cae-4ee8-a26f-b831e01abfa6)

2) The users preferred a classic bike much more than the Docked or electric bikes and it can be observed that only the casual riders use the docked bikes.

![image](https://github.com/paulalias452/Google-Data-Analytics-Capstone-Project/assets/88573857/5cc2c319-09de-401a-8099-a4aed7624e92)

3) The number of classic bike used are thrice the amount of the electric bikes and the docked bikes were used only 1/10 th of the casual bikes.

![image](https://github.com/paulalias452/Google-Data-Analytics-Capstone-Project/assets/88573857/62eeb537-6fa4-41ac-b03e-b4c08e74ef7e)

4) We can see that the afternoon is the most preferred time of app usage and riders actively using the bike

![image](https://github.com/paulalias452/Google-Data-Analytics-Capstone-Project/assets/88573857/53262e15-b679-4148-a542-ce9042774225)

5) Saturday and Sunday shows the highest number of rides among the days of the week. It also shows the distribution of casual and member riders showing member riders commuting on the weekdays and casual riders dominant over the weekends.

![image](https://github.com/paulalias452/Google-Data-Analytics-Capstone-Project/assets/88573857/7c12591d-e368-4fa8-ad33-3f6281c36076)

6) Summers are the most preferred time of the year implying the most usage in the season and creating the perfect time for advertising and marketing opportunities.














