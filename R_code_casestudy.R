#load libraries 
library(tidyverse) #calculations
library(lubridate) #dates 
library(hms) #time
library(data.table) #exporting data frame

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

#merge all of the data frames into one year view
combined_df <- rbind (aug08_df, sep09_df, oct10_df, nov11_df, dec12_df, jan01_df, feb02_df, mar03_df, apr04_df, may05_df, jun06_df, jul07_df)

#remove individual month data frames to clear up space in the environment 
remove(aug08_df, sep09_df, oct10_df, nov11_df, dec12_df, jan01_df, feb02_df, mar03_df, apr04_df, may05_df, jun06_df, jul07_df)

combined_date <- combined_df 

#calculate ride length by subtracting ended_at time from started_at time and converted it to minutes
combined_date$ride_length <- difftime(combined_df$ended_at, combined_df$started_at, units = "mins")

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

#clean the data
combined_date <- na.omit(combined_date) #remove rows with NA values
combined_date <- distinct(combined_date) #remove duplicate rows 
combined_date <- combined_date[!(combined_date$ride_length <=0),] #remove where ride_length is 0 or negative
combined_date <- combined_date %>%  #remove columns not needed: ride_id, start_station_id, end_station_id, start_lat, start_long, end_lat, end_lng
  select(-c(start_station_id, end_station_id,start_lat,start_lng,end_lat,end_lng)) 

#view the final data
View(combined_date)

#download the new data as a .csv file
fwrite(combined_date,"bikeshare_data.csv")

#-----------------------------------------TOTAL RIDES--------------------------------------

#total number of rides
nrow(combined_date)

#-----------------MEMBER TYPE---------------------
combined_date %>%
  group_by(member_casual) %>% 
  count(member_casual)

#----------------TYPE OF BIKE---------------------

#total rides by member type 
combined_date %>%
  group_by(member_casual, rideable_type) %>% 
  count(rideable_type)

#total rides 
combined_date %>%
  group_by(rideable_type) %>% 
  count(rideable_type)

#-------------------HOUR--------------------------

#total rides by member type 
combined_date %>%
  group_by(member_casual) %>% 
  count(hour) %>% 
  print(n = 48) #lets you view the entire tibble

#total rides
combined_date %>%
  count(hour) %>% 
  print(n = 24) #lets you view the entire tibble

#----------------------TIME OF DAY-----------------------

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


#----------------DAY OF THE WEEK------------------

#total rides by member type
combined_date %>%
  group_by(member_casual) %>% 
  count(day_of_week)

#total rides 
combined_date %>%
  count(day_of_week)

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

#---------------------MONTH-----------------------

#total rides by member type 
combined_date %>%
  group_by(member_casual) %>% 
  count(month) %>% 
  print(n = 24) #lets you view the entire tibble

#total rides
combined_date %>%
  count(month) 

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


#------------------------------------AVERAGE RIDE LENGTH-----------------------------------

#average of ride_length
cyclistic_avgRide <- mean(combined_date$ride_length)
print(cyclistic_avgRide)

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

#-----------------------HOUR-------------------------

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

#-------------------DAY OF THE WEEK-----------------

#average ride_length by member type
combined_date %>% group_by(member_casual, day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride_length 
combined_date %>% group_by(day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

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
combined_date %>% 
  group_by(season) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))
