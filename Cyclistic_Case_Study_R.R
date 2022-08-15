## Install packages and formulas needed for processing and analysis
library(tidyverse)
library(lubridate)
# obtained this formula from https://r-lang.com/mode-in-r/#:~:text=There%20is%20no%20built%2Din,do%20what%20you%20would%20expect.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

## Reading the files into R
june_2022 <- read.csv("202206-divvy-tripdata_copy.csv")
may_2022 <- read.csv("202205-divvy-tripdata_copy.csv")
april_2022 <- read.csv("202204-divvy-tripdata_copy.csv")
march_2022 <- read.csv("202203-divvy-tripdata_copy.csv")
february_2022 <- read.csv("202202-divvy-tripdata_copy.csv")
january_2022 <- read.csv("202201-divvy-tripdata_copy.csv")
december_2021 <- read.csv("202112-divvy-tripdata_copy.csv")
november_2021 <- read.csv("202111-divvy-tripdata_copy.csv")
october_2021 <- read.csv("202110-divvy-tripdata_copy.csv")
september_2021 <- read.csv("202109-divvy-tripdata_copy.csv")
august_2021 <- read.csv("202108-divvy-tripdata_copy.csv")
july_2021 <- read.csv("202107-divvy-tripdata_copy.csv")

## Creating a new variable that gets rid of the basic location data
june_2022_nolocation <-june_2022 %>% select(-c(start_station_name,start_station_id,end_station_name,end_station_id))
may_2022_nolocation <-may_2022 %>% select(-c(start_station_name,start_station_id,end_station_name,end_station_id))
april_2022_nolocation <-april_2022 %>% select(-c(start_station_name,start_station_id,end_station_name,end_station_id))
march_2022_nolocation <-march_2022 %>% select(-c(start_station_name,start_station_id,end_station_name,end_station_id))
february_2022_nolocation <-february_2022 %>% select(-c(start_station_name,start_station_id,end_station_name,end_station_id))
janury_2022_nolocation <-january_2022 %>% select(-c(start_station_name,start_station_id,end_station_name,end_station_id))
december_2021_nolocation <-december_2021 %>% select(-c(start_station_name,start_station_id,end_station_name,end_station_id))
november_2021_nolocation <-november_2021 %>% select(-c(start_station_name,start_station_id,end_station_name,end_station_id))
october_2021_nolocation <-october_2021 %>% select(-c(start_station_name,start_station_id,end_station_name,end_station_id))
september_2021_nolocation <-september_2021 %>% select(-c(start_station_name,start_station_id,end_station_name,end_station_id))
august_2021_nolocation <-august_2021 %>% select(-c(start_station_name,start_station_id,end_station_name,end_station_id))
july_2021_nolocation <-july_2021 %>% select(-c(start_station_name,start_station_id,end_station_name,end_station_id))

## Combine all trips into one df
all_bike_trips <- bind_rows(june_2022_nolocation,may_2022_nolocation,april_2022_nolocation,march_2022_nolocation,february_2022_nolocation,
                            janury_2022_nolocation,december_2021_nolocation,november_2021_nolocation,october_2021_nolocation,september_2021_nolocation,
                            august_2021_nolocation,july_2021_nolocation)



## Sort the data by start time
all_trips_sorted <- arrange(all_bike_trips,started_at)
head(all_trips_sorted)


## Create new columns
all_trips <- mutate(all_trips_sorted, date = as.Date(started_at)) %>% 
  mutate(all_trips_sorted, month = format(as.Date(date),"%m")) %>% 
  mutate(all_trips_sorted, day = format(as.Date(date),"%d")) %>% 
  mutate(all_trips_sorted, year = format(as.Date(date), "%Y")) %>% 
  mutate(all_trips_sorted, day_of_week = format(as.Date(date),"%A")) %>% 
  mutate(all_trips_sorted, ride_length_minutes = round(difftime(all_trips_sorted$ended_at, all_trips_sorted$started_at, units = "mins"),0))

## gets rid of "docked" bike type and negative minutes
all_trips_v2 <- all_trips[!(all_trips$rideable_type == "docked_bike" | all_trips$ride_length<0),]



## Calculate mean of ride_length, max_ride_length, mode day_of_week
data_summary <- all_trips_v2 %>% 
  summarize(avg_ride_length = round(mean(ride_length_minutes),2), max_ride_length = max(ride_length_minutes))
data_summary
mode_day <- getmode(all_trips$day_of_week)
mode_day
## Calculate the average ride_length for members and casual riders
ride_length_member_casual <- all_trips_v2 %>% 
  group_by(member_casual) %>% 
  summarize(avg_ride_length = round(mean(ride_length_minutes),0))
ride_length_member_casual
##  Calculate the average ride_length for users by day_of_week
ride_length_DOW <- all_trips_v2 %>%  
  group_by(member_casual,day_of_week) %>% 
  summarize(avg_ride_length_DOW = round(mean(ride_length_minutes),0))
ride_length_DOW
##Calculate the number of rides for users by day_of_week by adding Count of trip_id
ride_count <- all_trips_v2 %>% 
  group_by(member_casual,day_of_week) %>% 
  summarize(count = n())
ride_count
## Calculate the number of trips users had on each type of bike 
bike_type_trips <- all_trips_v2 %>%  
  group_by(member_casual, rideable_type) %>% 
  summarize(count = n())
bike_type_trips


number_of_trips <- all_trips_v2 %>% 
  summarize(count = n())
number_of_trips

