library("tidyverse")
library("ggplot2")
library("lubridate")
library("skimr")
library("janitor")
library("SimDesign")

january <- read_csv("202201-divvy-tripdata.csv")
february <- read_csv("202202-divvy-tripdata.csv")
march <- read_csv("202203-divvy-tripdata.csv")
april <- read_csv("202204-divvy-tripdata.csv")
may <- read_csv("202205-divvy-tripdata.csv")
june <- read_csv("202206-divvy-tripdata.csv")
july <- read_csv("202207-divvy-tripdata.csv")
august <- read_csv("202208-divvy-tripdata.csv")
september <- read_csv("202209-divvy-publictripdata.csv")
october <- read_csv("202210-divvy-tripdata.csv")
november <- read_csv("202211-divvy-tripdata.csv")
december <- read_csv("202212-divvy-tripdata.csv")

#glimpse(january)
#View(january)

# Checking the column names for the same names and sizes. 

colnames(january)
colnames(february)
colnames(march)
colnames(april)
colnames(may)
colnames(june)
colnames(july)
colnames(august)
colnames(september)
colnames(october)
colnames(november)
colnames(december)


# Checking the separate data sources to ensure that they are able to be joined

str(january)
str(february)
str(march)
str(april)
str(may)
str(june)
str(july)
str(august)
str(september)
str(october)
str(november)
str(december)


######################################################################
# 3. PROCESS
######################################################################

# Combining the data to get the entire year
all_trips_data <- rbind(january,february,march,april,may,june,july,august,september,
                  october,november,december)

colnames(all_trips_data)  #List of column names
nrow(all_trips_data)  #How many rows are in data frame?
dim(all_trips_data)  #Dimensions of the data frame?
head(all_trips_data)  #See the first 6 rows of data frame.  Also tail(qs_raw)
str(all_trips_data)  #See list of columns and data types (numeric, character, etc)
summary(all_trips_data)  #Statistical summary of data. Mainly for numerics
skim_without_charts(all_trips_data)

# reveals ~15% missing data from start/end station name and ID
# is that just missing data? or is there the option to pick and drop bikes ?
# Hard to say for sure, not enough documentation so remove all.

# There are a few problems we will need to fix:
# (1) The data can only be aggregated at the ride-level, which is too granular. We will to add some additional columns data such as weekday and month..
# (2) We will want to add a calculated field for length of ride since none of the data has a "tripduration" column.
# (3) There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.


# Adding new features 

all_trips_data$trip_duration_minutes <- round(as.numeric(difftime(all_trips_data$ended_at, all_trips_data$started_at, units = "mins")), 2) # Duration of trip
all_trips_data$day_of_week <- weekdays(all_trips_data$started_at) # Day that ride started at
all_trips_data$month <- month(all_trips_data$started_at) # Month that ride started at
all_trips_data$hour <- hour(all_trips_data$started_at) # Hour that ride started at
all_trips_data$trip_name <-    ifelse(all_trips_data$start_station_name == all_trips_data$end_station_name,
                                       paste("Round Trip from", all_trips_data$start_station_name),
                                       paste(all_trips_data$start_station_name, "to", all_trips_data$end_station_name)) # Trip route name


# Cleaning Data

all_trips_data %>%
  clean_names() %>%
  get_dupes()

all_trips_data <- all_trips_data %>% 
  rename("start_time" = "started_at", "end_time" = "ended_at", "user_type" = "member_casual") %>% #rename columns
  drop_na() %>% #drop rows with missing values
  filter(trip_duration_minutes >= 1, trip_duration_minutes <= 1440) #Set trip duration limits 

# Dropping station ID columns
all_trips_data_cleaned = subset(all_trips_data, select = -c(start_station_id, end_station_id))

write.csv(all_trips_data_cleaned, "C:/Users/tycym/OneDrive/Documents/Data Science/Personal Projects/Google Capstone/final_data.csv", row.names = FALSE)

########################################################################
# 4. ANALYZE
########################################################################


# Getting basic statistics from assignment 
summary(all_trips_data_cleaned$trip_duration_minutes)

# Compare members and casual users
aggregate(all_trips_data_cleaned$trip_duration_minutes ~ all_trips_data_cleaned$user_type, FUN = mean)
aggregate(all_trips_data_cleaned$trip_duration_minutes ~ all_trips_data_cleaned$user_type, FUN = median)
aggregate(all_trips_data_cleaned$trip_duration_minutes ~ all_trips_data_cleaned$user_type, FUN = max)
aggregate(all_trips_data_cleaned$trip_duration_minutes ~ all_trips_data_cleaned$user_type, FUN = min)

# Mode for Day of Week
day_counts <- table(all_trips_data_cleaned$day_of_week, useNA = "ifany")
most_common_day <- names(day_counts[which.max(day_counts)])

# Mode for Trips
trip_counts <- table(all_trips_data_cleaned$trip_name, useNA = "ifany")
most_common_trip <- names(trip_counts[which.max(trip_counts)])

# Average ride length by rider category
aggregate(all_trips_data_cleaned$trip_duration_minutes ~ all_trips_data_cleaned$user_type + all_trips_data_cleaned$day_of_week, FUN = mean)
  

# Average Ride Length by Day of Week
# analyze ridership data by type and weekday
all_trips_data_cleaned %>% 
  group_by(user_type, day_of_week) %>%  #groups by user_type and weekday
  summarise(number_of_rides = n(),average_duration = mean(trip_duration_minutes)) %>% 		# calculates the average duration
  arrange(user_type, day_of_week) 						# sorts

#############################################################################
# VISUALIZE
#############################################################################

#Number of Rides

# Let's visualize the number of rides each weekday by rider type
all_trips_data_cleaned %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(user_type, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(trip_duration_minutes)) %>% 
  arrange(user_type, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = user_type)) +
  geom_col(position = "dodge") + 
  labs(title = "Total Rides by Day of Week", y = "# of Rides", x = "Weekday")

# Number of Rides each month by rider type

all_trips_data_cleaned %>% 
  group_by(user_type, month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(trip_duration_minutes)) %>% 
  arrange(user_type, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, color = user_type)) +
  geom_smooth(method = "auto", position = "dodge", se = FALSE) + 
  theme_minimal()+
  labs(title = "Total Rides by Month", y = "# of Rides", x = "Month")

# Number of rides for users by hour, add Count of Trip_id 

all_trips_data_cleaned %>% 
  group_by(user_type, hour) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(trip_duration_minutes)) %>% 
  arrange(user_type, hour)  %>% 
  ggplot(aes(x = hour, y = number_of_rides, color=user_type))+
  geom_smooth(method = "auto", position = "dodge", se = FALSE) + 
  theme_minimal()+
  labs(title = "Total Rides by Hour", y = "# of Rides", x = "Hour of Day")

# Average Ride Times

# Average duration by Month

all_trips_data_cleaned %>% 
  group_by(user_type, month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(trip_duration_minutes)) %>% 
  arrange(user_type, month)  %>% 
  ggplot(aes(x = month, y = average_duration, color = user_type)) +
  geom_smooth(method = "auto", position = "dodge", se = FALSE) + 
  theme_minimal()+
  labs(title = "Average Duration by Month", y = "Average Duration (Minutes)", x = "Month")

# Average duration by day

all_trips_data_cleaned %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(user_type, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(trip_duration_minutes)) %>% 
  arrange(user_type, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = user_type)) +
  geom_col(position = "dodge") + 
  labs(title = "Average Duration by Day of Week", y = "Average Duration (Minutes)", x = "Weekday")

# Average duration by hour
all_trips_data_cleaned %>% 
  group_by(user_type, hour) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(trip_duration_minutes)) %>% 
  arrange(user_type, hour)  %>% 
  ggplot(aes(x = hour, y = number_of_rides, color=user_type))+
  geom_smooth(method = "auto", position = "dodge", se = FALSE) + 
  theme_minimal()+
  labs(title = "Average Duration of ride by Hour", y = "# of Rides", x = "Hour of Day")
