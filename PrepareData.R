#Prepare data

library(tidyverse)
library(lubridate)
library(hms)
library(dplyr)

#load original .csv files, from january 2022 to december 2022
dfjan2022 <- read_csv("202201Data.csv") 
dffeb2022 <- read_csv("202202Data.csv") 
dfmar2022 <- read_csv("202203Data.csv")
dfapr2022 <- read_csv("202204Data.csv") 
dfmay2022 <- read_csv("202205Data.csv")
dfjun2022 <- read_csv("202206Data.csv") 
dfjul2022 <- read_csv("202207Data.csv") 
dfaug2022 <- read_csv("202208Data.csv")
dfsep2022 <- read_csv("202209Data.csv")
dfoct2022 <- read_csv("202210Data.csv") 
dfnov2022 <- read_csv("202211Data.csv") 
dfdec2022 <- read_csv("202212Data.csv") 

#Merge the files in one

dfcyclistic <- rbind (dfjan2022,dffeb2022,dfmar2022,dfapr2022,dfmay2022,dfjun2022,dfjul2022,dfaug2022,dfsep2022,dfoct2022,dfnov2022,dfdec2022)
#Clear space in the enviroment by removing the individuals files
remove(dfjan2022,dffeb2022,dfmar2022,dfapr2022,dfmay2022,dfjun2022,dfjul2022,dfaug2022,dfsep2022,dfoct2022,dfnov2022,dfdec2022)
#Make a copy of the original source to work
cyclisticdfe <- dfcyclistic
#view the data heads
names(cyclisticdfe)
#Let's see how many of the data we can use without modifying the dataset
howmanystations <- length(unique(cyclisticdfe$start_station_name)) # 1675 vs 692 reported
howmanystationsid <- length(unique(cyclisticdfe$start_station_id)) # 1314 vs 692 reported
howmanybikestypes <- length(unique(cyclisticdfe$rideable_type))#3 de 3 reported 
howmanymembertypes <- length(unique(cyclisticdfe$member_casual))# 2 de 2 reported
#From the results, we conclude that the columns that have the station names are not reliable
#We must rely on the GPS data, and we can only analyze dates, duration, and distances using this dataset
#Format the dataset

cyclisticdfe$date <- as.Date(cyclisticdfe$started_at) #default format is yyyy-mm-dd, use start date
cyclisticdfe$day_of_week <- wday(cyclisticdfe$started_at) #calculate the day of the week 
cyclisticdfe$day_of_week <- format(as.Date(cyclisticdfe$date), "%A") #create column for day of week
cyclisticdfe$month <- format(as.Date(cyclisticdfe$date), "%m")#create column for month
cyclisticdfe$day <- format(as.Date(cyclisticdfe$date), "%d") #create column for day
cyclisticdfe$year <- format(as.Date(cyclisticdfe$date), "%Y") #create column for year
cyclisticdfe$time <- format(as.Date(cyclisticdfe$date), "%H:%M:%S") #format time as HH:MM:SS
cyclisticdfe$time <- as_hms((cyclisticdfe$started_at)) #create new column for time
cyclisticdfe$hour <- hour(cyclisticdfe$time) #create new column for hour

cyclisticdfe$ride_length <- difftime(dfcyclistic$ended_at, dfcyclistic$started_at, units = "mins")
cyclisticdfe$dlat <- (abs(cyclisticdfe$end_lat - cyclisticdfe$start_lat))*pi/180 #create new column for hour
cyclisticdfe$dlng <- (abs(cyclisticdfe$end_lng - cyclisticdfe$start_lng))*pi/180
cyclisticdfe$da <- (sin(cyclisticdfe$dlat/2))^2+cos(cyclisticdfe$start_lat*pi/180)*cos(cyclisticdfe$end_lat*pi/180)*(sin(cyclisticdfe$dlng/2))^2
cyclisticdfe$distance <- 6371000*2*atan(sqrt(cyclisticdfe$da)/sqrt(1-cyclisticdfe$da))

cyclisticdfe <- cyclisticdfe %>%  #remove columns not needed: ride_id, start_station_id, end_station_id, start_lat, start_long, end_lat, end_lng
  select(-c(ride_id, start_station_id, end_station_id,start_lat,start_lng,end_lat,end_lng,dlat,dlng,da))

#Looking for bias in the information
sum(is.na(cyclisticdfe)) #1731664 of 5667717 the 30% of observation has NA
#Let's search for bias in the data that interests us.
sum(is.na(cyclisticdfe$member_casual))#0 all entries have this data.
sum(is.na(cyclisticdfe$ride_length))#0 all entries have this data.
sum(is.na(cyclisticdfe$rideable_type))#0 all entries have this data.
sum(is.na(cyclisticdfe$distance))#5858 of 5667717 entries do not have this data.
sum(is.na(cyclisticdfe$started_at))#0 all entries have this data.

#It would be interesting to have information about the location of the trips, let's analyze the data by month.
dfjan2022 <- read_csv("202201Data.csv")
howmanystationsjan <- length(unique(dfjan2022$start_station_name))#759 de 692 reported
dffeb2022 <- read_csv("202202Data.csv")
howmanystationsfeb <- length(unique(dffeb2022$start_station_name))#780 de 692 reported
#We conclude that the start station column is not a reliable column and we cannot use this information for analysis.
#For the analysis, we will only use the duration of the trip, distance, start time, date, and bike type.
remove(dffeb2022,dfjan2022,howmanystationsfeb,howmanystationsjan,howmanybikestypes,howmanymembertypes,howmanystations,howmanystationsid)

