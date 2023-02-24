#Analyzing the distance traveled using GPS information
max(cyclisticdfe$distance)#we found a data point of 9,814,069 meters. The longest linear distance in Chicago is approximately 50 km.
which.max(cyclisticdfe$distance)#5162872 is the record
view(cyclisticdfe[5162872,])#This corresponds to record number 5162872, which appears to be complete.Therefore, it is likely a GPS error
sum(cyclisticdfe$distance>50000) #There were a total of 9 records with a distance greater than 50 km,
view(cyclisticdfe[cyclisticdfe$distance>50000,])#  and 8 of them were associated with the same station and all occurred on the same day. We assume there was a GPS failure, and therefore, 
cyclisticdfe <- cyclisticdfe[cyclisticdfe$distance<50000,]#we will remove those records.

#Analyzing ride time
max(cyclisticdfe$ride_length)#34354.07 min, equivalent to 60 hours
sum(cyclisticdfe$ride_length>1440)#206 records with time greater than a day
sum(cyclisticdfe$ride_length>2880)#36 records with time greater than 2 days
view(cyclisticdfe[cyclisticdfe$ride_length>2880,])#The 36 records belong to casual members and with docked bikes
sum(cyclisticdfe$ride_length>1440 & cyclisticdfe$rideable_type == "docked_bike")#103, implies that it's not a bike type error
sum(cyclisticdfe$ride_length>1440 & cyclisticdfe$member_casual == "member")#41, implies that it's not a characteristic of casual members
sum(cyclisticdfe$ride_length>720)#2098 records with time greater than 12 hours
sum(cyclisticdfe$ride_length>360)#4637 records with time greater than 6 hours
sum(cyclisticdfe$ride_length>120)#39521 ecords with time greater than 2 hours, less than 1.5% of total observations
cyclisticdfe <- cyclisticdfe[cyclisticdfe$ride_length<=120,]#Deleting those records

#Removing the extreme cases allows us to perform statistics on the data.
mean(cyclisticdfe$distance)#2123.682
mean(cyclisticdfe$ride_length)#14,73015

#We save the tables to be able to make the charts in Tableau.
write.csv(cyclisticdfe, "cyclistic.csv", row.names = FALSE)
cyclisticdfe_casual <- subset(cyclisticdfe, member_casual == "casual")
cyclisticdfe_member <- subset(cyclisticdfe, member_casual == "member")
write.csv(cyclisticdfe_casual, "cyclistic_casual.csv", row.names = FALSE)
write.csv(cyclisticdfe_member, "cyclistic_member.csv", row.names = FALSE)

#We will use a criteria to remove the data that corresponds to changing bikes at the same station, such as trips of less than 100 m and less than 5 minutes.
sum(cyclisticdfe$distance < 100 & cyclisticdfe$ride_length < 5)#200351 , 3% of the total observations are trips classified as bike change.
sum(cyclisticdfe$distance < 100 & cyclisticdfe$ride_length >= 5)#198068 are cyclic trips that return the bike to the same station.

hist(cyclisticdfe$distance,breaks = 10)#This histogram reveals that the majority of trips are less than 5000 meters.
sum(cyclisticdfe$distance>5000)#443734 almost 8% of the observations, can be a differentiator.
sum(cyclisticdfe$member_casual == "member")#3338776
sum(cyclisticdfe$member_casual == "casual")#2283022
sum(subset(cyclisticdfe, member_casual == "member")$distance >5000)#262424 almost 8%
sum(subset(cyclisticdfe, member_casual == "casual")$distance >5000)#181310 almost 8%, there is no difference.

cyclisticdfe_casual$ride_length <- as.numeric(cyclisticdfe_casual$ride_length)
cyclisticdfe_member$ride_length <- as.numeric(cyclisticdfe_member$ride_length)
max(cyclisticdfe$distance)# 30507
max(cyclisticdfe_member$distance)#30315.46
max(cyclisticdfe_casual$distance)#30507 There is no big difference in the maximums.

hist(cyclisticdfe$distance, breaks= 30)#The bars represent 1km, most of the trips are between 0 and 2 km
hist(cyclisticdfe_casual$distance, breaks= 30)#The bars represent 1km, most of the trips are between 0 and 2 km
hist(cyclisticdfe_member$distance, breaks= 30)#The bars represent 1km, most of the trips are between 0 and 2 km
#There is no difference in the distances traveled by members


hist(cyclisticdfe$ride_length, breaks= 60)#The bars represent 2 min, most trips last between 0 and 20 minutes
hist(cyclisticdfe_casual$ride_length, breaks= 60)#The bars represent 2 min, most trips last between 0 and 20 minutes
hist(cyclisticdfe_member$ride_length, breaks= 60)#The bars represent 2 min, most trips last between 0 and 20 minutes
#There seems to be a difference in the distribution of the data with respect to time
mean(cyclisticdfe$ride_length)#14.73015
mean(cyclisticdfe_casual$ride_length)#18.87006
mean(cyclisticdfe_member$ride_length)#11.89933
sd(cyclisticdfe_casual$ride_length)#18.61511
sd(cyclisticdfe_member$ride_length)#10.49575
#Most of the trips taken by members last from 0 to 21 minutes while most of the trips taken by casual riders last between 0 and 38 minutes.


#Let's analyze the number of trips per month
cyclisticdfe$month <- as.numeric(cyclisticdfe$month)
cyclisticdfe_casual$month <- as.numeric(cyclisticdfe_casual$month)
cyclisticdfe_member$month <- as.numeric(cyclisticdfe_member$month)
hist(cyclisticdfe$month,breaks=12)#The bars represent months, June, July, and August have the most trips
hist(cyclisticdfe_casual$month,breaks=12)#The months with the most trips are the same, but there is a drastic drop in the number of trips in the first three and last three months of the year
hist(cyclisticdfe_member$month,breaks=12)#The bars represent months, May, June, July, and August have the most trips
#There is a significant difference in the number of trips in the first three and last three months of the year.

#Let's analyze the data by day of the month
cyclisticdfe$day <- as.numeric(cyclisticdfe$day)
cyclisticdfe_casual$day <- as.numeric(cyclisticdfe_casual$day)
cyclisticdfe_member$day <- as.numeric(cyclisticdfe_member$day)
hist(cyclisticdfe$day,breaks=31)# The bars represent days of the month. The first day of each month has a significant difference compared to the other days.
hist(cyclisticdfe_casual$day,breaks=31)#The bars represent days of the month. The first day of each month has a significant difference compared to the other days.
hist(cyclisticdfe_member$day,breaks=31)#The bars represent days of the month. The first day of each month has a significant difference compared to the other days.
#All graphs look similar, but the first day of each month represents an opportunity for advertising.

#Let's analyze the data by day of the week.
table_day_of_week <- table(cyclisticdfe$day_of_week)
barplot(table_day_of_week, main = "Viajes por dia",
        xlab = "Día", ylab = "Frecuencia")

table_day_member <- table(cyclisticdfe_member$day_of_week)
barplot(table_day_member, main = "Viajes por dia members",
        xlab = "Día", ylab = "Frecuencia",ylim=c(200000,max(table_day_member)*1.1))
#members have a stable behavior, with only fewer rides on Sundays.
table_day_casual <- table(cyclisticdfe_casual$day_of_week)
barplot(table_day_casual, main = "Viajes por dia casual",
        xlab = "Día", ylab = "Frecuencia",ylim=c(200000,max(table_day_casual)*1.1))
#There is a significant difference between trips on Saturday and Sunday. Tuesday has the fewest trips.
