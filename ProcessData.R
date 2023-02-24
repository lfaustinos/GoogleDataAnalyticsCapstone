#Let's look for inconsistencies in the selected columns
sum(cyclisticdfe$ride_length<=0)#531 of 5667717
sum(cyclisticdfe$distance<0,na.rm = TRUE)#0 because are numbers from the formula for distance
#We also have 5858 trips with NA distance out of 5667717, so we will remove those entries from the dataset.
cyclisticdfe <- cyclisticdfe[cyclisticdfe$ride_length > 0, ]
cyclisticdfe <- cyclisticdfe[complete.cases(cyclisticdfe$distance), ]#6389 records were deleted, which is less than 1% of the data.
# We will not remove any more data because the NA values in the other columns are not important for the analysis. 
#We will add more columns to better classify the information. 
#We added columns to classify by seasons and hours.
cyclisticdfe <-cyclisticdfe %>% mutate(time = 
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

cyclisticdfe <-cyclisticdfe %>% mutate(season = 
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
                                                   month == "02" ~ "Winter")
)


