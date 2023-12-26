
##Load the necessary packages that we will use through the analysis
library(readxl)
library(tidyverse)
library(skimr)
library(janitor)
library(here)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggpubr) 

## Import the datasets that we are going to use

dailyActivity <- read_excel("Data Analytics Lessons/8.The Capstone Project/Case Study 2 - BELLABEAT/DATA/dailyActivity_merged.xlsx")
hourly_steps <- read_excel("C:/Users/ricke/Documents/Data Analytics Lessons/8.The Capstone Project/Case Study 2 - BELLABEAT/DATA/hourlySteps_merged.xlsx")
heartrate_seconds_merged <- read_csv("Data Analytics Lessons/8.The Capstone Project/Case Study 2 - BELLABEAT/DATA/heartrate_seconds_merged.csv")
hourly_intensity <- read_excel("Data Analytics Lessons/8.The Capstone Project/Case Study 2 - BELLABEAT/DATA/hourlyIntensities_merged.xlsx")
hourly_Calories <- read_excel("Data Analytics Lessons/8.The Capstone Project/Case Study 2 - BELLABEAT/DATA/hourlyCalories_merged.xlsx")
sleepDay_merged <- read_excel("Data Analytics Lessons/8.The Capstone Project/Case Study 2 - BELLABEAT/DATA/sleepDay_merged.xlsx")
minuteIntensitiesWide_merged <- read_excel("Data Analytics Lessons/8.The Capstone Project/Case Study 2 - BELLABEAT/DATA/minuteIntensitiesWide_merged.xlsx")


## Manipulation of the daily activity dataset and transformation for the visulalizations
View(dailyActivity) # take a look a the data
head(dailyActivity)
colnames(dailyActivity)
glimpse(dailyActivity)
class(dailyActivity$Id)
dailyActivity$Id <- as.factor(dailyActivity$Id) #convert the user ID to factor
levels(dailyActivity$Id)
colnames(dailyActivity)
sum(duplicated(dailyActivity)) #check for duplicates
# there are no duplicates

# now we create a dataset to see the total active minutes per user and day of the week 
activity_per_day <- dailyActivity %>% 
  select(Id, ActivityDate, VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes) %>% 
  mutate(ActivityDate = weekdays(ActivityDate)) %>% 
  group_by(Id,ActivityDate) %>% 
  summarise(
    VeryActiveMinutes = mean(VeryActiveMinutes),
    FairlyActiveMinutes = mean(FairlyActiveMinutes),
    LightlyActiveMinutes = mean(LightlyActiveMinutes),
    SedentaryMinutes = mean(SedentaryMinutes)
  ) %>% arrange(Id,ActivityDate) 
  view(activity_per_day)


#-------------------------------Create some visuals to understand the acitvity of users for the different days of the week---------------
ggarrange(
ggplot(data = activity_per_day) + 
  geom_col(mapping = aes(x = factor(activity_per_day$ActivityDate, 
                                    levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday" , "Saturday", "Sunday")),
                         y = VeryActiveMinutes), fill = "#980043") + 
  ggtitle("Very Active Minutes through the week") +
  xlab("") + ylab("") + 
  theme(axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(size = 20, color = "#df65b0", hjust = 0.5),
       legend.text = element_blank(),
       legend.background = element_blank(),
       legend.title = element_blank())
  ,
ggplot(data = activity_per_day) +
  geom_col(mapping = aes(x = factor(activity_per_day$ActivityDate, 
                                    levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday" , "Saturday", "Sunday")),
                         y = FairlyActiveMinutes), fill = "#e7298a") + 
  ggtitle("Fairly Active Minutes through the week") +
  xlab("") + ylab("") +
  theme(axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(size = 20, color = "#df65b0", hjust = 0.5),
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.title = element_blank())
        
,
ggplot(data = activity_per_day) +
  geom_col(mapping = aes(x = factor(activity_per_day$ActivityDate, 
                                    levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday" , "Saturday", "Sunday")),
                         y = LightlyActiveMinutes), fill = "#c994c7") + 
  ggtitle("Lightly Active Minutes through the week") +
  xlab("") + ylab("") +
  theme(axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(size = 20, color = "#df65b0", hjust = 0.5),
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.title = element_blank())

,
ggplot(data = activity_per_day) +
  geom_col(mapping = aes(x = factor(activity_per_day$ActivityDate, 
                                    levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday" , "Saturday", "Sunday")),
                         y = SedentaryMinutes), fill = "#e7e1ef") + 
  ggtitle("Sedentary Minutes through the week") +
  xlab("") + ylab("") +
  theme(axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(size = 20, color = "#df65b0", hjust = 0.5),
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.title = element_blank())
        
)





#-------------------------------------Relationship between very active minutes and calories-------------------------------------
ggarrange(
dailyActivity %>% 
ggplot(mapping = aes(x = VeryActiveMinutes, y = Calories)) +
  geom_point() +
  geom_smooth(color = "red") +
  labs(x = "minutes", y = "calories", 
       title = "Very Active Minutes vs Calories") +
  theme_minimal(),
dailyActivity %>% 
  ggplot(mapping = aes(x = SedentaryMinutes, y = Calories)) +
  geom_point() +
  geom_smooth(color = "red") +
  labs(x = "minutes", y = "calories", 
       title = "Sedentary Minutes vs Calories") +
  theme_minimal(),
dailyActivity %>% 
  ggplot(mapping = aes(x = FairlyActiveMinutes, y = Calories)) +
  geom_point() +
  geom_smooth(color = "red") +
  labs(x = "minutes", y = "Calories", 
       title = "Fairly Active Minutes vs Calories") +
  theme_minimal(),
dailyActivity %>% 
  ggplot(mapping = aes(x = LightlyActiveMinutes, y = Calories)) +
  geom_point() +
  geom_smooth(color = "red") +
  labs(x = "minutes", y = "Calories", 
       title = "Lightly Active Minutes vs Calories") +
  theme_minimal()
)


#-----------------------------------------------------  Steps progression throughout the day  ----------------------------

View(hourly_steps)
class(hourly_steps$ActivityHour)  
colnames(hourly_steps)

# Categorize the day into parts of the day
steps <- hourly_steps %>% 
  mutate(part_of_day = case_when(
    between(hour(ActivityHour), 0, 5) ~ "Night",
    between(hour(ActivityHour), 5, 12) ~ "Morning",
    between(hour(ActivityHour), 12, 17) ~ "Afternoon",
    between(hour(ActivityHour), 17, 20) ~ "Evening",
    TRUE ~ "Night" ))
View(steps)

#------------------------------------ Visualize the average steps on the different parts of the day ---------------------
steps_in_day <- steps %>% 
  group_by(part_of_day) %>% 
  mutate(StepTotal = mean(StepTotal)) %>% 
  summarise(total_steps = round(mean(StepTotal))) #group total steps by part of the day
  
#Visualize the result
steps_in_day %>% 
ggplot(aes(x = factor(part_of_day,levels = c("Morning", "Afternoon", "Evening", "Night")),
             y = total_steps, color = part_of_day, fill = part_of_day)) +
  geom_col(width = 0.8, color = "black") + 
  scale_fill_manual(values = c("#feebe2", "#fbb4b9", "#f768a1", "#ae017e")) +
  labs(title = "Average steps in the different parts of the day", x = "", y = "Total steps") +  
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12, color = "#004529"),
        axis.text.y = element_text(size = 12, color = "#004529"),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 30, hjust = 0.5, color = "#980043")) 


#-----------------------------------   Visualize the average steps per hour   ----------------------------------------------------
 
head(hourly_steps)
str(hourly_steps)
sum(duplicated(hourly_steps)) #check for duplicates





# format the data and prepare it for visualization
steps_by_hour <- hourly_steps %>% 
  mutate(Time = format(ActivityHour, format = "%H:%M")) %>% 
  group_by(Time) %>%
  mutate(average_steps = mean(StepTotal)) %>% 
  group_by(Time) %>% 
  summarize(average_steps = sum(average_steps))

# Visualization 

steps_by_hour %>% 
  ggplot(aes(x = Time, y = average_steps, fill = average_steps)) +
  geom_col() + 
  scale_fill_gradient(low = "#fde0dd", high = "#49006a") +
  ggtitle("Average steps per hour") +
  xlab("") + ylab("") + 
  theme_minimal() +
  theme(axis.text.y = element_blank(), 
        legend.position = "none",
        axis.text.x = element_text(angle = 45, size = 10, color = "#49006a", vjust = 0.2),
        plot.title = element_text(size = 22, color = "#49006a", hjust = 0.5, face = "bold.italic"),
        panel.background = element_rect(fill = "#fff7f3"))

         


#----------------Compare heartrate data to intensities data in order to identify stress within users of smart devices --------------------
 

View(heartrate_seconds_merged)
View(minuteIntensitiesWide_merged)

names(heartrate_seconds_merged)
class(heartrate_seconds_merged$Time) # it is a character type, so we need to convert it  
heartrate_seconds_merged$Time <- format(heartrate_seconds_merged$Time, format = "%Y-%m-%d %H:%M:%S")
heartrate_seconds_merged$Time <- as.POSIXct(heartrate_seconds_merged$Time, format = "%m/%d/%Y %I:%M:%S %p")

##Create a dataframe for the hourly heartrate 
hourly_heartrate <- as.data.frame(heartrate_seconds_merged %>%
  mutate(ActivityHour = floor_date(Time, unit = "hour")) %>%
  group_by(Id,ActivityHour) %>%
    arrange(Id,ActivityHour) %>% 
  summarise(heartrate = round(mean(Value)))) 
  View(hourly_heartrate)
  
  class(hourly_heartrate$Id)
  summary(hourly_heartrate$heartrate)


class(minuteIntensitiesWide_merged$ActivityHour)
minuteIntensitiesWide_merged$ActivityHour <- as.POSIXct(minuteIntensitiesWide_merged$ActivityHour, format = "%Y-%m-%d %H:%M:%S")

##Change the intensities table from wide to long range view
minute_intensities_long <- as.data.frame( minuteIntensitiesWide_merged %>%  
  pivot_longer(cols = 3:62,
               names_to = "Minutes",
               values_to = "Intensities") %>% 
  mutate(Minutes = gsub("[^0-9]", "", Minutes)))  
  View(minute_intensities_long)

##Create a dataframe for the hourly intensities
hourly_intensities_long <- as.data.frame(minute_intensities_long %>% 
  group_by(Id,ActivityHour) %>% 
  summarize(Intensities = sum(Intensities))) %>% 
  arrange(Id,ActivityHour) 
  View(hourly_intensities_long)

length(hourly_intensities_long$ActivityHour) 
length(hourly_heartrate$ActivityHour)


##Join the hourly heartrate table with the hourly intensities table 
merged_heartrate_intensity <- inner_join(hourly_heartrate, hourly_intensities_long, by = c("Id", "ActivityHour")) 
View(merged_heartrate_intensity)

nrow(hourly_heartrate)
nrow(hourly_intensities_long)
nrow(merged_heartrate_intensity)
class(merged_heartrate_intensity$ActivityHour)
unique(merged_heartrate_intensity$Intensities)
sum(duplicated(merged_heartrate_intensity))




##Creata a data frame to create a visualization
visuals <- merged_heartrate_intensity %>% 
  mutate(ActivityHour = format(ActivityHour, format = "%H:%M" )) %>%
  group_by(Id,ActivityHour) %>% 
  summarise(Intensities = mean(Intensities),
            heartrate = mean(heartrate))
max(visuals$Intensities)
median(visuals$Intensities)
unique(visuals$Intensities)
View(visuals)


#create a table to classify the intensity into 4 categories based on its value
visuals$Intensities <- factor(
  case_when(
    between(visuals$Intensities, 0, 10) ~ "low_intensity",
    between(visuals$Intensities, 11, 20) ~ "fairly_low_intensity",
    between(visuals$Intensities, 21, 55) ~ "medium_intensity",
    TRUE ~ "high_intensity"
  ))
  
  levels = c("low_intensity", "fairly_low_intensity", 
             "medium_intensity", "slightly_high_intensity", 
             "high_intensity", "very_high_intensity",
  ordered = TRUE)


#-------------------------------------- Heartrate vs Intensities Visualization -------------------------------------

visuals %>% 
  ggplot(aes(x =  ActivityHour, y = Intensities, fill = heartrate)) + 
  theme_minimal()+
  theme(axis.text.x= element_text(angle = 45, color = "#00441b", hjust = 1.2, size = 12),
        axis.text.y = element_text(color = "#00441b", size = 12))+
  scale_fill_gradient(low= "#fa9fb5", high="#7a0177")+
  geom_tile(color= "#00441b",lwd =.6,linetype =1) +
  coord_fixed() +
  theme(plot.title= element_text(hjust= 0.5,vjust= 2.0, size=20, color = "#00441b"), 
        panel.background = element_rect(fill = "#f7fcfd", color = "white", linetype = "solid", borders(fill = "NA"))) +
  labs(title = "Heartrate vs Intensities throughout the day",
      x = "",
      y = " ",
      fill = "Pulse Rate") 


                 

# -----------------------------Let's find out how much did users use their smart devices -----------------------------



View(dailyActivity)
head(dailyActivity)
colnames(dailyActivity)

starting_date <- as.Date("2016-04-12")
ending_date <- as.Date("2016-05-12")

days_passed <- as.numeric(difftime(ending_date, starting_date, units = "days"))
  view(days_passed) #check how many days lasted the observation

#Now we are going to categorize the users by how much they have used their smart devices
#To achieve that, we took into consideration the total records each user had within the observation period

View(daily_usage)
activity <- dailyActivity %>%
  group_by(Id) %>% 
  summarize(total_days = n(), 
            days_not_used = sum(TotalSteps == 0)) %>% 
  mutate(days_used = total_days - days_not_used) %>%  
  mutate( usage_status = case_when(
    days_used <= 10 ~ "Inactive",
    days_used >10 & days_used <= 20 ~ "Fairly active",
    days_used > 20 & days_used <= 25 ~ "Very active",
    days_used > 25 ~ "Super active"
  )) 
View(activity)

# I decided to subtract the days that the users had zero activity, for instance the days they had 0 steps ,
# and 0 intesity. The data defined a specific amount of calories for the users that had zero activity those days. 
# It seems that despite the fact that they have record in specific days they didn't use the smart device. 

# Create a data frame to depict the percentages of each user type
activity_percent <- activity %>% 
  group_by(usage_status) %>% 
  summarize(total = n()) %>% 
  mutate(totals = sum(total)) %>% 
  group_by(usage_status) %>% 
  summarize(total_percent = total / totals) %>% 
  mutate(labels = scales::percent(total_percent))
  
activity_percent$usage_status <- factor(activity_percent$usage_status, levels = c("Inactive", "Fairly active",
                                                                                  "Very active", "Super active"))

##Finally create a visualization to better understand the results
  activity_percent %>% 
    ggplot(aes(x= "", y = total_percent, fill = usage_status)) +
    geom_bar(stat = "identity", width = 1) + 
    coord_polar("y", start = 0) +
    theme_minimal() +
    theme(axis.title.x= element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(), 
          panel.grid = element_blank(), 
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          plot.title = element_text(hjust = 0.5, size=14, face = "bold", color = "#67001f"),
          plot.subtitle = element_text(color = "#ae017e", hjust = 0.5),
          plot.background = element_rect(fill = "#fff7f3")) +
    scale_fill_manual(values = c("#e7e1ef","#c994c7","#e7298a","#980043")) +
    geom_text(aes(label = labels),
              position = position_stack(vjust = 0.5))+
    labs(title="User type distribution: How much did they worn their smart devices",
         subtitle = "Inactive: <10, Fairly active: 10-20 days, Very active: 20-25 days, Super active: >25 days")
       ## In this visualization we see how much did users worn their devices based on the criteria that we defined earlier


#--------------------------- Now we are going to compare intensity and calories -------------------------------------


View(hourly_intensity)
str(hourly_intensity)
glimpse(hourly_intensity)
n_unique(hourly_intensity$Id) #check the number of users
sum(duplicated(hourly_intensity)) #check for duplicates

View(hourly_Calories)
str(hourly_Calories)
glimpse(hourly_Calories)
n_unique(hourly_Calories$Id) #check the number of users
sum(duplicated(hourly_Calories)) #check for duplicates

##Join the two tables together

intensity_calories <- inner_join(hourly_intensity, hourly_Calories, by = c("Id", "ActivityHour"))
View(intensity_calories)
sum(duplicated(intensity_calories)) #check for duplicates

#Create a visualization for the hourly intensities and one for the hourly calories
ggarrange(
intensity_calories %>% 
  mutate(time =  format(ActivityHour, format = "%H:%M:%S")) %>%  
  group_by(time) %>% 
    summarize(intensity = mean(round(TotalIntensity))) %>% 
  ggplot() +
  geom_col(mapping = aes(x = time, y = intensity, fill = intensity)) +
  labs(title = "Average Intensity During the Day", x="", y="") +
  scale_fill_gradient(low = "#ffffcc", high = "#bd0026") +
  theme(axis.text.x = element_text(angle = 90, color = "#800026"),
        axis.text.y = element_text(color = "#800026"),
        plot.title = element_text(color = "#800026"),
        legend.text = element_text(color = "#800026"),
        legend.title = element_text(color = "#800026")),

intensity_calories %>% 
  mutate(time =  format(ActivityHour, format = "%H:%M:%S")) %>%  
  group_by(time) %>% 
  summarize(calories = mean(round(Calories))) %>% 
  ggplot() +
  geom_col(mapping = aes(x = time, y = calories, fill = calories)) +
  labs(title = "Average Calories During the Day", x="",y="") +
  scale_fill_gradient(low = "#bcbddc", high = "#54278f") +
  theme(axis.text.x = element_text(angle = 90, color = "#54278f"),
        axis.text.y = element_text(color = "#54278f"),
        plot.title = element_text(color = "#54278f"),
        legend.text = element_text(color = "#54278f"),
        legend.title = element_text(color = "#54278f")) 
)
  
##Let's see the correlation

ggplot(intensity_calories, mapping = aes(x = TotalIntensity, y = Calories)) +
  geom_jitter() +
  geom_smooth(color = "red") +
  theme_minimal() +
  labs(title = "Intensity vs Calories", x="Calories", y = "Intensity") +
  theme(panel.background = element_blank(),
        plot.title = element_text(size = 16))

#----------------------- Categorize users based of their activity measured by minutes active within 24hours -----------------------

glimpse(dailyActivity)
str(dailyActivity)
n_unique(dailyActivity$Id)    ## 33 unique users
sum(duplicated(dailyActivity)) ## there are no duplicates

activity_tracker <- dailyActivity %>% 
  mutate(total_minutes_active = 
           VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes + SedentaryMinutes) %>% 
 mutate(percentage_minutes_active = (total_minutes_active/1440)*100) %>% 
          mutate(activity_by_day = case_when(percentage_minutes_active == 100 ~ "All day",
                                    percentage_minutes_active >= 50 ~ "More than half day",
                                    percentage_minutes_active < 50 ~ "Less than half day"))
    

View(activity_tracker)
  
# Create a table with the percentages of the activity categories we created in the previous data frame 
# The percentages are for each day of activity recording and not grouped by user.
percentages_of_activity <- activity_tracker %>% 
  group_by(activity_by_day) %>% 
  summarize(total_days = n()) %>% 
  mutate(total_activity = sum(total_days)) %>% 
  group_by(activity_by_day) %>% 
  summarize(perc_of_activity = total_days / total_activity ) %>% 
  mutate(labels = scales::percent(perc_of_activity))
head(percentages_of_activity)

# Users that have 100% worn the device all day(24hours)
# Users that have higher that 50%, worn it for more that a half day
# Users that worn the device less that 50%, worn it for less that a half day
# And know we would like to visualize the result in to a pie chart

percentages_of_activity$active <- factor(percentages_of_activity$activity_by_day,
                                         levels = c("All day", "More than half day", "Less than half day"))

percentages_of_activity %>% 
  ggplot(aes(x= "", y = perc_of_activity, fill = activity_by_day)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_minimal() +
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold", color = "#67001f"),
        legend.title = element_text(color = "#67001f", face = "bold")) +
  scale_fill_manual(values = c("#980043", "#e7298a", "#c994c7")) +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))+
  labs(title="How much the smart device worn within 24hours")


#----------------------------Now it is time to bring the sleep data into the game and explore some trends ----------

View(sleepDay_merged)
str(sleepDay_merged)
sum(duplicated(sleepDay_merged)) ## we have 3 duplicates in our dataset
# Let's remove them
sleepday <- sleepDay_merged %>%  distinct() 
sum(duplicated(sleepday)) ##check if the duplicates were been removed

# Compare time in bed with time asleep visualization
sleepday %>% 
  ggplot(aes(x = TotalTimeInBed, y = TotalMinutesAsleep)) +
  geom_jitter() +
  geom_smooth(color = 'red') +
  labs(title = 'Time in bed vs Time asleep', x = 'Total time in bed', y = 'Total time asleep') +
  theme(panel.background = element_blank()) 

# Transform the sleepday dataset to be able to merge with the activity tracker dataset

head(sleepday)
# Change the name of the variable sleepday to ActivityDate in order to join the two tables appropriately
sleepday$ActivityDate <- sleepday$SleepDay
sleepday$SleepDay <- NULL

str(sleepday)
str(activity_tracker)  
sleepday$Id <- factor(sleepday$Id)

# Join the two tables
activity_and_sleep <- inner_join(activity_tracker, sleepday, by = c("Id", "ActivityDate"))
head(activity_and_sleep)
 
 
str(activity_and_sleep)
glimpse(activity_and_sleep)

##Gain insights about the correlation between sleep, calories, total steps and sedentary minutes
ggarrange(
activity_and_sleep %>% 
  ggplot(aes(x = SedentaryMinutes , y = TotalMinutesAsleep)) +
           geom_jitter() +
  geom_smooth(color = 'red') +
  labs(title = 'Sedentary minutes vs Total time of sleep ',
       x = 'Total sedentary minutes', y = 'Total minutes of sleep') +
  theme(panel.background = element_blank()), 

activity_and_sleep %>% 
  ggplot(aes(x = TotalSteps , y = TotalMinutesAsleep)) +
  geom_jitter() +
  geom_smooth(color = 'red') +
  labs(title = 'Total steps vs Total time of sleep ',
       x = 'Total steps', y = 'Total minutes of sleep') +
  theme(panel.background = element_blank()), 

activity_and_sleep %>% 
  ggplot(aes(x = TotalSteps , y = Calories)) +
  geom_jitter() +
  geom_smooth(color = 'red') +
  labs(title = 'Total steps vs Calories ',
       x = 'Total steps', y = 'Calories') +
  theme(panel.background = element_blank()),

activity_and_sleep %>% 
  ggplot(aes(x = TotalMinutesAsleep , y = Calories)) +
  geom_jitter() +
  geom_smooth(color = 'red') +
  labs(title = 'Time asleep vs Calories ',
       x = 'Total time of sleep', y = 'Calories') +
  theme(panel.background = element_blank()) 
)
## As we can observe in our visualizations, the total sendetary minutes have negative correlation with the total time alseep
## Which is something  we want to warn the users about 
## To be more active during the day
## On the other hand we see that the time of sleep is not related to the total number of steps and calories for the users
## And finally, as we expected the total steps have positive correlation with the calories

## Categorize users based on their active minutes  

activity_categories <- activity_and_sleep %>% 
  mutate(day = weekdays(ActivityDate)) %>% 
  group_by(day, Id) %>% 
  mutate(total_steps = sum(TotalSteps)) %>% 
  mutate(total_minutes = 
           SedentaryMinutes + FairlyActiveMinutes + VeryActiveMinutes + LightlyActiveMinutes) %>% 
  mutate(sedentary_perc = SedentaryMinutes/total_minutes) %>% 
  mutate(light_perc = LightlyActiveMinutes/total_minutes) %>% 
  mutate(fairly_perc = FairlyActiveMinutes/total_minutes) %>% 
  mutate(very_perc = VeryActiveMinutes/total_minutes) %>% 
  group_by(Id) %>% 
  summarize(sedentary_perc = mean(sedentary_perc),
            light_perc = mean(light_perc) +
            mean(fairly_perc),
            very_perc = mean(very_perc)) %>%
  mutate(sedentary = scales::percent(sedentary_perc), 
         light = scales::percent(light_perc),
        very =  scales::percent(very_perc)) %>% 
  mutate(category = case_when(very_perc >= 0.05 ~ 'Very active',
                   very_perc <= 0.05 & very_perc >= 0.015 ~ 'Fairly active',
                   very_perc < 0.015 ~ 'Sedentary') )  
  

sleep <- sleepday %>% 
  group_by(Id) %>% 
  summarize(total_sleep = mean(TotalMinutesAsleep))
sleep_and_activity <- inner_join(sleep, activity_categories, by = c("Id"))
head(activity_and_sleep)

sleep_and_activity %>% 
  ggplot(aes(x = category, y = total_sleep, fill = category)) +
  geom_col()


head(activity_categories)
head(activity_tracker)
## Join the tables to have data about activity levels and percentage of worn during the day
activity_categories_and_sleep <- full_join(activity_and_sleep,activity_categories, by = c("Id"))
## Order the category variable from very active to sedentary
activity_categories_and_sleep$category <- ordered(activity_categories_and_sleep$category,
                                                  levels = c("Very active", "Fairly active", "Sedentary"))

View(activity_categories_and_sleep)
## Visualize how much active were users that worn their smart devices more or less than half day
ggarrange(
activity_categories_and_sleep %>% 
  filter(activity_by_day == "More than half day") %>% 
  group_by(category) %>% 
  summarize(total = n()) %>% 
  mutate(totals = sum(total)) %>% 
  group_by(category) %>% 
  summarize(total_percent = total / totals) %>% 
    mutate(labels = scales::percent(total_percent)) %>% 
  ggplot(aes(x= "", y = total_percent, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_minimal() +
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=11, face = "bold")) +
  scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff")) +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))+
  labs(title="How active were users who worn their devices more than half day")

  ,
activity_categories_and_sleep %>% 
  filter(activity_by_day == "Less than half day") %>% 
  group_by(category) %>% 
  summarize(total = n()) %>% 
  mutate(totals = sum(total)) %>% 
  group_by(category) %>% 
  summarize(total_percent = total / totals) %>% 
  mutate(labels = scales::percent(total_percent)) %>% 
  ggplot(aes(x= "", y = total_percent, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_minimal() +
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=11, face = "bold")) +
  scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff")) +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))+
  labs(title="How active were users who worn their devices less than half day")
)
# The activity patterns are very similar depenting on their daily usage of their smart devices

