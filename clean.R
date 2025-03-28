install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
install.packages("lubridate")
library(hms)
install.packages("readr")
library(readr)

setwd("C:/Users/taram/OneDrive/Documents/Cert/Case Study/round 2")
bind <- rbind(read_csv("3.12_dailyActivity_merged 3.12.csv"), read_csv("412dailyActivity_merged 4.12.csv"))
bind = rename(bind, Date = ActivityDate)
bind = distinct(bind)
glimpse(bind)
dateClean <- bind  %>%
  mutate(Date = mdy(Date)) 
dailyActivity <- dateClean
dailyActivity$day_of_week <- weekdays(dailyActivity$Date)
notunique <- dateClean %>% 
  add_count(Id, Date) %>% 
  filter(n > 1)
if(nrow(notunique) == 0){
  glimpse(dailyActivity)
} else if (nrow(notunique) > 0) {
  glimpse(dailyActivity)
  print("dailyActivity needs more work")
}

dailyActivityClean <- dailyActivity %>%
  group_by(Id, Date, day_of_week)  %>%
  summarise(
    TotalSteps = sum(TotalSteps),
    TotalDistance  = sum(TotalDistance),  
    TrackerDistance =sum(TrackerDistance),     
    LoggedActivitiesDistance = sum(LoggedActivitiesDistance),
    VeryActiveDistance = sum(VeryActiveDistance),
    ModeratelyActiveDistance = sum(ModeratelyActiveDistance),
    LightActiveDistance   = sum(LightActiveDistance),
    SedentaryActiveDistance = sum(SedentaryActiveDistance), 
    VeryActiveMinutes  = sum(VeryActiveMinutes),
    FairlyActiveMinutes  = sum(FairlyActiveMinutes),
    LightlyActiveMinutes  = sum(LightlyActiveMinutes),
    SedentaryMinutes    = sum(SedentaryMinutes),
    Calories   = sum(Calories) 
       )
glimpse(dailyActivityClean)    



bind <- rbind(read_csv("3.12_weightLogInfo_merged.csv"), read_csv("412weightLogInfo_merged.csv"))
bind = rename(bind, Date_Time = Date)
bind=distinct(bind)
glimpse(bind)
clean <- bind %>%
  mutate(Date_Time = mdy_hms(Date_Time),
         Date = as.Date(Date_Time),
         Time = as_hms(Date_Time)) 
weightLogInfo <- clean
notunique <- clean %>% 
  add_count(Id, Date) %>% 
  filter(n > 1)
if(nrow(notunique) == 0){
  glimpse(weightLogInfo) 
} else if (nrow(notunique) > 0) {
    print("weightLogInfo needs more work")
  }





bind <- rbind(read_csv("3.12_hourlyCalories_merged.csv"), read_csv("412hourlyCalories_merged.csv"))
bind = rename(bind, Date_Time = ActivityHour)
bind = distinct(bind)
glimpse(bind)
clean <- bind %>%
  mutate(Date_Time = mdy_hms(Date_Time),
         Date = as.Date(Date_Time),
         Time = as_hms(Date_Time)) 
hourlyCalories <- clean
notunique <- clean %>% 
  add_count(Id, Date, Time) %>% 
  filter(n > 1)
if(nrow(notunique) == 0){
  glimpse(hourlyCalories) 
} else if (nrow(notunique) > 0) {
  print("hourlyCalories needs more work")
}





bind <- rbind(read_csv("3.12_hourlyIntensities_merged.csv"), read_csv("412hourlyIntensities_merged.csv"))
bind = rename(bind, Date_Time = ActivityHour)
bind = distinct(bind)
glimpse(bind)
clean <- bind %>%
  mutate(Date_Time = mdy_hms(Date_Time),
         Date = as.Date(Date_Time),
         Time = as_hms(Date_Time)) 
hourlyIntensities <- clean
notunique <- clean %>% 
  add_count(Id, Date, Time) %>% 
  filter(n > 1)
if(nrow(notunique) == 0){
  glimpse(hourlyIntensities) 
} else if (nrow(notunique) > 0) {
  print("hourlyIntensities needs more work")
}



bind <- rbind(read_csv("3.12_hourlySteps_merged.csv"), read_csv("412hourlySteps_merged.csv"))
bind = rename(bind, Date_Time = ActivityHour)
bind = distinct(bind)
glimpse(bind)
clean <- bind %>%
  mutate(Date_Time = mdy_hms(Date_Time),
         Date = as.Date(Date_Time),
         Time = as_hms(Date_Time)) 
hourlySteps <- clean
notunique <- clean %>% 
  add_count(Id, Date, Time) %>% 
  filter(n > 1)
if(nrow(notunique) == 0){
  glimpse(hourlySteps) 
} else if (nrow(notunique) > 0) {
  print("hourlySteps needs more work")
}




bind <- rbind(read_csv("3.12_minuteCaloriesNarrow_merged.csv"), read_csv("412minuteCaloriesNarrow_merged.csv"))
bind = rename(bind, Date_Time = ActivityMinute)
bind = distinct(bind)
glimpse(bind)
clean <- bind %>%
  mutate(Date_Time = mdy_hms(Date_Time),
         Date = as.Date(Date_Time),
         Time = as_hms(Date_Time)) 
minuteCalories <- clean
notunique <- clean %>% 
  add_count(Id, Date, Time) %>% 
  filter(n > 1)
if(nrow(notunique) == 0){
  glimpse(minuteCalories) 
} else if (nrow(notunique) > 0) {
  print("minuteCalories needs more work")
}



bind <- rbind(read_csv("3.12_minuteIntensitiesNarrow_merged.csv"),read_csv("412minuteIntensitiesNarrow_merged.csv") )
bind = rename(bind, Date_Time = ActivityMinute)
bind = distinct(bind)
glimpse(bind)
clean <- bind %>%
  mutate(Date_Time = mdy_hms(Date_Time),
         Date = as.Date(Date_Time),
         Time = as_hms(Date_Time)) 
minuteIntensities <- clean
notunique <- clean %>% 
  add_count(Id, Date, Time) %>% 
  filter(n > 1)
if(nrow(notunique) == 0){
  glimpse(minuteIntensities) 
} else if (nrow(notunique) > 0) {
  print("minuteIntensities needs more work")
}



bind <- rbind(read_csv("3.12_minuteStepsNarrow_merged.csv"), read_csv("412minuteStepsNarrow_merged.csv"))
bind = rename(bind, Date_Time = ActivityMinute)
bind = distinct(bind)
glimpse(bind)
clean <- bind %>%
  mutate(Date_Time = mdy_hms(Date_Time),
         Date = as.Date(Date_Time),
         Time = as_hms(Date_Time)) 
minuteSteps <- clean
notunique <- clean %>% 
  add_count(Id, Date, Time) %>% 
  filter(n > 1)
if(nrow(notunique) == 0){
  glimpse(minuteSteps) 
} else if (nrow(notunique) > 0) {
  print("minuteSteps needs more work")
}



bind <- rbind(read_csv("3.12_minuteMETsNarrow_merged.csv"), "412minuteMETsNarrow_merged.csv")
bind = rename(bind, Date_Time = ActivityMinute)
bind = distinct(bind)
glimpse(bind)
clean <- bind %>%
  mutate(Date_Time = mdy_hms(Date_Time),
         Date = as.Date(Date_Time),
         Time = as_hms(Date_Time)) 
minuteMets <- clean
notunique <- clean %>% 
  add_count(Id, Date, Time) %>% 
  filter(n > 1)
if(nrow(notunique) == 0){
  glimpse(minuteMets) 
} else if (nrow(notunique) > 0) {
  print("minuteMets needs more work")
}



bind <- rbind(read_csv("3.12_heartrate_seconds_merged.csv"),read_csv("412heartrate_seconds_merged.csv") )
bind = rename(bind, Date_Time = Time)
bind = distinct(bind)
glimpse(bind)
clean <- bind %>%
  mutate( Date_Time = mdy_hms(Date_Time),
          Date = as.Date(Date_Time),
          Time = as_hms(Date_Time), 
          TimeInMinutes = as_hms(floor_date(Date_Time, "minute")) )
secondsHeartRate <- clean
notunique <- clean %>% 
  add_count(Id, Date, Time) %>% 
  filter(n > 1)
if(nrow(notunique) == 0){
  glimpse(secondsHeartRate) 
} else if (nrow(notunique) > 0) {
  print("secondsHeartRate needs more work")
}

minuteHeartRate <- secondsHeartRate %>%
group_by(Id, Date, TimeInMinutes)  %>%
  summarise(
    avgMinuteHeartRate = round(mean(Value), digits=2)
      )

---- Normalizing
intensityOneCalories = 









-- dailyActivitywWeight <- merge(dailyActivityClean, weightLogInfo,
--                                    by.x=c("Id", "Date"),
 --                                  by.y=c("Id", "Date"), 
 --                                  all.x = TRUE, all.y = TRUE)
filter <- dailyActivitywWeight[!is.na(dailyActivitywWeight$WeightPounds),] 
q = ggplot(filter, aes(x=Date, y=WeightPounds , color = Id)) + geom_point(aes(color = Date, size=WeightPounds)) + facet_wrap(~Id)
dev.off()
plot(rnorm(50), rnorm(50))
hourlyStats <- merge(merge(hourlyCalories, hourlyIntensities, all = TRUE), hourlySteps, all= TRUE) 
hourlyStats2 <- merge(merge(hourlyCalories, hourlyIntensities, by.x=c("Id", "Date", "Time"), by.y=c("Id", "Date", "Time"), all.x = TRUE, all.y = FALSE), hourlySteps, by.x=c("Id", "Date", "Time"), by.y=c("Id", "Date", "Time"), all.x = TRUE, all.y = FALSE) 
hourlyStats3 <- merge(merge(hourlyCalories, hourlyIntensities, by.x=c("Id", "Date", "Time"), by.y=c("Id", "Date", "Time"), all= TRUE), hourlySteps, by.x=c("Id", "Date", "Time"), by.y=c("Id", "Date", "Time"), all= TRUE) 
hourlyStats$day_of_week <- weekdays(hourlyStats$Date)

minuteStats <- merge(merge(minuteCalories, minuteIntensities, all = TRUE), minuteSteps, all = TRUE)
minuteStats$day_of_week <- weekdays(minuteStats$Date)
t<-minuteStats
minuteMetswStats <- merge(minuteStats, minuteMets, all.x = FALSE, all.y = TRUE)
filter <- filter(minuteMetswStats, Intensity > 1)
?filter

filter <- dailyActivitywWeight[!is.na(dailyActivitywWeight$WeightPounds),] 

minuteHeartRatewMETsStats <- merge(minuteMetswStats, minuteHeartRate, all.x = FALSE, all.y = TRUE)


filter <- hourlyStats %>%
  filter(if_any(c(Id:day_of_week), is.na))

filter <- minuteStats %>%
  filter(if_any(c(Id:METs), is.na))
-- miscounted so cleaned data more
minuteStats <- minuteStats %>%
  filter(!is.na(Date)) 