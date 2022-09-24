# Because this is my first capstone project, i had doubt on where should i start so some of my steps are referenced by someone's.
# reference: https://www.linkedin.com/pulse/google-data-analytics-capstone-project-ross-nelson/
# Cleaning data steps in excel: 
# Convert .csv to .xlsx
# Adding ride_length column with formula (=Ended_Time-Started_Time)
# Adding day_of_week column with formula (=Text(Started_Time;"dddd")
# Adding month_ride column with formula (=Text(Started_Time;"mmyy")
# Data ready to be used in R
# Reasons i chose R is because the data is too big to be imported to SQL, and  i want to train my R programming skills.

#Library used
library(tidyverse)
library(ggplot2)
library(lubridate)
library(hms)
library(readxl)

#Import Data or read xls file
aug21=read_excel("data202108.xlsx")
sep21=read_excel("data202109.xlsx")
okt21=read_excel("data202110.xlsx")
nov21=read_excel("data202111.xlsx")
dec21=read_excel("data202112.xlsx")
jan22=read_excel("data202201.xlsx")
feb22=read_excel("data202202.xlsx")
mar22=read_excel("data202203.xlsx")
apr22=read_excel("data202204.xlsx")
may22=read_excel("data202205.xlsx")
jun22=read_excel("data202206.xlsx")
jul22=read_excel("data202207.xlsx")
aug22=read_excel("data202208.xlsx")

#omit blank data
aug21 <- na.omit(aug21) 
sep21 <- na.omit(sep21) 
okt21 <- na.omit(okt21) 
nov21 <- na.omit(nov21) 
dec21 <- na.omit(dec21) 
jan22 <- na.omit(jan22)
feb22 <- na.omit(feb22)
mar22 <- na.omit(mar22)
apr22 <- na.omit(apr22)
may22 <- na.omit(may22)
jun22 <- na.omit(jun22)
jul22 <- na.omit(jul22)
aug22 <- na.omit(aug22) 

#convert ride_length to minutes
aug21$ride_length <- minute(aug21$ride_length) + 60*hour(aug21$ride_length)
sep21$ride_length <- minute(sep21$ride_length) + 60*hour(sep21$ride_length)
okt21$ride_length <- minute(okt21$ride_length) + 60*hour(okt21$ride_length)
nov21$ride_length <- minute(nov21$ride_length) + 60*hour(nov21$ride_length)
dec21$ride_length <- minute(dec21$ride_length) + 60*hour(dec21$ride_length)
jan22$ride_length <- minute(jan22$ride_length) + 60*hour(jan22$ride_length)
feb22$ride_length <- minute(feb22$ride_length) + 60*hour(feb22$ride_length)
mar22$ride_length <- minute(mar22$ride_length) + 60*hour(mar22$ride_length)
apr22$ride_length <- minute(apr22$ride_length) + 60*hour(apr22$ride_length)
may22$ride_length <- minute(may22$ride_length) + 60*hour(may22$ride_length)
jun22$ride_length <- minute(jun22$ride_length) + 60*hour(jun22$ride_length)
jul22$ride_length <- minute(jul22$ride_length) + 60*hour(jul22$ride_length)
aug22$ride_length <- minute(aug22$ride_length) + 60*hour(aug22$ride_length)

#order weekdays from monday to sunday
aug21$day_of_week <- ordered(aug21$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
"Friday", "Saturday", "Sunday"))
sep21$day_of_week <- ordered(sep21$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
"Friday", "Saturday", "Sunday"))
okt21$day_of_week <- ordered(okt21$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
"Friday", "Saturday", "Sunday"))
nov21$day_of_week <- ordered(nov21$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
"Friday", "Saturday", "Sunday"))
dec21$day_of_week <- ordered(dec21$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
"Friday", "Saturday", "Sunday"))
jan22$day_of_week <- ordered(jan22$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
"Friday", "Saturday", "Sunday"))
feb22$day_of_week <- ordered(feb22$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
"Friday", "Saturday", "Sunday"))
mar22$day_of_week <- ordered(mar22$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
"Friday", "Saturday", "Sunday"))
apr22$day_of_week <- ordered(apr22$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
"Friday", "Saturday", "Sunday"))
may22$day_of_week <- ordered(may22$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
"Friday", "Saturday", "Sunday"))
jun22$day_of_week <- ordered(jun22$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
"Friday", "Saturday", "Sunday"))
jul22$day_of_week <- ordered(jul22$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
"Friday", "Saturday", "Sunday"))
aug22$day_of_week <- ordered(aug22$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
"Friday", "Saturday", "Sunday"))

# Merge data with rbind
data_combined <- rbind(aug21,sep21,okt21,nov21,dec21,jan22,feb22,mar22,apr22,may22,jun22,jul22,aug22)

# Order month_ride
data_combined$month_ride <- ordered(data_combined$month_ride, levels=c("0821", "0921", "1021", "1121", 
"1221", "0122", "0222", "0322", "0422", "0522", "0622", "0722", "0822"))


#Total Ride Count by Customer type per Weekdays
aug21 %>%   
  group_by(member_casual, day_of_week) %>% 
  summarise(total_rides=n()) %>%
  ggplot(aes(x=day_of_week, y=total_rides, fill = member_casual))+
  geom_col(position = position_dodge (width=0.9) , width = 0.9) + labs (x="Day of Week", y="Total Ride Count", 
                                      title = "Total Ride Count by Customer type per Weekdays",
						  subtitle = "August 2021", 
                                      fill = "Membership Type") +
  geom_text(aes(label = total_rides, angle = 90),
    position = position_dodge(1),
    vjust = 0.4,
    hjust = 1.1,
    size = 3
  ) +
  theme(axis.text.x = element_text (angle = 45))
dec21 %>%   
  group_by(member_casual, day_of_week) %>% 
  summarise(total_rides=n()) %>%
  ggplot(aes(x=day_of_week, y=total_rides, fill = member_casual))+
  geom_col(position = position_dodge (width=0.9) , width = 0.9) + labs (x="Day of Week", y="Total Ride Count", 
                                      title = "Total Ride Count by Customer type per Weekdays",
						  subtitle = "December 2021", 
                                      fill = "Membership Type") +
  geom_text(aes(label = total_rides, angle = 90),
    position = position_dodge(1),
    vjust = 0.4,
    hjust = 1.1,
    size = 3
  ) +
  theme(axis.text.x = element_text (angle = 45))
mar22 %>%   
  group_by(member_casual, day_of_week) %>% 
  summarise(total_rides=n()) %>%
  ggplot(aes(x=day_of_week, y=total_rides, fill = member_casual))+
  geom_col(position = position_dodge (width=0.9) , width = 0.9) + labs (x="Day of Week", y="Total Ride Count", 
                                      title = "Total Ride Count by Customer type per Weekdays",
						  subtitle = "March 2022", 
                                      fill = "Membership Type") +
  geom_text(aes(label = total_rides, angle = 90),
    position = position_dodge(1),
    vjust = 0.4,
    hjust = 1.1,
    size = 3
  ) +
  theme(axis.text.x = element_text (angle = 45))

#Average Minutes per ride by Customer type on Weekdays

aug21 %>%   
  group_by(member_casual, day_of_week) %>% 
  summarise(average_minutes=round(mean(ride_length), digits=2)) %>%
  ggplot(aes(x=day_of_week, y=average_minutes, fill = member_casual))+
  geom_col(position = position_dodge (width=0.9) , width = 0.9) + labs (x="Day of Week", y="Average Minutes", 
                                      title = "Average length per ride by Customer type on Weekdays",
						  subtitle = "August 2021", 
                                      fill = "Membership Type") +
  geom_text(aes(label = average_minutes, angle = 90),
    position = position_dodge(1),
    vjust = 0.4,
    hjust = 1.1,
    size = 3
  )+
  theme(axis.text.x = element_text (angle = 45))

dec21 %>%   
  group_by(member_casual, day_of_week) %>% 
  summarise(average_minutes=round(mean(ride_length), digits=2)) %>%
  ggplot(aes(x=day_of_week, y=average_minutes, fill = member_casual))+
  geom_col(position = position_dodge (width=0.9) , width = 0.9) + labs (x="Day of Week", y="Average Minutes", 
                                      title = "Average length per ride by Customer type on Weekdays",
						  subtitle = "December 2021", 
                                      fill = "Membership Type") +
  geom_text(aes(label = average_minutes, angle = 90),
    position = position_dodge(1),
    vjust = 0.4,
    hjust = 1.1,
    size = 3
  )+
  theme(axis.text.x = element_text (angle = 45))

mar22 %>%   
  group_by(member_casual, day_of_week) %>% 
  summarise(average_minutes=round(mean(ride_length), digits=2)) %>%
  ggplot(aes(x=day_of_week, y=average_minutes, fill = member_casual))+
  geom_col(position = position_dodge (width=0.9) , width = 0.9) + labs (x="Day of Week", y="Average Minutes", 
                                      title = "Average length per ride by Customer type on Weekdays",
						  subtitle = "March 2021", 
                                      fill = "Membership Type") +
  geom_text(aes(label = average_minutes, angle = 90),
    position = position_dodge(1),
    vjust = 0.4,
    hjust = 1.1,
    size = 3
  )+
  theme(axis.text.x = element_text (angle = 45))

## Conclusion 1 based on total ride per month and average minutes ride per days:
## Even though casual member rides are down sharply on the winter months, the average minutes of casual riders are still higher than the member's average minutes ride.
## Casual riders number tend to rise on the weekends, whereas member riders have steady numbers throughout the week. this is also predictable


#Combined Average ride length by Customer type and Day of Week
data_combined %>%   
  group_by(member_casual, day_of_week) %>% 
  summarise(average_minutes = round(mean(ride_length),digits=2)) %>% 
  ggplot(aes(x=day_of_week, y = average_minutes, fill = member_casual))+
  geom_col(position = "dodge") + labs (x="Day of Week", y="Average Minutes", 
                                      title = "Average Ride Length by Customer Type and Day of Week", 
						  subtitle = "August 2021 - August 2022", 
                                      fill = "Type of Membership") +
  geom_text(aes(label = average_minutes, angle = 0),
    position = position_dodge(1),
    vjust = -0.6,
    hjust = 0.5,
    size = 3
  )+
  theme(axis.text.x = element_text (angle = 45))

#Total ride by Customer Type per days in the past 1 year
data_combined %>%   
  group_by(member_casual, day_of_week) %>% 
  summarise(total_rides=n()) %>%
  ggplot(aes(x=day_of_week, y=total_rides, fill = member_casual))+
  geom_col(position = position_dodge (width=0.9) , width = 0.9) + labs (x="Day of Week", y="Count", 
                                      title = "Total ride by Customer Type per days in the past 1 year",
						  subtitle = "August 2021 - August 2022", 
                                      fill = "Membership Type") +
  geom_text(aes(label = total_rides, angle = 90),
    position = position_dodge(1),
    vjust = 0.4,
    hjust = 1.1,
    size = 3
  )+
  theme(axis.text.x = element_text (angle = 45))

#Total ride by Customer Type per Month in the past 1 year
data_combined %>%   
  group_by(member_casual, month_ride) %>% 
  summarise(total_rides=n()) %>%
  ggplot(aes(x=month_ride, y=total_rides, fill = member_casual))+
  geom_col(position = position_dodge (width=0.9) , width = 0.9) + labs (x="Month", y="Count", 
                                      title = "Total ride by Customer Type per Month in the past 1 year",
						  subtitle = "August 2021 - August 2022", 
                                      fill = "Membership Type") +
  geom_text(aes(label = total_rides, angle = 90),
    position = position_dodge(1),
    vjust = 0.4,
    hjust = 1.1,
    size = 3
  )+
  theme(axis.text.x = element_text (angle = 45))

## Conclusion 2:
## Casual riders have double the average ride minutes of member riders
## On colder months such as november - april, numbers of riders do go down.
## overall we had more member riders ride throughout the weekdays, but more casual riders on the weekends.

#Bicycle Type preferred by customer type in the past 1 year
data_combined %>%   
  group_by(member_casual, rideable_type) %>% 
  summarise(total_rides=n()) %>%
  ggplot(aes(x=rideable_type, y=total_rides, fill = member_casual))+
  geom_col(position = position_dodge (width=0.9) , width = 0.9) + labs (x="Bicycle Type", y="Count", 
                                      title = "Bicycle Type preferred by customer type in the past 1 year",
						  subtitle = "August 2021 - August 2022", 
                                      fill = "Membership Type") +
  geom_text(aes(label = total_rides),
    position = position_dodge(1),
    vjust = -1,
    hjust = 0.5,
    size = 3
  )
## Conclusion 3:
## Classic bike is preferred by member riders, whereas electric bikes are distributed evenly between member and casual riders.
## docked bike are only used by casual riders.

glimpse(aug21)
str(data_combined)