# Because this is my first capstone project, i had doubt on where should i start so i referenced my steps to someone's findings in linkedin, but i do it in my own way.
# reference: https://www.linkedin.com/pulse/google-data-analytics-capstone-project-ross-nelson/
# Cleaning data steps in excel: 
# Convert .csv to .xlsx
# Adding ride_length column with formula (=Ended_Time-Started_Time)
# Adding day_of_week column with formula (=Text(Started_Time;"dddd")
# Adding month_ride column with formula (=Text(Started_Time;"mmyy")
# Data ready to be used in R
# Reasons i chose R is because the data is too big to be imported to SQL, plus i want to train my R programming.

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

#Ride Data Visualization Count per Month and Type
ggplot(data = aug21, mapping = aes(x = day_of_week,y=mean(ride_length),fill=day_of_week))+geom_bar(stat="identity")+facet_wrap(~member_casual)+
theme(axis.text.x = element_text(angle = 45))+labs(title="August 2021",x="Day",y="Minutes")+
geom_text(aes(label = mean(ride_length)), position = position_dodge(0.9))
ggplot(data = aug22, mapping = aes(x = day_of_week,fill=day_of_week))+geom_bar()+facet_wrap(~member_casual)+
theme(axis.text.x = element_text(angle = 45))+labs(title="August 2022")

#Combined Ride Data Visualization Count per Month and Type
data_combined %>%  
ggplot(mapping = aes(x = day_of_week,fill=day_of_week))+geom_bar()+facet_wrap(~ month_ride + member_casual)+
theme(axis.text.x = element_text(angle = 45))+labs(title="Data from 08-2021 to 08-2022")
#Casual rider tend to ride more on the weekends
#Member rider have consistent ride all weekdays


str(aug21)
str(data_combined)