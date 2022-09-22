# Steps referenced to 
# https://www.linkedin.com/pulse/google-data-analytics-capstone-project-ross-nelson/

#Library used
library(tidyverse)
library(ggplot2)
library(lubridate)
library(hms)
library(readxl)

#Import Data
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

aug21xl %>%
ggplot(data = aug21xl, mapping = aes(x = day_of_week, y = sum(ride_length)))+geom_line()

str(aug21)