
####Read me####

#This script cleans Roberts Tunnel diversion data to prep it for simple linear regresssion 

####libraries ####

library(tidyverse)
library(lubridate)


####Reading in csv data####

Roberts <- read_csv(file = "data/raw/ROBERTS TUNNEL_Diversions_Monthly_202304191741.csv",) #From CO DWR

####Cleaning Data ####

RobertsClean <- Roberts %>% 
  select(Date, Amount) %>%
  dplyr :: rename(Discharge = Amount) #renaming columns to simplify replication 

RobertsClean$Date = parse_date_time(RobertsClean$Date, c("%m/%d/%Y %H:%M"), exact = T, tz="UTC")  #formatting date
as.numeric(RobertsClean$Discharge) #defining data 

#Reformat date to remove time 
RobertsClean$yr = lubridate::year(RobertsClean$Date)# extract just the year
RobertsClean$mo = lubridate::month(RobertsClean$Date) # extract just the month
RobertsClean$Dates = make_date(year = RobertsClean$yr, month = RobertsClean$mo, day = 1) #Re-format date column 

RobertsMonthly <- RobertsClean %>%#Select and rename relevant columns
  ungroup() %>%
  select(Discharge,Dates) %>% 
  dplyr :: rename(Date = Dates)

#There are no duplicates in the data
sum(duplicated(RobertsMonthly$Date))
#.... but if there were, I woudl run THIS! 
#sum duplicates 
#RobertsMonthly = aggregate(Discharge~Date,data=RobertsMonthly,FUN=sum)

sum(duplicated(RobertsMonthly$Date))

               
write_csv(RobertsMonthly,file = "data/processed/RobertsMonthlyDischarge") #write to csv

