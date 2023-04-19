
####Read me####

#This script cleans Adams Tunnel diversion data to prep it for simple linear regresssion 

####libraries ####

library(tidyverse)
library(lubridate)


####Reading in csv data####

Adams <- read_csv(file = "data/raw/ADAMS TUNNEL_0404634_Diversions_Releases.csv",) #From CO DWR

####Cleaning Data ####

AdamsClean <- Adams %>% 
  select(Date, Amount) %>%
  dplyr :: rename(Discharge = Amount) #renaming columns to simplify replication 

AdamsClean$Date = parse_date_time(AdamsClean$Date, c("%m/%d/%Y %H:%M"), exact = T, tz="UTC")  #formatting date
as.numeric(AdamsClean$Discharge) #defining data 

#Reformat date to remove time 
AdamsClean$yr = lubridate::year(AdamsClean$Date)# extract just the year
AdamsClean$mo = lubridate::month(AdamsClean$Date) # extract just the month
AdamsClean$Dates = make_date(year = AdamsClean$yr, month = AdamsClean$mo, day = 1) #Re-format date column 

AdamsMonthly <- AdamsClean %>%#Select and rename relevant columns
  ungroup() %>%
  select(Discharge,Dates) %>% 
  dplyr :: rename(Date = Dates)

#There are duplicates in the data
sum(duplicated(AdamsMonthly$Date))

#sum duplicates 
AdamsMonthly = aggregate(Discharge~Date,data=AdamsMonthly,FUN=sum)

sum(duplicated(AdamsMonthly$Date))

               
write_csv(AdamsMonthly,file = "data/processed/AdamsMonthlyDischarge") #write to csv

