
####Read me####

#This script cleans Roberts Tunnel diversion data to prep it for simple linear regresssion 

####libraries ####

library(tidyverse)
library(lubridate)


####Reading in csv data####

Roberts <- read_csv(file = "data/raw/RobertsTunnelDischarge-Daily-1963-2023-Selected_Station_Analysis_Xtab_202304192044.csv",) #From CO DWR

####Cleaning Data ####

RobertsClean <- Roberts %>% 
  select("Streamflow Value", "meas_date") %>%
  dplyr :: rename(Discharge = 'Streamflow Value', date_measured = meas_date) #renaming columns to simplify replication 

#Date format: 10/02/1963
RobertsClean$date_measured = as.Date(RobertsClean$date_measured, c("%m/%d/%Y"), exact = T)  #formatting date
as.numeric(RobertsClean$Discharge) #defining data 

###Converting daily to monthly measurement
RobertsClean$yr = lubridate::year(RobertsClean$date_measured)# extract just the year
RobertsClean$mo = lubridate::month(RobertsClean$date_measured) # extract just the month

RobertsMonthly <- RobertsClean %>% #Sum daily data into monthly
  group_by(yr,mo) %>%
  #Omit NAs, otherwise when you sum data to monthly, it will input an NA for all months that contain ANY NAs 
  na.omit(Discharge) %>% 
  summarize(MonthlyDischarge = sum(Discharge))

RobertsMonthly$Date = make_date(year = RobertsMonthly$yr, month = RobertsMonthly$mo, day = 1) #Re-format date column 

RobertsDischarge <- RobertsMonthly %>% #Select and rename relevant columns
  ungroup() %>%
  select(MonthlyDischarge,Date) %>%
  rename(Discharge = MonthlyDischarge)

#There are no duplicates in the data
sum(duplicated(RobertsMonthly$Date))
#.... but if there were, I woudl run THIS! 
#sum duplicates 
#RobertsMonthly = aggregate(Discharge~Date,data=RobertsMonthly,FUN=sum)

sum(is.na(RobertsDischarge$Discharge))

               
write_csv(RobertsDischarge,file = "data/processed/RobertsMonthlyDischarge") #write to csv

