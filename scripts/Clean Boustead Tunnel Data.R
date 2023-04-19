
####Read me####

#This script cleans Boustead discharge data to prep it for simple linear regresssion 

####libraries ####

library(tidyverse)
library(lubridate)


####Reading in csv data####

BousteadDiversions <- read.table(file = "data/raw/FRY ARK PR BOUSTEAD TUNNEL_3804625_Diversions.csv",sep=",", header=TRUE) #From USGS, older diversion record

####Cleaning Data ####
## Cleaning 1970 data 
BousteadClean <- BousteadDiversions %>% 
  select(Date,Amount) %>% #Selecting relevant columns
  rename(Discharge = Amount, 
         date_measured = Date)  #renaming columns so they make sense


BousteadClean$Discharge = as.double(BousteadClean$Discharge) #defining data 
BousteadClean$date_measured = parse_date_time(BousteadClean$date_measured, c("%m/%d/%Y %H:%M"), exact = T, tz="UTC")  #formatting date

BousteadClean$yr = lubridate::year(BousteadClean$date_measured)# extract just the year
BousteadClean$mo = lubridate::month(BousteadClean$date_measured) # extract just the month

BousteadClean$Date = make_date(year = BousteadClean$yr, month = BousteadClean$mo, day = 1) #Re-format date column

BousteadMonthly <- BousteadClean %>% select(Date,Discharge)


write_csv(BousteadMonthly,file = "data/processed/BousteadMonthlyDiversions") #write to csv

