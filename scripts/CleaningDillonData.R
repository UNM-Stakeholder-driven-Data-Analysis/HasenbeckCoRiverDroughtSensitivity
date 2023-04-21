#### Read Me####

#This script cleans Dillon reservoir outflow data and sums daily discharge into a monthly total discharge. 

####Libraries #### 
library(readr)
library(tidyverse)
library(lubridate)
#USGS Data Retrival Package, instructions here:https://waterdata.usgs.gov/blog/dataretrieval/
library(dataRetrieval)

####Read in data from USGS ####

#See list of paramter codes here: https://nwis.waterdata.usgs.gov/dc/nwis/pmcode
# p code is also in the gauge url:
  #https://waterdata.usgs.gov/monitoring-location/09050700/#parameterCode=00060&period=P7D


# Real-time discharge at a site
DillonDischarge <- readNWISdata(sites = "05114000", #site or gauge number 
                                service = "iv", 
                                parameterCd = "00060", 
                                startDate = ("1989-04-01"),
                                endDate = ("2023-03-31"))


dataTemp <- readNWISdata("05114000",
                         parameterCd = "00060",
                         service="iv") 




####Clean data####
CleanDillon <- DillonDischarge %>% 
  select(X_00060_00000, dateTime) %>%
  rename(Discharge = X_00060_00000, 
         date_measured = dateTime) #rename columns so they make sense 

CleanDillon$Discharge = (as.numeric(CleanDillon$Discharge)) #Formatting data
?posi
#Date format: 1994-10-01 06:00:00
CleanDillon$date_measured = parse_date_time(CleanDillon$date_measured, c("%Y-%m-%d %H:%M:$S"), exact = T, tz="UTC")
#### Sum daily data to monthly ####
CleanDillon$yr = lubridate::year(CleanDillon$date_measured)# extract just the year
CleanDillon$mo = lubridate::month(CleanDillon$date_measured) # extract just the month

DillonMonthly <- CleanDillon %>% #Sum daily data into monthly
  group_by(yr,mo) %>%
  #Omit NAs, otherwise when you sum data to monthly, it will input an NA for all months that contain ANY NAs 
  na.omit(Discharge) %>% 
  #Sum to monthly
  summarize(MonthlyDischarge = sum(Discharge))


DillonMonthly$Date = make_date(year = DillonMonthly$yr, month = DillonMonthly$mo, day = 1) #Re-format date column 

DillonMonthlyRelease <- DillonMonthly %>% #Select and rename relevant columns
  ungroup() %>%
  select(MonthlyDischarge,Date) %>%
  rename(Release = MonthlyDischarge)

####Write to CSV#### 
write_csv(DillonMonthlyRelease,file = "data/processed/DillonLakesMonthlyReleases") #write to csv

