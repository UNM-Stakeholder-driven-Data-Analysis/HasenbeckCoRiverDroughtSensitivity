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

#Blue River below Dillon
siteNumber <- "05114000" #Gauge or sitenumber from USGS 
DillonInfo <- readNWISsite(siteNumber)

#Parameter code - this tells R to call for discharge parameter.
#See list of paramter codes here: https://nwis.waterdata.usgs.gov/dc/nwis/pmcode
# p code is also in the gauge url:
#https://waterdata.usgs.gov/monitoring-location/09050700/#parameterCode=00060&period=P7D
parameterCd <- "00060" 

# Raw daily data:
DillonDischarge <- readNWISdv(
  siteNumber, parameterCd,
  "1989-03-11", "2023-03-31")


####Clean data####
CleanDillon <- DillonDischarge %>% 
  select(X_00060_00003, Date) %>%
  rename(Discharge = X_00060_00003, 
         date_measured = Date) #rename columns so they make sense 

#USGS import automatially formats data correctly, so you don't have to worry about formatting it. 

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

