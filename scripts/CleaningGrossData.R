#### Read Me####

#This script cleans Gross reservoir outflow data and sums daily discharge into a monthly total discharge. 

####Libraries #### 
library(readr)
library(tidyverse)
library(lubridate)

####Read in data ####
GrossReleases <- read.table(file = "data/processed/Gross_Selected_Station_Observations_Daily_Xtab_202304191750.csv",
         sep = ",", header=TRUE)

####Clean data####
CleanGross <- GrossReleases %>% 
  select("Date.Time","DISCHRG.Value") %>%
  rename(Discharge = DISCHRG.Value, 
         date_measured = Date.Time) #rename columns so they make sense 

CleanGross$Discharge = (as.numeric(CleanGross$Discharge)) #Formatting data
#Date format: #09/12/1986 00:00
CleanGross$date_measured = parse_date_time(CleanGross$date_measured, c("%m/%d/%Y %H:%M"), exact = T, tz="UTC")  #formatting date

#### Sum daily data to monthly ####
CleanGross$yr = lubridate::year(CleanGross$date_measured)# extract just the year
CleanGross$mo = lubridate::month(CleanGross$date_measured) # extract just the month

GrossMonthly <- CleanGross %>% #Sum daily data into monthly
  group_by(yr,mo) %>%
  #Omit NAs, otherwise when you sum data to monthly, it will input an NA for all months that contain ANY NAs 
  na.omit(Discharge) %>% 
  #Sum to monthly
  summarize(MonthlyDischarge = sum(Discharge))


GrossMonthly$Date = make_date(year = GrossMonthly$yr, month = GrossMonthly$mo, day = 1) #Re-format date column 

GrossMonthlyRelease <- GrossMonthly %>% #Select and rename relevant columns
  ungroup() %>%
  select(MonthlyDischarge,Date) %>%
  rename(Release = MonthlyDischarge)

####Write to CSV#### 
write_csv(GrossMonthlyRelease,file = "data/processed/GrossLakesMonthlyReleases") #write to csv

