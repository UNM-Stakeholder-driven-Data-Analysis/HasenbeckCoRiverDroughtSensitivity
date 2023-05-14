#### Read Me####

#This script cleans Horsetooth reservoir outflow data and sums daily discharge into a monthly total discharge. 

####Libraries #### 
library(readr)
library(tidyverse)
library(lubridate)

####Read in data ####
HorsetoothReleases <- read.table(file = "data/raw/Reservoirs/Horsetooth Reservoir and Dam Daily Lake_Reservoir Release - Total-cfs Time Series Data (1991-01-28 - 2023-03-15).csv",
         sep = ",", header=TRUE)

####Clean data####
CleanHorsetooth <- HorsetoothReleases %>% 
  select("X.1","X.5") %>%
  rename(Discharge = X.1, 
         date_measured = X.5) %>% #rename columns so they make sense 
  slice(c(8:10338)) #clean up .csv import so only data is left. 

CleanHorsetooth$Discharge = (as.numeric(CleanHorsetooth$Discharge)) #Formatting data

CleanHorsetooth$date_measured = parse_date_time(CleanHorsetooth$date_measured, c("%Y-%m-%d %H:%M:%S"), exact = T, tz="UTC")  #formatting date

#### Sum daily data to monthly ####
CleanHorsetooth$yr = lubridate::year(CleanHorsetooth$date_measured)# extract just the year
CleanHorsetooth$mo = lubridate::month(CleanHorsetooth$date_measured) # extract just the month

HorsetoothMonthly <- CleanHorsetooth %>% #Sum daily data into monthly
  group_by(yr,mo) %>%
  #Omit NAs, otherwise when you sum data to monthly, it will input an NA for all months that contain ANY NAs 
  na.omit(Discharge) %>% 
  summarize(MonthlyDischarge = sum(Discharge))


HorsetoothMonthly$Date = make_date(year = HorsetoothMonthly$yr, month = HorsetoothMonthly$mo, day = 1) #Re-format date column 

HorsetoothMonthlyRelease <- HorsetoothMonthly %>% #Select and rename relevant columns
  ungroup() %>%
  select(MonthlyDischarge,Date) %>%
  rename(Release = MonthlyDischarge)

####Write to CSV#### 
write_csv(HorsetoothMonthlyRelease,file = "data/processed/HorsetoothLakesMonthlyReleases") #write to csv
