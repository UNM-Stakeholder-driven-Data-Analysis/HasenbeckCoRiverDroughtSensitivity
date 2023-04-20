#### Read Me####

#This script cleans Twin Lakes reservoir outflow data and sums daily discharge into a monthly total discharge. 

####Libraries #### 
library(readr)
library(tidyverse)
library(lubridate)

####Read in data ####
TwinReleases <- read.table(file = "data/raw/Reservoirs/Twin Lakes Reservoir and Dam Daily Lake_Reservoir Release - Total-cfs Time Series Data (1980-12-16 - 2023-03-14).csv",
         sep = ",", header=TRUE)

####Clean data####
CleanTwin <- TwinReleases %>% 
  select("X.1","X.5") %>%
  rename(Discharge = X.1, 
         date_measured = X.5) %>% #rename columns so they make sense 
  slice(c(8:12847)) #clean up .csv import so only data is left. 

CleanTwin$Discharge = (as.numeric(CleanTwin$Discharge)) #Formatting data

CleanTwin$date_measured = parse_date_time(CleanTwin$date_measured, c("%Y-%m-%d %H:%M:%S"), exact = T, tz="UTC")  #formatting date

#### Sum daily data to monthly ####
CleanTwin$yr = lubridate::year(CleanTwin$date_measured)# extract just the year
CleanTwin$mo = lubridate::month(CleanTwin$date_measured) # extract just the month

TwinMonthly <- CleanTwin %>% #Sum daily data into monthly
  group_by(yr,mo) %>%
  #Omit NAs, otherwise when you sum data to monthly, it will input an NA for all months that contain ANY NAs 
  na.omit(Discharge) %>% 
  summarize(MonthlyDischarge = sum(Discharge))

TwinMonthly$Date = make_date(year = TwinMonthly$yr, month = TwinMonthly$mo, day = 1) #Re-format date column 

TwinMonthlyRelease <- TwinMonthly %>% #Select and rename relevant columns
  ungroup() %>%
  select(MonthlyDischarge,Date) %>%
  rename(Release = MonthlyDischarge)

####Write to CSV#### 
write_csv(TwinMonthlyRelease,file = "data/processed/TwinLakesMonthlyReleases") #write to csv
