#### Read Me####

#This script cleans Gross reservoir outflow data and sums daily discharge into a monthly total discharge. 

####Libraries #### 
library(readr)
library(tidyverse)
library(lubridate)

####Read in data ####
GrossReleases <- read.table(file = "data/raw/Gross Outflows.csv",
         sep = ",", header=TRUE)

GrosssReleases <- read.csv(file = "data/raw/Gross Outflows.csv", header=TRUE)

####Clean data####
CleanGross <- GrossReleases %>% 
  select("X.1","X.5") %>%
  rename(Discharge = X.1, 
         date_measured = X.5) %>% #rename columns so they make sense 
  slice(c(8:10338)) #clean up .csv import so only data is left. 

CleanGross$Discharge = (as.numeric(CleanGross$Discharge)) #Formatting data

CleanGross$date_measured = parse_date_time(CleanGross$date_measured, c("%Y-%m-%d %H:%M:%S"), exact = T, tz="UTC")  #formatting date

#### Sum daily data to monthly ####
CleanGross$yr = lubridate::year(CleanGross$date_measured)# extract just the year
CleanGross$mo = lubridate::month(CleanGross$date_measured) # extract just the month

GrossMonthly <- CleanGross %>% #Sum daily data into monthly
  group_by(yr,mo) %>%
  summarize(MonthlyDischarge = sum(Discharge))

GrossMonthly$Date = make_date(year = GrossMonthly$yr, month = GrossMonthly$mo, day = 1) #Re-format date column 

GrossMonthlyRelease <- GrossMonthly %>% #Select and rename relevant columns
  ungroup() %>%
  select(MonthlyDischarge,Date) %>%
  rename(Release = MonthlyDischarge)

####Write to CSV#### 
write_csv(GrossMonthlyRelease,file = "data/processed/GrossLakesMonthlyReleases") #write to csv
