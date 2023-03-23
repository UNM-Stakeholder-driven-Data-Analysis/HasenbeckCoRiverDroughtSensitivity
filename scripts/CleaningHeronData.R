#### Read Me####

#This script cleans Heron Reservoir Outflow data and sums daily discharge into a monthly total discharge. 

####Libraries #### 
library(readr)
library(tidyverse)
library(lubridate)

####Read in data ####
HeronReleases <- read.table(file = "data/raw/Reservoirs/Heron Reservoir and Dam Daily Lake_Reservoir Release - Total-cfs Time Series Data (2008-01-01 - 2023-03-15).csv",
         sep = ";", header=TRUE)

####Clean data####
CleanHeron <- HeronReleases %>% 
  select(Result,Datetime..UTC.) %>% #Selecting relevant columns
  rename(Release = Result, 
         date_measured = Datetime..UTC.) #renaming columns so they make sense

CleanHeron$Release = as.double(CleanHeron$Release) #Formatting data

CleanHeron$date_measured = parse_date_time(CleanHeron$date_measured, c("%m/%d/%y %H:%M"), exact = T, tz="MST")  #formatting date

#### Sum daily data to monthly ####
CleanHeron$yr = lubridate::year(CleanHeron$date_measured)# extract just the year
CleanHeron$mo = lubridate::month(CleanHeron$date_measured) # extract just the month

HeronMonthly <- CleanHeron %>% #Sum daily data into monthly
  group_by(yr,mo) %>%
  summarize(MonthlyRelease = sum(Release))

HeronMonthly$Date = make_date(year = HeronMonthly$yr, month = HeronMonthly$mo, day = 1) #Re-format date column 

HeronMonthlyReleases <- HeronMonthly %>% #Select and rename relevant columns
  ungroup() %>%
  select(MonthlyRelease,Date) %>%
  rename(Release = MonthlyRelease)

####Write to CSV#### 
write_csv(HeronMonthlyReleases,file = "data/processed/HeronMonthlyReleases") #write to csv
