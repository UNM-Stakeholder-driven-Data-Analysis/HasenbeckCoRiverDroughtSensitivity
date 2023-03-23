
####Read me####

#This script cleans Azotea discharge data to prep it for simple linear regresssion 

####libraries ####

library(readr)
library(tidyverse)
library(lubridate)
library(zoo) 

####Reading in csv data####

Azotea1970 <- read.table(file = "data/raw/AzoteaUSGS1975-2008-column.csv",sep=";", header=TRUE) #From USGS, older diversion record

Azotea2023 <- read_csv(file = "data/raw/Azotea Discharge - 1985-2023.csv",) #From CO DWR, more recent record

####Cleaning Data ####
## Cleaning 1970 data 
Clean1970 <- Azotea1970 %>% 
  select(datetime,X99716_00060_00003) %>% #Selecting relevant columns
  rename(Discharge = X99716_00060_00003, 
         date_measured = datetime) #renaming columns so they make sense

Clean1970$date_measured = as.Date(Clean1970$date_measured, format="%m/%d/%y") #formatting date
as.double(Clean1970$Discharge) #defining data 

##cleaning 2023 data 
Clean2023 <- Azotea2023 %>% 
  select(meas_date,`Streamflow Value`) %>% #selecting relevant columns
  rename(date_measured = meas_date,
         Discharge = `Streamflow Value`) #renaming columns so they make sense 

Clean2023$date_measured = as.Date(Clean2023$date_measured, format="%m/%d/%Y") #formatting date
as.double(Clean2023$Discharge)

##Joining both data sets 

AzoteaJoin <- full_join(Clean2023, Clean1970) 

#Data has duplicates. These are going to be averaged. 
ggplot(data=AzoteaJoin, aes(x=date_measured, y=Discharge))+
  geom_point() +
  xlim(c(as.Date("1985-01-01"), as.Date("2008-12-01")))+
  theme(legend.title = element_blank()) +
  theme_bw()

sum(is.na(Clean2023$Discharge)) #206 NAs
sum(is.na(Clean1970$Discharge)) #275 NAs


###Converting daily to monthly measurement

AzoteaJoin$yr = lubridate::year(AzoteaJoin$date_measured)# extract just the year
AzoteaJoin$mo = lubridate::month(AzoteaJoin$date_measured) # extract just the month

AzoteaMonthly <- AzoteaJoin %>% #Sum daily data into monthly
  group_by(yr,mo) %>%
  summarize(MonthlyDischarge = sum(Discharge))

AzoteaMonthly$Date = make_date(year = AzoteaMonthly$yr, month = AzoteaMonthly$mo, day = 1) #Re-format date column 

AzoteaDischarge <- AzoteaMonthly %>% #Select and rename relevant columns
  ungroup() %>%
  select(MonthlyDischarge,Date) %>%
  rename(Discharge = MonthlyDischarge)

write_csv(AzoteaDischarge,file = "data/processed/AzoteaMonthlyDischarge") #write to csv

