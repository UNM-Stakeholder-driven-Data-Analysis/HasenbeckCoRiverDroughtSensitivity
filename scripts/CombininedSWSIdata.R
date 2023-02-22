####Read me #### 

#This script first cleans and then combines two SWSI datasets.

#libraries 
library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)

SWSI1981to2011 <- read.csv("data/raw/SWSI1981to2011.csv")
SWSI2010tonow <- read.csv("data/raw/SWSI2010tonow.csv")


####Cleaning SWSI 2010 to now data####
  
SWSI2010tonowCLEAN <- SWSI2010tonow %>%
  select("basin","report_year","report_month","swsi") %>% #select only columns of interest
  pivot_wider(names_from = "basin", values_from = "swsi") %>% #pivot to a wider table to match format of past data
  mutate(Date_Recorded = paste(report_year,report_month,"1", sep = "-")) %>%  #combine year and month columns into one date, add 01 as day
  select(!(report_year:report_month)) #remove year and month columns


View(SWSI2010tonowCLEAN)
    
    
