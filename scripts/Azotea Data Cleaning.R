
####Read me####

#This script cleans Azotea discharge data to prep it for simple linear regresssion 

####libraries ####

library(readr)
library(tidyverse)
library(lubridate)

####Reading in csv data####

Azotea1970 <- read.table(file = "data/raw/AzoteaUSGS1975-2008-column.csv",sep=";", header=TRUE) #From USGS, older diversion record
    
read_csv(file = "data/raw/AzoteaUSGS1975-2008.csv") #From USGS, older diversion record
Azotea2023 <- read_csv(file = "data/raw/Azotea Discharge - 1985-2023.csv", ) #From CO DWR, more recent record

View(Azotea1970)
View(Azotea2023)

Clean1970 <- Azotea1970 %>% 
  select(datetime,X99716_00060_00003) %>% #Selecting relevant columns
  rename(Discharge = X99716_00060_00003, 
         date_measured = datetime)

View(Clean1970)

Clean1970$`Obs Code`[AllTBDiversions$`Obs Code` == "M"] <- "modeled"   # model

