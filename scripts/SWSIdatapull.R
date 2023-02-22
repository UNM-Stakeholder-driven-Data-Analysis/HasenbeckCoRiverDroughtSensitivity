####Read me #### 
#The first part of this script pulls api data from the Colorado DWR SWSI database 
#Data is for 2010 to present
#Data source: https://data.colorado.gov/Water/DWR-Surface-Water-Supply-Index-by-Basin/gs59-iraj


#The second part of this script converts an excel file of historic SWSI data which was shared with me via email on 02/22/2023 by Joel Atwood, a Joel Atwood a Hydrologist with the NRCS's Colorado Snow Survey 


#### Pulling in 2010-present data from API####
library(dplyr)
library(httr)
SWSI2010tonow <- GET("https://data.colorado.gov/resource/gs59-iraj.csv")
SWSI2010tonow = GET("https://data.colorado.gov/resource/gs59-iraj.csv") %>%
  httr::content("parsed")

###Writing to csv###
write.csv(SWSI2010tonow,"data/raw/SWSI2010tonow.csv")

#### Converting 1981-2011 SWSI from Excel File to csv #### 
#install and load readxl package
install.packages('readxl')
library(readxl)

#import Excel file into R
SWSI1981to2011 <- read_excel("data/processed/swsi_data_1981-2011-with-headers.xlsx", 
                             #top line are headers 
                             col_names = TRUE, 
                             #import dates correctly
                             col_types = c("date", "numeric", "numeric","numeric","numeric","numeric","numeric","numeric"))

###Writing to csv###
write.csv(SWSI1981to2011,"data/raw/SWSI1981to2011.csv")

View(SWSI1981to2011)
