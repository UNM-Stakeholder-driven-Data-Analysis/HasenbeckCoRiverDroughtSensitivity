####Read me #### 
#This script pulls api data from the Colorado DWR SWSI database. 
#Data is for 2010 to present
#Data source: https://data.colorado.gov/Water/DWR-Surface-Water-Supply-Index-by-Basin/gs59-iraj


#### Pulling in data####
library(dplyr)
library(httr)
SWSI2010tonow <- GET("https://data.colorado.gov/resource/gs59-iraj.csv")
SWSI2010tonow = GET("https://data.colorado.gov/resource/gs59-iraj.csv") %>%
  httr::content("parsed")
View(SWSI2010tonow)


####Writing to csv####

write.csv(SWSI2010tonow,"data/raw/SWSI2010tonow.csv")


