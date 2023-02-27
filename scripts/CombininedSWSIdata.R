####Read me #### 

#This script first cleans and then combines two SWSI datasets.

#libraries 
library(tidyverse)
library(dplyr)
library(lubridate)
library(psych) # to plot pair-wise correlations
library(car) # I like their qq plot fxn
library(tsibble) # useful for creating time series objects
library(forecast) # I like their Acf fxn
library(ape) # for spatial autocorrelation
library(ade4)# for spatial autocorrelation
library(rgdal) # for mapping



SWSI1981to2011 <- read.csv("data/raw/SWSI1981to2011.csv")
SWSI2010tonow <- read.csv("data/raw/SWSI2010tonow.csv")

####Cleaning SWSI 2010 to now data####
  
SWSI2010tonowCLEAN <- SWSI2010tonow %>%
  select("basin","report_year","report_month","swsi") %>% #select only columns of interest
  pivot_wider(names_from = "basin", values_from = "swsi") %>% #pivot to a wider table to match format of past data
  mutate(Date_Recorded = paste(report_year,report_month,"1", sep = "-")) %>%  #combine year and month columns into one date, add 01 as day
  select(!(report_year:report_month)) #remove year and month columns

#### Joining 2010 and 1981 datasets####
SWSIjoin <- full_join(SWSI1981to2011,
           SWSI2010tonowCLEAN,
          join_by = (
            "Date" = "Date_Recorded" +
            "Gunnison" = "Gunnison" +
            "Colorado" ="Colorado" +
            "Arkansas" = "Arkansas"+
            "South.Platte" = "South Platte" +
            "Yampa..White...N..Platte" = "Yampa-White" +
            "Rio.Grande" = "Rio Grande" +
            "San.Juan..Animas..Dolores...San.Miguel" = "San Juan-Dolores"+
            keep = FALSE)
)
          
SWSI1981to2023 <- SWSIjoin  %>%
  #Merge columns
  unite(Date, "Date","Date_Recorded", na.rm = TRUE) %>%
  unite(Colorado, "Colorado", "Colorado", na.rm = TRUE) %>%
  unite(Gunnison, "Gunnison", na.rm = TRUE) %>%
  unite(Arkansas, "Arkansas", na.rm = TRUE) %>%
  unite(South_Platte, "South Platte", "South.Platte", na.rm = TRUE) %>%
  unite(Yampa_White_N_Platte, "Yampa..White...N..Platte", "Yampa-White", na.rm = TRUE) %>%
  unite(Rio_Grande, "Rio.Grande", "Rio Grande", na.rm = TRUE) %>%
  unite(San_Juan, "San.Juan..Animas..Dolores...San.Miguel", "San Juan-Dolores", na.rm = TRUE) %>% 
  #remove un-needed X column 
  select(!X)

###change data types ###
#change dates to dates 
  SWSI1981to2023$Date = as.Date(as.character(SWSI1981to2023$Date,format = "Y!-m!-d!"))
#change basin values to values 
  SWSI1981to2023$Gunnison = as.numeric(SWSI1981to2023$Gunnison)
  SWSI1981to2023$Colorado= as.numeric(SWSI1981to2023$Colorado)
  SWSI1981to2023$South_Platte = as.numeric (SWSI1981to2023$South_Platte)
  SWSI1981to2023$Yampa_White_N_Platte = as.numeric(SWSI1981to2023$Yampa_White_N_Platte) 
  SWSI1981to2023$Arkansas = as.numeric(SWSI1981to2023$Arkansas)
  SWSI1981to2023$Rio_Grande = as.numeric(SWSI1981to2023$Rio_Grande)  
  SWSI1981to2023$San_Juan = as.numeric(SWSI1981to2023$San_Juan)


str(SWSI1981to2023)


#HAVE DUPLICATE VALUES FOR PERIOD IN 2011 


####Data exploration ####

#Pivot longer to complete data exploration: 
SWSIdataexplore <- pivot_longer(SWSI1981to2023,
             cols = Gunnison:San_Juan,
            cols_vary = "fastest",
            names_to = "basin",
            values_to = "SWSI",
            values_drop_na = FALSE)

#tibble converted to df 

SWSIdataexplore <- as.data.frame(SWSIdataexplore)
  
  

#convert new column to factor 
SWSIdataexplore$basin = as.factor(SWSIdataexplore$basin)


                    
#### describe dataset size and structure ####

head(SWSIdataexplore)
#Date, single SWSI observation sorted by basin. 

str(SWSIdataexplore)
#parameters were converted to date, numerics and factors. 
#Value is the measure of the SWSI
#Total observations: 3570
#Total number of variables: 3 (date, basins, SWSI value)

with(dat, table(Parameter, Site_Name))





### check timesteps by looking and time series of most frequently collected parameters
# make dataset of one of the most frequently collected parameters
SWSI_lts = 
  SWSIdataexplore %>% 
  group_by(basin) %>% 
  arrange(Date)
# add year and day of year for plotting
SWSI_lts$year = lubridate::year(SWSI_lts$Date)
SWSI_lts$doy = lubridate::yday(SWSI_lts$Date)
# plot
ggplot(data=SWSI_lts, aes(x=doy, y=SWSI_lts$SWSI, color=basin))+
  geom_point() + geom_path()+
  facet_wrap(~year, scales= "fixed")+
  scale_y_continuous(breaks = c(-4,-2,0,2,4), labels = c(-4,-2,0,2,4)) +
  theme(legend.title = element_blank()) +
  theme_bw()

#2010 and 2011 have duplicate values that are different from eachother. 
# timesteps are regular, monthly
# timesteps are not sub-daily, at most frequent are approximately monthly

# can also look at single years in detail
ggplot(data=SWSI_lts, aes(x=Date, y=SWSI, color=basin))+
  geom_point() + geom_path()+
  xlim(c(as.Date("2010-01-01"), as.Date("2012-04-01")))+
  theme(legend.title = element_blank()) +
  theme_bw()
#Some basins have similar duplicates, others are more different. 


### How many variables are in your dataset?
str(SWSIdataexplore)
# 3: date, basin, SWSI

### How many observations are in your dataset?
nrow(SWSIdataexplore)
#3570 total

with(SWSIdataexplore, table(SWSIdataexplore$SWSI, SWSIdataexplore$basin))
#Not all basins experience extremes.
range(with(SWSIdataexplore, table(SWSIdataexplore$Date, SWSIdataexplore$SWSI)))


### Are the data nested in time or space?
# Yes in time - observations were collected repeatedly on a monthly schedule, with some data overlapping
# Yes in space - observations were created for each basin. 

#### describe data types ####

str(SWSIdataexplore)
summary(SWSIdataexplore$basin)
# continuous intervals. 

#### check distributions ####


  SWSIdataexplore %>% 
  group_by(basin, SWSI) %>% 
  arrange(Date)
summary(SWSIdataexplore_r$SWSI)

#Summary
#Min.     1st Qu.  Median Mean    3rd Qu.    Max.    NA's 
# -4.100  -1.350   0.400   0.322   2.000   4.200       1 
#Skews positive. 

####None of the data is normal, but all follows a similar pattern. #### 

temp = SWSIdataexplore_r
qqPlot(temp$SWSI); shapiro.test(temp$SWSI) # normal

qqPlot(temp$SWSI[temp$basin=="Colorado"]); shapiro.test(temp$SWSI[temp$basin=="Colorado"]) 
#Shapiro-Wilk normality test Colorado 
#data:  temp$SWSI[temp$basin == "Colorado"]
#W = 0.97114, p-value = 1.774e-08

qqPlot(temp$SWSI[temp$basin=="Yampa_White_N_Platte"]); shapiro.test(temp$SWSI[temp$basin=="Yampa_White_N_Platte"]) 
#Shapiro-Wilk normality test Yampa
#data:  temp$SWSI[temp$basin == "Yampa_White_N_Platte"]
#W = 0.94996, p-value = 4.212e-12

qqPlot(temp$SWSI[temp$basin=="Rio_Grande"]); shapiro.test(temp$SWSI[temp$basin=="Rio_Grande"]) 
#Shapiro-Wilk normality test RG
#Shapiro-Wilk normality test
#data:  temp$SWSI[temp$basin == "Rio_Grande"]
#W = 0.97476, p-value = 1.058e-07


qqPlot(temp$SWSI[temp$basin=="Arkansas"]); shapiro.test(temp$SWSI[temp$basin=="Arkansas"]) 
#[1] 275  32
#Shapiro-Wilk normality test Arkansas
#data:  temp$SWSI[temp$basin == "Arkansas"]
#W = 0.97526, p-value = 1.364e-07

qqPlot(temp$SWSI[temp$basin=="South_Platte"]); shapiro.test(temp$SWSI[temp$basin=="South_Platte"]) 
#Shapiro-Wilk normality test
#data:  temp$SWSI[temp$basin == "South_Platte"]
#W = 0.9485, p-value = 2.513e-12

qqPlot(temp$SWSI[temp$basin=="San_Juan"]); shapiro.test(temp$SWSI[temp$basin=="San_Juan"]) 
#[1] 253 254
#Shapiro-Wilk normality test
#data:  temp$SWSI[temp$basin == "San_Juan"]
#W = 0.97125, p-value = 1.872e-08

qqPlot(temp$SWSI[temp$basin=="Gunnison"]); shapiro.test(temp$SWSI[temp$basin=="Gunnison"]) 
#[1] 500 501
#Shapiro-Wilk normality test
#data:  temp$SWSI[temp$basin == "Gunnison"]
#W = 0.95845, p-value = 8.386e-11


#### Making histogram ####

hist(SWSIdataexplore_r$SWSI,
     breaks = 9)

#### Density function ####
plot(density(SWSIdataexplore_r$SWSI, na.rm = T))
range(SWSIdataexplore_r$SWSI, na.rm = T)

plot(density(temp$SWSI, na.rm = T))


##### what happens if I log-transform it? NOTHING!#### 
temp = SWSIdataexplore_r[SWSIdataexplore_r$SWSI == "Colorado",]
qqPlot(log10(temp$SWSI)); shapiro.test(log10(temp$SWSI))

#### temporal autocorrelation ####
