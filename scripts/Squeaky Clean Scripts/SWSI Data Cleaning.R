####Read me #### 

#This script first cleans and then combines two SWSI datasets. Then it completes some data exploration tasks 

#libraries 
library(tidyverse)
library(dplyr)
library(lubridate)
library(psych) # pair-wise correlations
library(car) # qq plot fxn
library(tsibble) #creating time series objects
library(forecast) # Acf fxn


SWSI1981to2011 <- read.csv("data/raw/SWSI1981to2011.csv")
SWSI2010tonow <- read.csv("data/raw/SWSI2010tonow.csv")

####Cleaning SWSI 2010 to now data####
  
#Re-scale more recent data from -4.2 to 4.2 to -4 to 4. 
SWSI2010tonow$swsi = scales :: rescale(SWSI2010tonow$swsi, to = c(-4,4))

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

write_csv(SWSI1981to2023,"data/processed/SWSI1981to2023.csv")
str(SWSI1981to2023)


#HAVE DUPLICATE VALUES FOR PERIOD IN 2011. This is addressed in the model scripts. 



#### Data exploration and distribution ####

SWSI1981to2023dataexplore <- read_csv("data/processed/SWSI1981to2023.csv")
#Pivot longer to complete data exploration: 
SWSIdataexplore <- pivot_longer(SWSI1981to2023dataexplore,
             cols = Gunnison:San_Juan,
            cols_vary = "fastest",
            names_to = "basin",
            values_to = "SWSI",
            values_drop_na = FALSE)

#tibble converted to df 

SWSIdataexplore <- as.data.frame(SWSIdataexplore)

View(SWSI1981to2023)

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

View(SWSIdataexplore)

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
  theme_bw() +
  ggtitle("SWSI over Time") + # for the main title
  xlab("Day of Year") + # for the x axis label
  ylab("SWSI Value") # for the y axis label

duplicates(SWSI_lts)

# For unscaled parameters 2010-2011 only 
SWSI_lts = 
  SWSI_lts %>% 
  group_by(basin) %>% 
  arrange(Date)
# add year and day of year for plotting
SWSI_lts$year = lubridate::year(SWSI_lts$Date)
SWSI_lts$doy = lubridate::yday(SWSI_lts$Date)
# plot
unscaledplot <- ggplot(data=SWSI_lts, aes(x=Date, y=SWSI, color=basin))+
  geom_point() + geom_path()+
  xlim(c(as.Date("2010-01-01"), as.Date("2012-04-01")))+
  theme(legend.title = element_blank()) +
  theme_bw() + 
  ggtitle("SWSI Values by Basin, unscaled data") + # for the main title
  xlab("Day of Year") + # for the x axis label
  ylab("SWSI Value") # for the y axis label

# For scaled parameters 2010-2011 only 
SWSI_ltsnorm = 
  SWSI_lts %>% 
  group_by(basin) %>% 
  arrange(Date)
# add year and day of year for plotting
SWSI_ltsnorm$year = lubridate::year(SWSI_lts$Date)
SWSI_ltsnorm$doy = lubridate::yday(SWSI_lts$Date)
# plot
ggplot(data=SWSI_ltsnorm, aes(x=Date, y=SWSI, color=basin))+
  geom_point() + geom_path()+
  xlim(c(as.Date("2010-01-01"), as.Date("2012-04-01")))+
  theme(legend.title = element_blank()) +
  theme_bw() + 
  ggtitle("SWSI Values by Basin, scaled data") + # for the main title
  xlab("Day of Year") + # for the x axis label
  ylab("SWSI Value") # for the y axis label

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

SWSIdataexplore_r <- SWSIdataexplore
summary(SWSIdataexplore_r$SWSI)

#Summary
#Min.     1st Qu.  Median Mean    3rd Qu.    Max.    NA's 
# -4.100  -1.350   0.400   0.322   2.000   4.200       1 
#Skews positive. 

####None of the data is normal, but all follows a similar pattern. #### 

####distributions####
temp = SWSIdataexplore_r
qqPlot(temp$SWSI); shapiro.test(temp$SWSI) # normal


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
#non-normalized data 
hist(SWSIdataexplore_r$SWSI,
     breaks = 100
    )


#normalized data 
hist(SWSIdataexplore$SWSI,
     breaks = 50, 
     main="Distribution of all SWSI data",
     xlab="SWSI Value")

#### Density function ####
plot(density(SWSIdataexplore_r$SWSI, na.rm = T))
range(SWSIdataexplore_r$SWSI, na.rm = T)


#### temporal autocorrelation: Colorado  ####
SWSItemporal_r = 
  SWSIdataexplore %>% 
  group_by(Date, basin) %>% 
  arrange(Date)

# Prep to make time series to check for temporal autocorrelation. 
dat_monthly = 
  SWSItemporal_r %>%
  mutate(yr = lubridate::year(Date)) %>%
  mutate(mo = lubridate::month(Date)) %>%
  dplyr::group_by(basin, SWSI, yr, mo) %>%
  summarise(Value.mn = mean(SWSI, na.rm = T)) %>%
  mutate(date = paste(yr, mo, "15", sep="-")) %>%
  mutate(date = as.Date(date))

### subset data to be one site and one parameter
temp = dat_monthly[dat_monthly$basin == "Colorado",]

### make this a time series object
## first, make doubly sure that the data is arranged by time before converting to ts object!
temp = temp %>% arrange(date) 
## second, make the spacing of dates consistent and fill in missing obs with NA. This is a handy fxn. You can also create a df of evenly spaced dates and left_join the data to this.
temp_ts =
  #remove duplicates
  temp[!duplicated(temp$SWSI), ]

temp_ts = temp_ts %>%
  complete(date = seq(min(date), max(date), by = "1 month"), 
           fill = list(value = NA)) %>%
  as_tsibble(index = date)


## convert to a ts object
# Required arguments are:
# - the data vector
# - the frequency, which is the number of observations per unit of time. Lots of ways to specify this. For monthly data, you can put in 12 and it will assume that's 12 obs in a year. Google for help for other frequencies.
# - the start, which specifies when the first obs occured. Lots of ways to specify this. For monthly data, you can put in c(year, month) and it will know what you mean. 
head (temp_ts)
#Data starts 1981-06-01
temp_ts = ts(temp_ts$Value.mn, frequency=12, start=c(1980, 01)) 
# check that you specified the ts correctly
print(temp_ts, calendar = T) 
### now we're ready to check for temporal autocorrelation in this ts!
# I prefer the forecast pkg's Acf fxn over base R acf() because Acf() doesn't include 0 (which is always 1) and shows month #s by default instead of decimal years. Note the different options for dealing with NAs and how this changes the results (see ?na.fail and ?Acf for details). 
forecast::Acf(temp_ts, na.action = na.pass) #fills in with likely points
forecast::Acf(temp_ts, na.action = na.contiguous) #fills in based on longest time period with nas
forecast::Acf(temp_ts, na.action = na.interp) # uses time series model to fill in NA 

forecast::Pacf(temp_ts, na.action = na.pass)
forecast::Pacf(temp_ts, na.action = na.contiguous)
forecast::Pacf(temp_ts, na.action = na.interp)

####Temporal autocorrelation Colorado responses ####
#I have autcorrelation at lags 1-6, 10-20.
#Strongest autocorrelation at lag 1 with a bit of a strong aurocorrelation at lag 21 
#Auto regressive process. autocorrelation is at lag 1, which indicates a random walk/AR1 process



#### temporal autocorrelation:Yampa  ####
# I'm going to check these one site at a time and only of data with at least 100 obs in each site, as I am unlikely to analyze less frequently gathered data
SWSItemporal_r = 
  SWSIdataexplore %>% 
  group_by(Date, basin) %>% 
  arrange(Date)

# Prep to make timeseries 
### subset data to be one site and one parameter
temp = dat_monthly[dat_monthly$basin == "Yampa_White_N_Platte",]

### make this a time series object
## first, make doubly sure that the data is arranged by time before converting to ts object!
temp = temp %>% arrange(date) 
## second, make the spacing of dates consistent and fill in missing obs with NA. This is a handy fxn. You can also create a df of evenly spaced dates and left_join the data to this.
temp_ts =
  #remove duplicates
  temp[!duplicated(temp$SWSI), ]

temp_ts = temp_ts %>%
  complete(date = seq(min(date), max(date), by = "1 month"), 
           fill = list(value = NA)) %>%
  as_tsibble(index = date)


## finally, convert to a ts object
head (temp_ts)
temp_ts = ts(temp_ts$Value.mn, frequency=12, start=c(1980, 01)) 
# check that you specified the ts correctly
print(temp_ts, calendar = T) 

# I prefer the forecast pkg's Acf fxn over base R acf() because Acf() doesn't include 0 (which is always 1) and shows month #s by default instead of decimal years. Note the different options for dealing with NAs and how this changes the results (see ?na.fail and ?Acf for details). 
forecast::Acf(temp_ts, na.action = na.pass) #fills in with likely points
forecast::Acf(temp_ts, na.action = na.contiguous) #fills in based on longest time period with nas
forecast::Acf(temp_ts, na.action = na.interp) # uses time series model to fill in NA 

forecast::Pacf(temp_ts, na.action = na.pass)
forecast::Pacf(temp_ts, na.action = na.contiguous)
forecast::Pacf(temp_ts, na.action = na.interp)

####Temporal autocorrelation Yampa responses ####
#I have autcorrelation at lags 1-2, 
#Strongest autocorrelation at lag 1, less autocorrelation at lag 2


#### temporal autocorrelation: Rio Grande  ####
# I'm going to check these one site at a time and only of data with at least 100 obs in each site, as I am unlikely to analyze less frequently gathered data
SWSItemporal_r = 
  SWSIdataexplore %>% 
  group_by(Date, basin) %>% 
  arrange(Date)

### subset data to be one site and one parameter
temp = dat_monthly[dat_monthly$basin == "Rio_Grande",]

### make this a time series object
## first, make doubly sure that the data is arranged by time before converting to ts object!
temp = temp %>% arrange(date) 
## second, make the spacing of dates consistent and fill in missing obs with NA. This is a handy fxn. You can also create a df of evenly spaced dates and left_join the data to this.
temp_ts =
  #remove duplicates
  temp[!duplicated(temp$SWSI), ]

temp_ts = temp_ts %>%
  complete(date = seq(min(date), max(date), by = "1 month"), 
           fill = list(value = NA)) %>%
  as_tsibble(index = date)


## finally, convert to a ts object
head (temp_ts)
temp_ts = ts(temp_ts$Value.mn, frequency=12, start=c(1980, 01)) 
# check that you specified the ts correctly
print(temp_ts, calendar = T) 
### now we're ready to check for temporal autocorrelation in this ts!
# I prefer the forecast pkg's Acf fxn over base R acf() because Acf() doesn't include 0 (which is always 1) and shows month #s by default instead of decimal years. Note the different options for dealing with NAs and how this changes the results (see ?na.fail and ?Acf for details). 
forecast::Acf(temp_ts, na.action = na.pass) #fills in with likely points
forecast::Acf(temp_ts, na.action = na.contiguous) #fills in based on longest time period with nas
forecast::Acf(temp_ts, na.action = na.interp) # uses time series model to fill in NA 

forecast::Pacf(temp_ts, na.action = na.pass)
forecast::Pacf(temp_ts, na.action = na.contiguous)
forecast::Pacf(temp_ts, na.action = na.interp)

####Temporal autocorrelation Rio Grande responses ####
#I have autcorrelation at lags 1-5, 20.
#Strongest autocorrelation at lag 1 with a bit of a strong aurocorrelation at lag 21 


#### temporal autocorrelation: San Juan   ####

### subset data to be one site and one parameter
temp = dat_monthly[dat_monthly$basin == "San_Juan",]

### make this a time series object
## first, make doubly sure that the data is arranged by time before converting to ts object!
temp = temp %>% arrange(date) 
## second, make the spacing of dates consistent and fill in missing obs with NA. This is a handy fxn. You can also create a df of evenly spaced dates and left_join the data to this.
temp_ts =
  #remove duplicates
  temp[!duplicated(temp$SWSI), ]

temp_ts = temp_ts %>%
  complete(date = seq(min(date), max(date), by = "1 month"), 
           fill = list(value = NA)) %>%
  as_tsibble(index = date)


## finally, convert to a ts object
head (temp_ts)
temp_ts = ts(temp_ts$Value.mn, frequency=12, start=c(1980, 01)) 
# check that you specified the ts correctly
print(temp_ts, calendar = T) 
### now we're ready to check for temporal autocorrelation in this ts!
# I prefer the forecast pkg's Acf fxn over base R acf() because Acf() doesn't include 0 (which is always 1) and shows month #s by default instead of decimal years. Note the different options for dealing with NAs and how this changes the results (see ?na.fail and ?Acf for details). 
forecast::Acf(temp_ts, na.action = na.pass) #fills in with likely points
forecast::Acf(temp_ts, na.action = na.contiguous) #fills in based on longest time period with nas
forecast::Acf(temp_ts, na.action = na.interp) # uses time series model to fill in NA 

forecast::Pacf(temp_ts, na.action = na.pass)
forecast::Pacf(temp_ts, na.action = na.contiguous)
forecast::Pacf(temp_ts, na.action = na.interp)

####Temporal autocorrelation San Juan  responses ####
#I have autcorrelation at lags 1-10.
#Strongest autocorrelation at lag 1 
#Auto regressive process. autocorrelation is at lag 1, which indicates a random walk/AR1 process


#### temporal autocorrelation: S Platte  ####


### subset data to be one site and one parameter
temp = dat_monthly[dat_monthly$basin == "South_Platte",]

### make this a time series object
## first, make doubly sure that the data is arranged by time before converting to ts object!
temp = temp %>% arrange(date) 
## second, make the spacing of dates consistent and fill in missing obs with NA. This is a handy fxn. You can also create a df of evenly spaced dates and left_join the data to this.
temp_ts =
  #remove duplicates
  temp[!duplicated(temp$SWSI), ]

temp_ts = temp_ts %>%
  complete(date = seq(min(date), max(date), by = "1 month"), 
           fill = list(value = NA)) %>%
  as_tsibble(index = date)


## finally, convert to a ts object
head (temp_ts)
temp_ts = ts(temp_ts$Value.mn, frequency=12, start=c(1980, 01)) 
# check that you specified the ts correctly
print(temp_ts, calendar = T) 
### now we're ready to check for temporal autocorrelation in this ts!
# I prefer the forecast pkg's Acf fxn over base R acf() because Acf() doesn't include 0 (which is always 1) and shows month #s by default instead of decimal years. Note the different options for dealing with NAs and how this changes the results (see ?na.fail and ?Acf for details). 
forecast::Acf(temp_ts, na.action = na.pass) #fills in with likely points
forecast::Acf(temp_ts, na.action = na.contiguous) #fills in based on longest time period with nas
forecast::Acf(temp_ts, na.action = na.interp) # uses time series model to fill in NA 

forecast::Pacf(temp_ts, na.action = na.pass)
forecast::Pacf(temp_ts, na.action = na.contiguous)
forecast::Pacf(temp_ts, na.action = na.interp)

####Temporal autocorrelation S Platte responses ####
#I have autcorrelation at lags 1-7, 12-18.
#Strongest autocorrelation at lag 1. 

#### temporal autocorrelation: Arkansas  ####
### subset data to be one site and one parameter
temp = dat_monthly[dat_monthly$basin == "Arkansas",]

### make this a time series object
## first, make doubly sure that the data is arranged by time before converting to ts object!
temp = temp %>% arrange(date) 
## second, make the spacing of dates consistent and fill in missing obs with NA. This is a handy fxn. You can also create a df of evenly spaced dates and left_join the data to this.
temp_ts =
  #remove duplicates
  temp[!duplicated(temp$SWSI), ]

temp_ts = temp_ts %>%
  complete(date = seq(min(date), max(date), by = "1 month"), 
           fill = list(value = NA)) %>%
  as_tsibble(index = date)


## finally, convert to a ts object
head (temp_ts)
temp_ts = ts(temp_ts$Value.mn, frequency=12, start=c(1980, 01)) 
# check that you specified the ts correctly
print(temp_ts, calendar = T) 
### now we're ready to check for temporal autocorrelation in this ts!
# I prefer the forecast pkg's Acf fxn over base R acf() because Acf() doesn't include 0 (which is always 1) and shows month #s by default instead of decimal years. Note the different options for dealing with NAs and how this changes the results (see ?na.fail and ?Acf for details). 
forecast::Acf(temp_ts, na.action = na.pass) #fills in with likely points
forecast::Acf(temp_ts, na.action = na.contiguous) #fills in based on longest time period with nas
forecast::Acf(temp_ts, na.action = na.interp) # uses time series model to fill in NA 

forecast::Pacf(temp_ts, na.action = na.pass)
forecast::Pacf(temp_ts, na.action = na.contiguous)
forecast::Pacf(temp_ts, na.action = na.interp)

####Temporal autocorrelation Arkansas  responses ####
#I have autcorrelation at lags 1-11, 10-20.
#Strongest autocorrelation at lag 1 with a bit of a strong aurocorrelation at lag 5 


#### temporal autocorrelation: Gunnison  ####

### subset data to be one site and one parameter
temp = dat_monthly[dat_monthly$basin == "Gunnison",]

### make this a time series object
## first, make doubly sure that the data is arranged by time before converting to ts object!
temp = temp %>% arrange(date) 
## second, make the spacing of dates consistent and fill in missing obs with NA. This is a handy fxn. You can also create a df of evenly spaced dates and left_join the data to this.
temp_ts =
  #remove duplicates
  temp[!duplicated(temp$SWSI), ]

temp_ts = temp_ts %>%
  complete(date = seq(min(date), max(date), by = "1 month"), 
           fill = list(value = NA)) %>%
  as_tsibble(index = date)


## finally, convert to a ts object
head (temp_ts)
temp_ts = ts(temp_ts$Value.mn, frequency=12, start=c(1980, 01)) 
# check that you specified the ts correctly
print(temp_ts, calendar = T) 
### now we're ready to check for temporal autocorrelation in this ts!
# I prefer the forecast pkg's Acf fxn over base R acf() because Acf() doesn't include 0 (which is always 1) and shows month #s by default instead of decimal years. Note the different options for dealing with NAs and how this changes the results (see ?na.fail and ?Acf for details). 
forecast::Acf(temp_ts, na.action = na.pass) #fills in with likely points
forecast::Acf(temp_ts, na.action = na.contiguous) #fills in based on longest time period with nas
forecast::Acf(temp_ts, na.action = na.interp) # uses time series model to fill in NA 

forecast::Pacf(temp_ts, na.action = na.pass)
forecast::Pacf(temp_ts, na.action = na.contiguous)
forecast::Pacf(temp_ts, na.action = na.interp)

####Temporal autocorrelation Gunnison responses ####
#I have autcorrelation at lags 1-6
#Strongest autocorrelation at lag 1 
