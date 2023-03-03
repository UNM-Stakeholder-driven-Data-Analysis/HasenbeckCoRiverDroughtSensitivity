####Read me #### 

#This script first cleans and then combines two SWSI datasets. Then it completes some data exploration tasks 

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

write_csv(SWSI1981to2023,"data/processed//SWSI1981to2023.csv")
str(SWSI1981to2023)


#HAVE DUPLICATE VALUES FOR PERIOD IN 2011 



#### Run this to get data for data exploration and normalization ####

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
  
  

#convert new column to factor 
SWSIdataexplore$basin = as.factor(SWSIdataexplore$basin)

write_csv(SWSIdataexplore,"data/processed/SWSIdataexplore.csv")
#### describe dataset size and structure ####

SWSIdataexplore <- read_csv("data/processed/SWSI1981to2023.csv")

SWSIdataexplore <- SWSIdataexplore %>%
  pivot_longer(cols = "Gunnison":"San_Juan", 
               names_to = "basin",
               values_to = "SWSI")


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
  SWSInorm %>% 
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

view(SWSInorm)
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

####distributions without normalization####
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


####distributions with normalization####
temp = SWSInorm
qqPlot(temp$SWSI); shapiro.test(temp$SWSI) # normal
View(temp)

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
hist(SWSInorm$SWSI,
     breaks = 50, 
     main="Distribution of all SWSI data",
     xlab="SWSI Value")

#### Density function ####
plot(density(SWSIdataexplore_r$SWSI, na.rm = T))
range(SWSIdataexplore_r$SWSI, na.rm = T)

plot(density(temp$SWSI, na.rm = T))


##### what happens if I log-transform it? NOTHING!#### 
temp = SWSIdataexplore_r[SWSIdataexplore_r$SWSI == "Colorado",]
qqPlot(log10(temp$SWSI)); shapiro.test(log10(temp$SWSI))


#### Normalizing SWSI values #### 

#Prep
SWSIdataexplore$SWSI = as.vector(SWSIdataexplore$SWSI) #tidying it up. I dont know if this is necessary.
SWSInorm = SWSIdataexplore

#Norming function: 
library(scales)
SWSInorm$SWSI = rescale(SWSInorm$SWSI, 
                        to = c(-4,4))

####

#### temporal autocorrelation: Colorado  ####
# I'm going to check these one site at a time and only of data with at least 100 obs in each site, as I am unlikely to analyze less frequently gathered data
SWSItemporal_r = 
  SWSIdataexplore %>% 
  group_by(Date, basin) %>% 
  arrange(Date)

# checking for temporal autocorrelation requires the data to be a time series object (read ?ts for details on this)
# To achieve this, I need regularly spaced data. This data is irregularly spaced, approximately monthly, but sometimes there are more than one observations per month or fewer
# I will start by averaging observations within the same month:
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


## finally, convert to a ts object
# a ts object is a vector of data taken sequentially through time. Required arguments are:
# - the data vector
# - the frequency, which is the number of observations per unit of time. Lots of ways to specify this. For monthly data, you can put in 12 and it will assume that's 12 obs in a year. Google for help for other frequencies.
# - the start, which specifies when the first obs occured. Lots of ways to specify this. For monthly data, you can put in c(year, month) and it will know what you mean. 
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

# checking for temporal autocorrelation requires the data to be a time series object (read ?ts for details on this)
# To achieve this, I need regularly spaced data. This data is irregularly spaced, approximately monthly, but sometimes there are more than one observations per month or fewer
# I will start by averaging observations within the same month:
dat_monthly = 
  SWSItemporal_r %>%
  mutate(yr = lubridate::year(Date)) %>%
  mutate(mo = lubridate::month(Date)) %>%
  dplyr::group_by(basin, SWSI, yr, mo) %>%
  summarise(Value.mn = mean(SWSI, na.rm = T)) %>%
  mutate(date = paste(yr, mo, "15", sep="-")) %>%
  mutate(date = as.Date(date))

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
# a ts object is a vector of data taken sequentially through time. Required arguments are:
# - the data vector
# - the frequency, which is the number of observations per unit of time. Lots of ways to specify this. For monthly data, you can put in 12 and it will assume that's 12 obs in a year. Google for help for other frequencies.
# - the start, which specifies when the first obs occured. Lots of ways to specify this. For monthly data, you can put in c(year, month) and it will know what you mean. 
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

####Temporal autocorrelation Yampa responses ####
#I have autcorrelation at lags 1-2, 
#Strongest autocorrelation at lag 1, less autocorrelation at lag 2
#Auto regressive process. autocorrelation is at lag 1, which indicates a random walk/AR1 process


#### temporal autocorrelation: Rio Grande  ####
# I'm going to check these one site at a time and only of data with at least 100 obs in each site, as I am unlikely to analyze less frequently gathered data
SWSItemporal_r = 
  SWSIdataexplore %>% 
  group_by(Date, basin) %>% 
  arrange(Date)

# checking for temporal autocorrelation requires the data to be a time series object (read ?ts for details on this)
# To achieve this, I need regularly spaced data. This data is irregularly spaced, approximately monthly, but sometimes there are more than one observations per month or fewer
# I will start by averaging observations within the same month:
dat_monthly = 
  SWSItemporal_r %>%
  mutate(yr = lubridate::year(Date)) %>%
  mutate(mo = lubridate::month(Date)) %>%
  dplyr::group_by(basin, SWSI, yr, mo) %>%
  summarise(Value.mn = mean(SWSI, na.rm = T)) %>%
  mutate(date = paste(yr, mo, "15", sep="-")) %>%
  mutate(date = as.Date(date))

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
# a ts object is a vector of data taken sequentially through time. Required arguments are:
# - the data vector
# - the frequency, which is the number of observations per unit of time. Lots of ways to specify this. For monthly data, you can put in 12 and it will assume that's 12 obs in a year. Google for help for other frequencies.
# - the start, which specifies when the first obs occured. Lots of ways to specify this. For monthly data, you can put in c(year, month) and it will know what you mean. 
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
#Auto regressive process. autocorrelation is at lag 1, which indicates a random walk/AR1 process


#### temporal autocorrelation: San Juan   ####
# I'm going to check these one site at a time and only of data with at least 100 obs in each site, as I am unlikely to analyze less frequently gathered data
SWSItemporal_r = 
  SWSIdataexplore %>% 
  group_by(Date, basin) %>% 
  arrange(Date)

# checking for temporal autocorrelation requires the data to be a time series object (read ?ts for details on this)
# To achieve this, I need regularly spaced data. This data is irregularly spaced, approximately monthly, but sometimes there are more than one observations per month or fewer
# I will start by averaging observations within the same month:
dat_monthly = 
  SWSItemporal_r %>%
  mutate(yr = lubridate::year(Date)) %>%
  mutate(mo = lubridate::month(Date)) %>%
  dplyr::group_by(basin, SWSI, yr, mo) %>%
  summarise(Value.mn = mean(SWSI, na.rm = T)) %>%
  mutate(date = paste(yr, mo, "15", sep="-")) %>%
  mutate(date = as.Date(date))

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
# a ts object is a vector of data taken sequentially through time. Required arguments are:
# - the data vector
# - the frequency, which is the number of observations per unit of time. Lots of ways to specify this. For monthly data, you can put in 12 and it will assume that's 12 obs in a year. Google for help for other frequencies.
# - the start, which specifies when the first obs occured. Lots of ways to specify this. For monthly data, you can put in c(year, month) and it will know what you mean. 
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
# I'm going to check these one site at a time and only of data with at least 100 obs in each site, as I am unlikely to analyze less frequently gathered data
SWSItemporal_r = 
  SWSIdataexplore %>% 
  group_by(Date, basin) %>% 
  arrange(Date)

# checking for temporal autocorrelation requires the data to be a time series object (read ?ts for details on this)
# To achieve this, I need regularly spaced data. This data is irregularly spaced, approximately monthly, but sometimes there are more than one observations per month or fewer
# I will start by averaging observations within the same month:
dat_monthly = 
  SWSItemporal_r %>%
  mutate(yr = lubridate::year(Date)) %>%
  mutate(mo = lubridate::month(Date)) %>%
  dplyr::group_by(basin, SWSI, yr, mo) %>%
  summarise(Value.mn = mean(SWSI, na.rm = T)) %>%
  mutate(date = paste(yr, mo, "15", sep="-")) %>%
  mutate(date = as.Date(date))

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
# a ts object is a vector of data taken sequentially through time. Required arguments are:
# - the data vector
# - the frequency, which is the number of observations per unit of time. Lots of ways to specify this. For monthly data, you can put in 12 and it will assume that's 12 obs in a year. Google for help for other frequencies.
# - the start, which specifies when the first obs occured. Lots of ways to specify this. For monthly data, you can put in c(year, month) and it will know what you mean. 
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
#Auto regressive process. autocorrelation is at lag 1, which indicates a random walk/AR1 process


#### temporal autocorrelation: Arkansas  ####
# I'm going to check these one site at a time and only of data with at least 100 obs in each site, as I am unlikely to analyze less frequently gathered data
SWSItemporal_r = 
  SWSIdataexplore %>% 
  group_by(Date, basin) %>% 
  arrange(Date)

# checking for temporal autocorrelation requires the data to be a time series object (read ?ts for details on this)
# To achieve this, I need regularly spaced data. This data is irregularly spaced, approximately monthly, but sometimes there are more than one observations per month or fewer
# I will start by averaging observations within the same month:
dat_monthly = 
  SWSItemporal_r %>%
  mutate(yr = lubridate::year(Date)) %>%
  mutate(mo = lubridate::month(Date)) %>%
  dplyr::group_by(basin, SWSI, yr, mo) %>%
  summarise(Value.mn = mean(SWSI, na.rm = T)) %>%
  mutate(date = paste(yr, mo, "15", sep="-")) %>%
  mutate(date = as.Date(date))

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
# a ts object is a vector of data taken sequentially through time. Required arguments are:
# - the data vector
# - the frequency, which is the number of observations per unit of time. Lots of ways to specify this. For monthly data, you can put in 12 and it will assume that's 12 obs in a year. Google for help for other frequencies.
# - the start, which specifies when the first obs occured. Lots of ways to specify this. For monthly data, you can put in c(year, month) and it will know what you mean. 
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
#Auto regressive process. autocorrelation is at lag 1, which indicates a random walk/AR1 process


#### temporal autocorrelation: Gunnison  ####
# I'm going to check these one site at a time and only of data with at least 100 obs in each site, as I am unlikely to analyze less frequently gathered data
SWSItemporal_r = 
  SWSIdataexplore %>% 
  group_by(Date, basin) %>% 
  arrange(Date)

# checking for temporal autocorrelation requires the data to be a time series object (read ?ts for details on this)
# To achieve this, I need regularly spaced data. This data is irregularly spaced, approximately monthly, but sometimes there are more than one observations per month or fewer
# I will start by averaging observations within the same month:
dat_monthly = 
  SWSItemporal_r %>%
  mutate(yr = lubridate::year(Date)) %>%
  mutate(mo = lubridate::month(Date)) %>%
  dplyr::group_by(basin, SWSI, yr, mo) %>%
  summarise(Value.mn = mean(SWSI, na.rm = T)) %>%
  mutate(date = paste(yr, mo, "15", sep="-")) %>%
  mutate(date = as.Date(date))

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
# a ts object is a vector of data taken sequentially through time. Required arguments are:
# - the data vector
# - the frequency, which is the number of observations per unit of time. Lots of ways to specify this. For monthly data, you can put in 12 and it will assume that's 12 obs in a year. Google for help for other frequencies.
# - the start, which specifies when the first obs occured. Lots of ways to specify this. For monthly data, you can put in c(year, month) and it will know what you mean. 
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
#Auto regressive process. autocorrelation is at lag 1, which indicates a random walk/AR1 process



#### temporal autocorrelation: all basins  ####
# I'm going to check these one site at a time and only of data with at least 100 obs in each site, as I am unlikely to analyze less frequently gathered data
SWSItemporal_r = 
  SWSIdataexplore %>% 
  group_by(Date, basin) %>% 
  arrange(Date)

# checking for temporal autocorrelation requires the data to be a time series object (read ?ts for details on this)
# To achieve this, I need regularly spaced data. This data is irregularly spaced, approximately monthly, but sometimes there are more than one observations per month or fewer
# I will start by averaging observations within the same month:
dat_monthly = 
  SWSItemporal_r %>%
  mutate(yr = lubridate::year(Date)) %>%
  mutate(mo = lubridate::month(Date)) %>%
  dplyr::group_by(basin, SWSI, yr, mo) %>%
  summarise(Value.mn = mean(SWSI, na.rm = T)) %>%
  mutate(date = paste(yr, mo, "15", sep="-")) %>%
  mutate(date = as.Date(date))

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
# a ts object is a vector of data taken sequentially through time. Required arguments are:
# - the data vector
# - the frequency, which is the number of observations per unit of time. Lots of ways to specify this. For monthly data, you can put in 12 and it will assume that's 12 obs in a year. Google for help for other frequencies.
# - the start, which specifies when the first obs occured. Lots of ways to specify this. For monthly data, you can put in c(year, month) and it will know what you mean. 
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
#Auto regressive process. autocorrelation is at lag 1, which indicates a random walk/AR1 process




#### check for spatial autocorrelation ####

# I'm interested in spatial and not temporal autocorrelation, so I am going to look at just a few observations across all sites

# reload and format data with all sites
dat_all = SWSIdataexplore
#remove duplicates
SWSIdataexplore[!duplicated(SWSIdataexplore$SWSI), ]

dat_all$Date = as.POSIXct(dat_all$Date,format="%m-%d-%y")
dat_all$SWSI = as.numeric(dat_all$SWSI)
dat_all$basin = as.factor(dat_all$basin)

# how many sites are there? ####
length(unique(dat_all$basin))
# 7 basins 

# what parameters were collected across all sites in June 1995?
dat_june2002 = dat_all[dat_all$Date >= as.POSIXct("2002-06-01") &
                         dat_all$Date < as.POSIXct("2002-07-01"),]
tb = as.data.frame( with(dat_june2002, table(basin, SWSI)) )
tb = tb[tb$Freq>0,]
tb2 = tb %>% group_by(basin) %>% summarise(n = n()) %>% arrange(desc(n))
head(tb2, 15)
#   parameter          # of sites it was collected at in June 1995
# 1 Bicarbonate        82
# 2 Calcium            82
# 3 Chloride           82
# 4 LabpH              82
# 5 LabTDS             82
# 6 Magnesium          82
# 7 Potassium          82
# 8 Sodium             82
# 9 Sulfate            82
# ^ these are good options for testing for spatial autocorrelation

### June 2002 SWSI Colorado
dat_june2002 = dat_all[dat_all$Date >= as.POSIXct("2002-06-01") &
                         dat_all$Date < as.POSIXct("2002-07-01"),]
temp = dat_june2002 %>%  filter(basin=="Colorado")
# randomly generate lat/lon for demo
set.seed(42)
temp$lat = runif(nrow(temp),35.090956,35.634117)
temp$lon = runif(nrow(temp),-107.65829,-106.65829)
## Moran.I
# generate an inverse distance matrix 
dists = as.matrix(dist(cbind(temp$lon, temp$lat)))
dists.inv = 1/dists
diag(dists.inv) = 0
# calculate Moran.I
Moran.I(temp$Value, dists.inv)
# we can NOT reject the null hypothesis that there is zero spatial autocorrelation present. In other words, there doesn't seem to be a lot of spatial autocorrelation. 
## Mantel test
# generate spatial distance matrix
site_dists = dist(cbind(temp$lon, temp$lat))
# generate response distance matrix 
resp_dists = dist(temp$Value)
# run Mantel test
mantel.rtest(site_dists, resp_dists, nrepet = 9999)
# 'observation' is the correlation between the distance matrices
# p value suggests that they are NOT correlated
# So, based on this test, there is no detectable correlation
## Map
proj = CRS("+proj=longlat +datum=WGS84")
temp_spatial  <- SpatialPointsDataFrame(coords= cbind(temp$lon, temp$lat),
                                        data = as.data.frame(cbind(temp$Site_Name, temp$Value)),
                                        proj4string = proj)
plot(temp_spatial)

# ect.......... for other parameters of interest and for a few other time points, depending on how your data is structured ...........

#
#### check correlation between variables ####

# first, returning to the dataset of just 3 sites and more than 100 obs per parameter (dat_r), reformat data to make it wider, such that parameters get their own columns. 

dat_r_long = SWSIdataexplore %>% 
  select(c(basin, Date, SWSI))%>%
  group_by(basin, Date, SWSI) %>%
  pivot_wider(names_from = basin, 
              values_from = SWSI,
              values_fill = list(Value = NA))

# reduce data down to Colorado
temp = dat_r_long %>% 
  select(Colorado = Colorado) 

temp$Colorado = as.double(unlist(temp$Colorado))
view(temp)
# make table of correlations (I am rounding and replacing low values with text so that it is easier to see results)
tab = round(as.data.frame(cor(cov(temp[,2], use="na.or.complete"))), 2)
tab[abs(tab)<0.4] = "no_corr"

# reduce data down to one site - VR-3
temp = dat_r_long %>% filter(Site_Name=="VR-3")
# plot correlations (of data columns only)
pairs.panels(temp[,3:24], scale=T)
pairs.panels(temp[,3:24], scale=F)
# make table of correlations (I am rounding and replacing low values with text so that it is easier to see results)
tab = round(as.data.frame(cor(cov(temp[,3:24], use="na.or.complete"))), 2)
tab[abs(tab)<0.4] = "no_corr"



