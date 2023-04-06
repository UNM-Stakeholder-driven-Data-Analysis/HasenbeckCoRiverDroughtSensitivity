



#### SWSI autocorrelation correction ####

#### read me ####

# the purpose of this script is to demonstrate statisitcal testing of trends by two methods

# Here, I use monthly data with no NAs and with seasonality removed via decomposition. 
# It's worth exploring what happens and how you need to adjust models if you use higher frequency data, retain NAs, retain seasonality, or use other methods to remove it.

#### libraries ####

library(tidyverse)
library(lubridate)
library(forecast)
library(MARSS)
library(nlme)
library(zoo)
library(beepr)
library(gridExtra)
library(lme4)
library(car)
library(visreg)


####SWSI####
#### load data and format date/time ####

SWSI = read.csv("data/processed/SWSI1981to2023.csv", header = T)
SWSI$Date = as.Date.character(SWSI$Date, format = "%Y-%m-%d")
View(SWSI)
#### prep time series ####

sum(is.na(SWSI$Date))
#No NAs. 

# remove dup time stamps by taking mean of values
sum(duplicated(SWSI$Date))
#15 duplicates

SWSI[which(duplicated(SWSI$Date)),]
SWSI = SWSI %>% 
  pivot_longer(cols = "Gunnison":"San_Juan", 
                               names_to = "basin",
                               values_to = "SWSI_values") %>%
  group_by(Date) %>% 
  summarise(SWSI_values = mean(SWSI_values, na.rm =T))

write_csv(SWSI,file = "data/processed/SWSIduplicatesAvged")

# chec for duplicate date/time stamps
anyDuplicated(SWSI$Date)
# check percentage of dataset with NAs - this is important to document!
sum(is.na(SWSI))/nrow(SWSI)*100
#No NAs! 

#### create time series ####

# need to do this to prep for removing seasonality

SWSI_ts = ts(SWSI$SWSI_values, start = c(1981-06-01), frequency = 12)
head(SWSI)

par(mfrow=c(1,1))
plot(SWSI)

#### remove seasonality ####

# examine seasonality
par(mfrow=c(3,1))
plot(SWSI_ts)
Acf(SWSI_ts)
Pacf(SWSI_ts)

# decompose into additive components
plot(decompose(SWSI_ts))
# decompose into multiplicative components
plot(decompose(SWSI_ts, type="multiplicative"))
# extract components from multiplicative
SWSI_ts_decomp = decompose(SWSI_ts, type="multiplicative")
SWSI_ts_trend = SWSI_ts_decomp$trend
SWSI_ts_remainder = SWSI_ts_decomp$random
# save de-seasoned ts
SWSI_ts_DEs = SWSI_ts_trend * SWSI_ts_remainder

# compare original to de-seasoned ts
par(mfrow=c(3,2))
plot(SWSI_ts)
plot(SWSI_ts_DEs)
Acf(SWSI_ts)
Acf(SWSI_ts_DEs)
Pacf(SWSI_ts)
Pacf(SWSI_ts_DEs)
View(SWSI_DEs)
# revert back to df
SWSI_DEs = as.data.frame(SWSI_ts_DEs)
SWSI_DEs$Date = SWSI$Date
names(SWSI_DEs) = c("SWSI_values","Date")
SWSI_DEs = SWSI_DEs %>% dplyr::select(Date, SWSI_values) %>% arrange(Date)
SWSI_DEs = na.trim(SWSI_DEs, "both")

ggplot(SWSI_DEs, aes(x=Date, y=SWSI_values, color = ))+
  geom_path() + geom_point() + theme_bw()


#### Azotea - Create time series and remove seasonality ####
## load data and format date/time ##
AzoteaDiversions <- read_csv(file = "data/processed/AzoteaMonthlyDischarge")
View(AzoteaDiversions)

AzoteaDiversions$Date = as.Date(AzoteaDiversions$Date, "%y-%m-%d")

## prep time series ##
sum(is.na(AzoteaDiversions$Date))
#No NAs

sum(is.na(AzoteaDiversions$Discharge))
#46 NAs

# check percentage of dataset with NAs - this is important to document!
sum(is.na(AzoteaDiversions))/nrow(AzoteaDiversions)*100
#7%

## fill gaps with spline interpolation ##
# for calculating long-term trends, you can be pretty liberal with how large of gaps you fill. However, if you go too big with a spline interpolation, you'll get wacky results (ALWAYS EXAMINE THE RESULTS OF GAP FILLING!!). To strike this balance, I'm filling gaps of up to five days here. 
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling

# Make univariate zoo time series #
ts.temp<-read.zoo(AzoteaDiversions, index.column=2, format="%Y-%m-%d")

# ‘order.by’ are not unique warning suggests duplicate time stamps. I found that this is due to time zone changes, so nothing to worry about for regular time steps. 
plot(ts.temp)
# Apply NA interpolation method
Azotea_filled = na.spline(ts.temp, na.rm = T, maxgap = 7)
plot(Azotea_filled)

#Converting negative values to 0. 
par(mfrow=c(1,1)) # reset plotting window
# revert back to df
Azotea_filled = as.data.frame(Azotea_filled)
Azotea_filled$Date = AzoteaDiversions$Date
names(Azotea_filled) = c(colnames(AzoteaDiversions)[1],colnames(AzoteaDiversions)[2])
Azotea_filled = Azotea_filled %>% dplyr::select(Discharge, Date)
Azotea_filled$Discharge[Azotea_filled$Discharge < 0] = 0 #replace negative extrapolated values with 0.
View(Azotea_filled)
# check NAs that are left
sum(is.na(Azotea_filled$Date))
#No more NAs. 

# need to do this to prep for removing seasonality
#set df:
Discharge_data <- Azotea_filled

timeseries = ts(Discharge_data$Discharge, start = c(1970-10-01), frequency = 12)

head(timeseries)

par(mfrow=c(1,1))
plot(timeseries)

### remove seasonality ###

# examine seasonality
par(mfrow=c(3,1))
plot(timeseries)
Acf(timeseries)
Pacf(timeseries)

# decompose into additive components
plot(decompose(timeseries))
# decompose into multiplicative components
plot(decompose(timeseries, type="multiplicative"))
# extract components from multiplicative
timeseries_decomp = decompose(timeseries, type="multiplicative")
timeseries_trend = timeseries_decomp$trend
timeseries_remainder = timeseries_decomp$random
# save de-seasoned ts
timeseries_DEs = timeseries_trend * timeseries_remainder

# compare original to de-seasoned ts
par(mfrow=c(3,2))
plot(timeseries)
plot(timeseries_DEs)
Acf(timeseries)
Acf(timeseries_DEs)
Pacf(timeseries)
Pacf(timeseries_DEs)

# revert back to df
Discharge_data_DEs = as.data.frame(timeseries_DEs)
Discharge_data_DEs$Date = Discharge_data$Date
names(Discharge_data_DEs) = c("Discharge","Date")
Discharge_data_DEs = Discharge_data_DEs %>% dplyr::select(Date, Discharge) %>% arrange(Date)
Discharge_data_DEs = na.trim(Discharge_data_DEs, "both")

ggplot(Discharge_data_DEs, aes(x=Date, y=Discharge))+
  geom_path() + geom_point() + theme_bw()

Azotea_Corrected <-Discharge_data_DEs

#### Heron - Create time series and remove seasonality ####
## load data and format date/time ##
HeronReleases <- read_csv(file = "data/processed/HeronMonthlyReleases") %>%
  rename("Discharge" = "Release")
View(HeronReleases)

HeronReleases$Date = as.Date(HeronReleases$Date, "%y-%m-%d")

## prep time series ##
sum(is.na(HeronReleases$Date))
#No NAs

sum(is.na(HeronReleases$Discharge))
#2 NAs

# check percentage of dataset with NAs - this is important to document!
sum(is.na(HeronReleases))/nrow(HeronReleases)*100
#1%

## fill gaps with spline interpolation ##
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling

# Make univariate zoo time series #
ts.temp<-read.zoo(HeronReleases, index.column=2, format="%Y-%m-%d")

# ‘order.by’ are not unique warning suggests duplicate time stamps. I found that this is due to time zone changes, so nothing to worry about for regular time steps. 
plot(ts.temp)
# Apply NA interpolation method
Heron_filled = na.spline(ts.temp, na.rm = T, maxgap = 7)
plot(Heron_filled)

#Converting negative values to 0. 
par(mfrow=c(1,1)) # reset plotting window
# revert back to df
Heron_filled = as.data.frame(Heron_filled)
Heron_filled$Date = HeronReleases$Date
names(Heron_filled) = c(colnames(HeronReleases)[1],colnames(HeronReleases)[2])
Heron_filled = Heron_filled %>% dplyr::select(Discharge, Date)
View(Heron_filled)
# check NAs that are left
sum(is.na(Heron_filled$Date))
#No more NAs. 

# need to do this to prep for removing seasonality
#set df:
Discharge_data <- Heron_filled

timeseries = ts(Discharge_data$Discharge, start = c(2008-01-01), frequency = 12)

head(timeseries)

par(mfrow=c(1,1))
plot(timeseries)

### remove seasonality ###

# examine seasonality
par(mfrow=c(3,1))
plot(timeseries)
Acf(timeseries)
Pacf(timeseries)

# decompose into additive components
plot(decompose(timeseries))
# decompose into multiplicative components
plot(decompose(timeseries, type="multiplicative"))
# extract components from multiplicative
timeseries_decomp = decompose(timeseries, type="multiplicative")
timeseries_trend = timeseries_decomp$trend
timeseries_remainder = timeseries_decomp$random
# save de-seasoned ts
timeseries_DEs = timeseries_trend * timeseries_remainder

# compare original to de-seasoned ts
par(mfrow=c(3,2))
plot(timeseries)
plot(timeseries_DEs)
Acf(timeseries)
Acf(timeseries_DEs)
Pacf(timeseries)
Pacf(timeseries_DEs)

# revert back to df
Discharge_data_DEs = as.data.frame(timeseries_DEs)
Discharge_data_DEs$Date = Discharge_data$Date
names(Discharge_data_DEs) = c("Discharge","Date")
Discharge_data_DEs = Discharge_data_DEs %>% dplyr::select(Date, Discharge) %>% arrange(Date)
Discharge_data_DEs = na.trim(Discharge_data_DEs, "both")

ggplot(Discharge_data_DEs, aes(x=Date, y=Discharge))+
  geom_path() + geom_point() + theme_bw()

HeronCorrected <- Discharge_data_DEs

#### CO SWSI - Create time series and remove seasonality ####
## load data and format date/time ##
SWSI_CO <- read_csv(file = "data/processed/SWSI1981to2023.csv") %>%
  dplyr::select(Date,Colorado) %>%
  rename("SWSI_values" = "Colorado")

View(SWSI_CO)

SWSI_CO$Date = as.Date(SWSI_CO$Date, "%y-%m-%d")

## prep time series ##
sum(is.na(SWSI_CO$Date))
#No NAs

sum(is.na(SWSI_CO$SWSI_values))
#No NAs

# need to do this to prep for removing seasonality
#set df:
SWSI_data <- SWSI_CO

timeseries = ts(SWSI_data$SWSI_values, start = c(2008-01-01), frequency = 12)

head(timeseries)

par(mfrow=c(1,1))
plot(timeseries)

### remove seasonality ###

# examine seasonality
par(mfrow=c(3,1))
plot(timeseries)
Acf(timeseries)
Pacf(timeseries)

# decompose into additive components
plot(decompose(timeseries))
# decompose into multiplicative components
plot(decompose(timeseries, type="multiplicative"))
# extract components from multiplicative
timeseries_decomp = decompose(timeseries, type="multiplicative")
timeseries_trend = timeseries_decomp$trend
timeseries_remainder = timeseries_decomp$random
# save de-seasoned ts
timeseries_DEs = timeseries_trend * timeseries_remainder

# compare original to de-seasoned ts
par(mfrow=c(3,2))
plot(timeseries)
plot(timeseries_DEs)
Acf(timeseries)
Acf(timeseries_DEs)
Pacf(timeseries)
Pacf(timeseries_DEs)

# revert back to df
SWSI_data_DEs = as.data.frame(timeseries_DEs)
SWSI_data_DEs$Date = SWSI_data$Date
names(SWSI_data_DEs) = c("SWSI_values","Date")
SWSI_data_DEs = SWSI_data_DEs %>% dplyr::select(Date, SWSI_values) %>% arrange(Date)
SWSI_data_DEs = na.trim(SWSI_data_DEs, "both")

ggplot(SWSI_data_DEs, aes(x=Date, y=SWSI_values))+
  geom_path() + geom_point() + theme_bw()

CO_SWSI_Corrected <- SWSI_data_DEs

#### RG SWSI - Create time series and remove seasonality ####
## load data and format date/time ##
SWSI_RG <- read_csv(file = "data/processed/SWSI1981to2023.csv") %>%
  dplyr::select(Date,Rio_Grande) %>%
  rename("SWSI_values" = "Rio_Grande")


SWSI_RG$Date = as.Date(SWSI_RG$Date, "%y-%m-%d")

## prep time series ##
sum(is.na(SWSI_RG$Date))
#No NAs

sum(is.na(SWSI_RG$SWSI_values))
#No NAs

# need to do this to prep for removing seasonality
#set df:
SWSI_data <- SWSI_RG

timeseries = ts(SWSI_data$SWSI_values, start = c(2008-01-01), frequency = 12)

head(timeseries)

par(mfrow=c(1,1))
plot(timeseries)

### remove seasonality ###

# examine seasonality
par(mfrow=c(3,1))
plot(timeseries)
Acf(timeseries)
Pacf(timeseries)

# decompose into additive components
plot(decompose(timeseries))
# decompose into multiplicative components
plot(decompose(timeseries, type="multiplicative"))
# extract components from multiplicative
timeseries_decomp = decompose(timeseries, type="multiplicative")
timeseries_trend = timeseries_decomp$trend
timeseries_remainder = timeseries_decomp$random
# save de-seasoned ts
timeseries_DEs = timeseries_trend * timeseries_remainder

# compare original to de-seasoned ts
par(mfrow=c(3,2))
plot(timeseries)
plot(timeseries_DEs)
Acf(timeseries)
Acf(timeseries_DEs)
Pacf(timeseries)
Pacf(timeseries_DEs)

# revert back to df
SWSI_data_DEs = as.data.frame(timeseries_DEs)
SWSI_data_DEs$Date = SWSI_data$Date
names(SWSI_data_DEs) = c("SWSI_values","Date")
SWSI_data_DEs = SWSI_data_DEs %>% dplyr::select(Date, SWSI_values) %>% arrange(Date)
SWSI_data_DEs = na.trim(SWSI_data_DEs, "both")

ggplot(SWSI_data_DEs, aes(x=Date, y=SWSI_values))+
  geom_path() + geom_point() + theme_bw()

RG_SWSI_Corrected <- SWSI_data_DEs


###Azotea - CO SWSI - Linear model ####
Azotea_CO_SWSI <- 
  full_join(Azotea_Corrected,CO_SWSI_Corrected, by = "Date") %>%
  filter(Date >= "1981-12-01") %>% #No SWSI data before this date.
  filter(Date <= "2022-02-01") #No SWSI data after this date.

CombinedData <- Azotea_CO_SWSI
View(CombinedData)
## plot the data ###
CombinedData %>%
  ggplot(aes(x = Discharge, y = SWSI_values)) + 
  geom_point()
