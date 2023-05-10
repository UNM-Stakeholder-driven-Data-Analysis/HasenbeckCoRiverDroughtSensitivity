#### read me ####

# This script models Denver data via linear regression

#### libraries ####

library(tidyverse)
library(lubridate)
library(forecast)
library(nlme)
library(zoo)
library(lme4)
library(visreg)


#These libraries didn't get used but may be helpful/are holdovers from past code. 
#library(car)
#library(beepr)
#library(gridExtra)
#library(MARSS)



#### load data ####

#SWSI#
SWSI = read.csv("data/processed/SWSI1981to2023.csv", header = T)
SWSI$Date = as.Date.character(SWSI$Date, format = "%Y-%m-%d")

#Pulling out Colorado SWSI. Will use this to interpolate diversions/outflows
SWSI_CO <- SWSI %>%
  dplyr::select(Date, Colorado) %>%
  rename("SWSI_values" = "Colorado") %>% 
  group_by(Date) %>%
  summarize(SWSI_values=mean(SWSI_values)) #Some dates have two entries. Avg the duplicates here. 

#### Interpolate missing values - Dillon Reservoir #### 
##Dillon Reservoir ## 
DillonReleases <- read_csv(file = "data/processed/DillonLakesMonthlyReleases") %>%
  rename("Discharge" = "Release") #using the same column name as in diversion data to simplify replication


#There are NAs when I combined data with SWSI. 
#Going to combine SWSI just to create a date column to introduce NAs where diversion data is missing.
#Then remove SWSI and interpolate missing values. 
DillonInterpolation <- full_join(DillonReleases,SWSI_CO, by = "Date") %>%#Combining SWSI by basin with diversion data
  select(Date,Discharge) 

sum(is.na(DillonInterpolation$Discharge))
#93 NAs. 

plot(read.zoo(DillonInterpolation, index.column=1, format="%Y-%m-%d")) #plot as time series
#Big outlier in 06/2011

#Replace outlier with NA so it will interpolate. 
DillonInterpolation$Discharge = replace(DillonInterpolation$Discharge, DillonInterpolation$Discharge > 110761, NA)

## fill gaps with linear interpolation ##
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling

# Make univariate zoo time series #
DillonInterpolation <- DillonInterpolation %>% arrange(Date)
ts.temp<-read.zoo(DillonInterpolation, index.column=1, format="%Y-%m-%d")

# Apply NA interpolation method: Using max gap of 12 months 
Dillon_filled = na.approx(ts.temp, na.rm = T, maxgap = 11)


plot(Dillon_filled)
plot(ts.temp)

par(mfrow=c(1,1)) # reset plotting window
# revert back to dfs
Dillon_filled = as.data.frame(Dillon_filled)
Dillon_filled$Date = DillonInterpolation$Date
names(Dillon_filled) = c(colnames(DillonInterpolation)[2],colnames(DillonInterpolation)[1])
Dillon_filled = na.trim(Dillon_filled, "both")

Dillon_filled %>% dplyr::select(Discharge, Date)

sum(is.na(Dillon_filled$Discharge))
#No more NAs.

Dillon_filled$Date = (as.Date(Dillon_filled$Date))


####Interpolate missing values - Gross Reservoir ####
##Gross Reservoir ## 
GrossReleases <- read_csv(file = "data/processed/GrossLakesMonthlyReleases") %>%
  rename("Discharge" = "Release") #using the same column name as in diversion data to simplify replication


#There are NAs when I combined data with SWSI. 
#Going to combine SWSI just to create a date column to introduce NAs where diversion data is missing.
#Then remove SWSI and interpolate missing values. 
GrossInterpolation <- full_join(GrossReleases,SWSI_CO, by = "Date") %>%#Combining SWSI by basin with diversion data
  select(Date,Discharge) 

sum(is.na(GrossInterpolation$Discharge))
#79 NAs. 
plot(read.zoo(GrossInterpolation, index.column=1, format="%Y-%m-%d")) #plot as time series
#data gap ~1990? And one REALLY big outlier. Occurs in 1997.   
# #Data starts in 1986-09-01, but year long data gap right before 1990-12-01. Continous data ends in 2023-03-01 
#I tried to fill two years of NAs, but resulted in unrealistic data pattern (no seasonal peak), so I am going to start my data after the gap.

GrossInterpolation <-  GrossInterpolation %>% filter(Date >= "1990-12-01") 

# Replace outlier with NA so it will interpolate. 
GrossInterpolation$Discharge = replace(GrossInterpolation$Discharge, GrossInterpolation$Discharge > 110761, NA)

## fill gaps with spline interpolation ##
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling

# Make univariate zoo time series #
ts.temp<-read.zoo(GrossInterpolation, index.column=1, format="%Y-%m-%d")

# Apply NA interpolation method: Using max gap of 7 months 
Gross_filled = na.spline(ts.temp, na.rm = T, maxgap = 7)

plot(Gross_filled)
plot(ts.temp)

par(mfrow=c(1,1)) # reset plotting window
# revert back to dfs
Gross_filled = as.data.frame(Gross_filled)
Gross_filled$Date = GrossInterpolation$Date
Gross_filled$Date = GrossInterpolation$Date
names(Gross_filled) = c(colnames(GrossInterpolation)[2],colnames(GrossInterpolation)[1])
Gross_filled = na.trim(Gross_filled, "both")
Gross_filled %>% dplyr::select(Discharge, Date)


sum(is.na(Gross_filled$Discharge))
#0 NAs. 

Gross_filled$Date = (as.Date(Gross_filled$Date))
#In case some interpolated values went negative, replace negative values with 0.
Gross_filled$Discharge[(Gross_filled$Discharge < 0)] = 0 

## Roberts #### 

RobertsDiversions <- read_csv(file = "data/processed/RobertsMonthlyDischarge") 

#There are no NAs but when I combined data with SWSI, I found data gaps. 
#Going to combine SWSI just to create a date column to introduce NAs where diversion data is missing.
#Then remove SWSI and interpolate missing values. 
#Roberts Data start: 	
#1963-10-01
#Data end: 	
#2023-03-01
RobertsClean <- full_join(RobertsDiversions,SWSI_CO, by = "Date") %>%#Combining SWSI by basin with diversion data
  select(Date, Discharge) 

sum(is.na(RobertsClean$Discharge))
#There are no NAS in discharge. 

plot(read.zoo(RobertsClean, index.column=1, format="%Y-%m-%d")) #plot as time series
#The June 1 2011 outlier is here. But it looks reasonable compared to runoff in neighboring months. 
#There is also a strange section with very little diversion between 1970-1980
#I am not going to interpolate any data in this set. 


#### Roberts Create time series and remove seasonality #### 

#prepping to remove seasonality: 

#set df:
Discharge_data <- RobertsClean

#create timeseries. IMPORTANT!!!!!!!!!! RESET START DATE TO DATA START DATE!!!! 
#Roberts start date: 1963-10-01. 
timeseries = ts(Discharge_data$Discharge, start = c(1963-10-01), frequency = 12)
#SERIOUSLY CHECK DATES!!!!!! 

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

sum(is.na(Discharge_data_DEs))
#decomposition introduced 11 NAs. Tried to replace them with via spine interpolation. 

## fill gaps with spline interpolation ##
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling
# Make univariate zoo time series #
ts.temp <- read.zoo(Discharge_data_DEs, index.column=1, format="%Y-%m-%d")
plot(ts.temp)
# Apply NA interpolation method: Using max gap of 11 mon 
Roberts_Decomp_filled = na.spline(ts.temp, na.rm = T, maxgap = 11)
plot(Roberts_Decomp_filled)

#There is a weird section between 1970-1980, but remember this was in the raw data, too, so I am not going to worry about it.

par(mfrow=c(1,1)) # reset plotting window

# revert back to df
Roberts_Decomp_filled = as.data.frame(Roberts_Decomp_filled)
Roberts_Decomp_filled$Date = as.Date(rownames(Roberts_Decomp_filled)) 
names(Roberts_Decomp_filled) = c(colnames(RobertsDiversions)[1],colnames(RobertsDiversions)[2])
Roberts_Decomp_filled = Roberts_Decomp_filled %>% dplyr::select(Discharge, Date)
Roberts_Decomp_filled = na.trim(Roberts_Decomp_filled)

#In case some interpolated values went negative, replace negative values with 0.
Roberts_Decomp_filled$Discharge[Roberts_Decomp_filled$Discharge < 0] = 0 

sum(is.na(Roberts_Decomp_filled))
#0NAs 
RobertsDecomp <- Roberts_Decomp_filled

#### Gross - Create time series and remove seasonality ####
## prep time series ##
##data explore## 
hist(Gross_filled$Discharge, breaks = 100)

sum(is.na(Gross_filled$Date))
#No NAs

sum(is.na(Gross_filled$Discharge))
#0 NAs

# check percentage of dataset with NAs 
sum(is.na(Gross_filled))/nrow(Gross_filled)*100
#0%


# need to do this to prep for removing seasonality
#set df:
Discharge_data <- Gross_filled

#Check time period!!!!!!
# Starts 1990-12-01

timeseries = ts(Discharge_data$Discharge, start = c(1990-12-01), frequency = 12)
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

#decomposition introduced 1 NAs. Replace them with via spine interpolation. 
sum(is.na(Discharge_data_DEs))
## fill gaps with spline interpolation ##
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling
# Make univariate zoo time series #
ts.temp <- read.zoo(Discharge_data_DEs, index.column=1, format="%Y-%m-%d")
plot(ts.temp)
# Apply NA interpolation method: Using max gap of 12 mon 
Gross_Decomp_filled = na.spline(ts.temp, na.rm = T, maxgap = 12)
plot(Gross_Decomp_filled)
par(mfrow=c(1,1)) # reset plotting window
# revert back to df
Gross_Decomp_filled = as.data.frame(Gross_Decomp_filled)
Gross_Decomp_filled$Date = as.Date(rownames(Gross_Decomp_filled)) 
names(Gross_Decomp_filled) = c(colnames(GrossReleases)[1],colnames(GrossReleases)[2])
Gross_Decomp_filled = Gross_Decomp_filled %>% dplyr::select(Discharge, Date)


#In case some interpolated values went negative, replace negative values with 0.
Gross_Decomp_filled$Discharge[Gross_Decomp_filled$Discharge < 0] = 0 


ggplot(Gross_Decomp_filled, aes(x=Date, y=Discharge))+
  geom_path() + geom_point() + theme_bw()

GrossDecomp <- Gross_Decomp_filled

#### Dillon - Create time series and remove seasonality ####
## prep time series ##
##data explore## 
hist(Dillon_filled$Discharge, breaks = 100)

sum(is.na(Dillon_filled$Date))
#No NAs

sum(is.na(Dillon_filled$Discharge))
#0 NAs

# check percentage of dataset with NAs 
sum(is.na(Dillon_filled))/nrow(Dillon_filled)*100
#0%


# need to do this to prep for removing seasonality
#set df:
Discharge_data <- Dillon_filled

#Check time period!!!!!!
# Starts 1981-06-01
timeseries = ts(Discharge_data$Discharge, start = c(1981-06-01), frequency = 12)
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

#decomposition introduced 1 NAs. Replace them with via spine interpolation. 
sum(is.na(Discharge_data_DEs))
## fill gaps with spline interpolation ##
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling
# Make univariate zoo time series #
ts.temp <- read.zoo(Discharge_data_DEs, index.column=1, format="%Y-%m-%d")
plot(ts.temp)
# Apply NA interpolation method: Using max gap of 7 mon 
Dillon_Decomp_filled = na.spline(ts.temp, na.rm = T, maxgap = 12)
plot(Dillon_Decomp_filled)
par(mfrow=c(1,1)) # reset plotting window
# revert back to df
Dillon_Decomp_filled = as.data.frame(Dillon_Decomp_filled)
Dillon_Decomp_filled$Date = as.Date(rownames(Dillon_Decomp_filled)) 
names(Dillon_Decomp_filled) = c(colnames(DillonReleases)[1],colnames(DillonReleases)[2])
Dillon_Decomp_filled = Dillon_Decomp_filled %>% dplyr::select(Discharge, Date)


#In case some interpolated values went negative, replace negative values with 0.
Dillon_Decomp_filled$Discharge[Dillon_Decomp_filled$Discharge < 0] = 0 

sum(is.na(Dillon_Decomp_filled))

ggplot(Dillon_Decomp_filled, aes(x=Date, y=Discharge))+
  geom_path() + geom_point() + theme_bw()

#### CO SWSI Prep for modeling ####

# check for duplicate date/time stamps
anyDuplicated(SWSI_CO$Date)

# check percentage of dataset with NAs 
sum(is.na(SWSI_CO))/nrow(SWSI_CO)*100
#No NAs! 

#### S Platte SWSI - Prep for modeling ####
## load data and format date/time ##
SWSI_Platte <- SWSI %>%
  dplyr::select(Date, South_Platte) %>%
  rename("SWSI_values" = "South_Platte") %>% 
  group_by(Date) %>% 
  summarize(SWSI_values=mean(SWSI_values)) #Some dates have two entries. Avg the duplicates here. 

# check for duplicate date/time stamps
anyDuplicated(SWSI_Platte$Date)

# check percentage of dataset with NAs 
sum(is.na(SWSI_Platte))/nrow(SWSI_Platte)*100
#No NAs! 

#### Roberts - S Platte SWSI using NLME p = 0 ####
Roberts_Decomp_SP_SWSI_Raw <- full_join(RobertsDecomp,SWSI_Platte, by = "Date")  #Combining SWSI by basin with diversion data

#POR for SWSI is different than Roberts. Remove dates where there are no Discharge values. 
Roberts_Decomp_SP_SWSI_Raw <- na.trim(Roberts_Decomp_SP_SWSI_Raw)

CombinedData <- Roberts_Decomp_SP_SWSI_Raw

###linear trend### 
#Ask auto-arima best fit. 
auto.arima(CombinedData$Discharge)

#ARIMA(1,1,2)
#first number is autoregressive coef 1
# middle is differencing data 1
#last number is moving average term 2

#Run ARMA p1q2 with scaled data 
CombinedData$yr = lubridate::year(CombinedData$Date)# extract just the year
CombinedData$scaled_yr = scale(CombinedData$yr, center = TRUE, scale = FALSE) #scale year to a 0 mean to incorporate as random effect
mod_ARMAp1q2 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=1,q=2), data = CombinedData) #run model

# extract and assess residuals: AMRAp1q2. 
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp1q2, type = "normalized"), main="Discharge adjusted, Raw SWSI ARMAp1q2 model residuals")
plot(resid(mod_ARMAp1q2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI ARMAp1q2 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp1q2, type = "normalized"), main="Discharge adjusted, Raw SWSI p1q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp1q2, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp1q2, type = "normalized"))

#some temporal autocorrelation at lag 15 and 24. Ok to use. 
summary(mod_ARMAp1q2)

Roberts_SP_ARMAp1q2 <- mod_ARMAp1q2

#Plot result 
visreg(Roberts_SP_ARMAp1q2, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Outflow Discharge") +
  ggtitle("Dillon Outflows by S Platte SWSI")


# saving the plot as png 
ggsave("Roberts_SP_ARMAp1q2result.png", path = "results/graphs/")




#### Roberts - CO SWSI linear model w seasonal correction on Roberts data - ARIMA model p = 0.0123   ####

Roberts_Decomp_CO_SWSI_Raw <- full_join(RobertsDecomp,SWSI_CO, by = "Date")  #Combining SWSI by basin with diversion data

#POR for SWSI is different than Roberts. Remove dates where there are no Discharge values. 
Roberts_Decomp_CO_SWSI_Raw <- na.trim(Roberts_Decomp_CO_SWSI_Raw)

CombinedData <- Roberts_Decomp_CO_SWSI_Raw
###linear trend### 
#Ask auto-arima best fit. 
auto.arima(CombinedData$Discharge)

#ARIMA(1,1,2)
#first number is autoregressive coef 1
# middle is differencing data 1
#last number is moving average term 2

#Run ARMA p1q2 with scaled data 
CombinedData$yr = lubridate::year(CombinedData$Date)# extract just the year
CombinedData$scaled_yr = scale(CombinedData$yr, center = TRUE, scale = FALSE) #scale year to a 0 mean to incorporate as random effect
mod_ARMAp1q2 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=1,q=2), data = CombinedData) #run model

# extract and assess residuals: AMRAp1q2. 
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp1q2, type = "normalized"), main="Discharge adjusted, Raw SWSI ARMAp1q2 model residuals")
plot(resid(mod_ARMAp1q2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI ARMAp1q2 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp1q2, type = "normalized"), main="Discharge adjusted, Raw SWSI p1q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp1q2, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp1q2, type = "normalized"))

#some temporal autocorrelation at lag 15 and 24. Ok to use. 

Roberts_CO_ARMAp1q2 <- mod_ARMAp1q2

summary(mod_ARMAp1q2)

#Plot result 
visreg(Roberts_CO_ARMAp1q2, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Outflow Discharge") +
  ggtitle("Dillon Outflows by S Platte SWSI")


# saving the plot as png 
ggsave("Roberts_CO_ARMAp1q2result.png", path = "results/graphs/")

####  Gross - CO SWSI linear model w seasonal correction on HT data p = 0.0333 ####
Gross_Decomp_CO_SWSI_Raw <- full_join(GrossDecomp,SWSI_CO, by = "Date")  #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI


#POR for discharge data is older than for SWSI. Remove dates where there are no SWSI values. 
Gross_Decomp_CO_SWSI_Raw = na.trim(Gross_Decomp_CO_SWSI_Raw)

CombinedData <- Gross_Decomp_CO_SWSI_Raw

###linear trend### 
#Ask auto-arima best fit. 
auto.arima(CombinedData$Discharge)

#ARIMA(0,0,1)
#first number is autoregressive coef 0
# middle is differencing data 0
#last number is moving average term 1

#Run AR1 with scaled data 
CombinedData$yr = lubridate::year(CombinedData$Date)# extract just the year
CombinedData$scaled_yr = scale(CombinedData$yr, center = TRUE, scale = FALSE) #scale year to a 0 mean to incorporate as random effect

#Scaled AR1 model
mod_Ar1 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corAR1(), data = CombinedData) #run model

# extract and assess residuals: Ar1
par(mfrow=c(1,3))
Acf(resid(mod_Ar1, type = "normalized"), main="Discharge adjusted, Raw SWSI Ar1 model residuals")
plot(resid(mod_Ar1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI Ar1 model residuals"); abline(h=0)
qqnorm(resid(mod_Ar1, type = "normalized"), main="Discharge adjusted, Raw SWSI p1q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_Ar1, type = "normalized"))$statistic,2))); qqline(resid(mod_Ar1, type = "normalized"))

Gross_CO_AR1 <- mod_Ar1

summary(Gross_CO_AR1)

#Plot result 
visreg(Gross_CO_AR1, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Outflow Discharge") +
  ggtitle("Gross Outflows by Colorado SWSI")


# saving the plot as png 
ggsave("Gross_CO_AR1result.png", path = "results/graphs/")


####Gross - SP SWSI linear model w seasonal correction on HT data p = 0.0663 ####
Gross_Decomp_SP_SWSI_Raw <- full_join(GrossDecomp,SWSI_Platte, by = "Date")  #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI

#POR for discharge data is older than for SWSI. Remove dates where there are no SWSI values. 
Gross_Decomp_SP_SWSI_Raw = na.trim(Gross_Decomp_SP_SWSI_Raw)

CombinedData <- Gross_Decomp_SP_SWSI_Raw


### linear trends ###

###linear trend### 
#Ask auto-arima best fit. 
auto.arima(CombinedData$Discharge)

#ARIMA(0,0,1)
#first number is autoregressive coef 0
# middle is differencing data 0
#last number is moving average term 1

#Run AR1 with scaled data 
CombinedData$yr = lubridate::year(CombinedData$Date)# extract just the year
CombinedData$scaled_yr = scale(CombinedData$yr, center = TRUE, scale = FALSE) #scale year to a 0 mean to incorporate as random effect

#Scaled AR1 model
mod_Ar1 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corAR1(), data = CombinedData) #run model

# extract and assess residuals: Ar1
par(mfrow=c(1,3))
Acf(resid(mod_Ar1, type = "normalized"), main="Discharge adjusted, Raw SWSI Ar1 model residuals")
plot(resid(mod_Ar1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI Ar1 model residuals"); abline(h=0)
qqnorm(resid(mod_Ar1, type = "normalized"), main="Discharge adjusted, Raw SWSI p1q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_Ar1, type = "normalized"))$statistic,2))); qqline(resid(mod_Ar1, type = "normalized"))

Gross_SP_AR1 <- mod_Ar1
summary(Gross_SP_AR1)


#Plot result 
visreg(Gross_SP_AR1, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Outflow Discharge") +
  ggtitle("Gross Outflows by South Platte SWSI")


# saving the plot as png 
ggsave("Gross_SP_AR1result.png", path = "results/graphs/")


####Dillon - CO SWSI linear model w seasonal correction on HT data p =  0.8546 ####
Dillon_Decomp_CO_SWSI_Raw <- full_join(Dillon_Decomp_filled,SWSI_CO, by = "Date")  #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI

#POR for discharge data is different than for SWSI. Remove dates where there are no SWSI values. 
Dillon_Decomp_CO_SWSI_Raw = na.trim(Dillon_Decomp_CO_SWSI_Raw)

CombinedData <- Dillon_Decomp_CO_SWSI_Raw


###linear trend### 
#Ask auto-arima best fit. 
auto.arima(CombinedData$Discharge)

#ARIMA(2,0,4)
#first number is autoregressive coef 2
# middle is differencing data 0
#last number is moving average term 4

#Run ARMAp2q4 with scaled data 
CombinedData$yr = lubridate::year(CombinedData$Date)# extract just the year
CombinedData$scaled_yr = scale(CombinedData$yr, center = TRUE, scale = FALSE) #scale year to a 0 mean to incorporate as random effect

#Scaled ARMAp2q4 model
mod_ARMAp2q4 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=2,q=4), data = CombinedData) #run model

# extract and assess residuals: ARMAp2q4
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp2q4, type = "normalized"), main="Discharge adjusted, Raw SWSI ARMAp2q4 model residuals")
plot(resid(mod_ARMAp2q4, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI ARMAp2q4 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp2q4, type = "normalized"), main="Discharge adjusted, Raw SWSI p1q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp2q4, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp2q4, type = "normalized"))

Dillon_CO_ARMAp2q4 <- mod_ARMAp2q4

summary(Dillon_CO_ARMAp2q4)

#Signficant temporal autocorrelation. 

#Plot result 
visreg(Dillon_CO_ARMAp2q4, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Outflow Discharge") +
  ggtitle("Dillon Outflows by Colorado SWSI")


# saving the plot as png 
ggsave("Dillon_CO_ARMAp2q4result.png", path = "results/graphs/")


####Dillon - SP SWSI linear model w seasonal correction on HT data p =  0.8546 ####
Dillon_Decomp_SP_SWSI_Raw <- full_join(Dillon_Decomp_filled,SWSI_Platte, by = "Date")  #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI

#POR for discharge data is different than for SWSI. Remove dates where there are no SWSI values. 
Dillon_Decomp_SP_SWSI_Raw = na.trim(Dillon_Decomp_SP_SWSI_Raw)

CombinedData <- Dillon_Decomp_SP_SWSI_Raw


###linear trend### 
#Ask auto-arima best fit. 
auto.arima(CombinedData$Discharge)

#ARIMA(2,1,4)
#first number is autoregressive coef 2
# middle is differencing data 1
#last number is moving average term 4

#Run ARMAp2q4 with scaled data 
CombinedData$yr = lubridate::year(CombinedData$Date)# extract just the year
CombinedData$scaled_yr = scale(CombinedData$yr, center = TRUE, scale = FALSE) #scale year to a 0 mean to incorporate as random effect

#Scaled ARMAp2q4 model
mod_ARMAp2q4 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=2,q=4), data = CombinedData) #run model

# extract and assess residuals: ARMAp2q4
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp2q4, type = "normalized"), main="Discharge adjusted, Raw SWSI ARMAp2q4 model residuals")
plot(resid(mod_ARMAp2q4, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI ARMAp2q4 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp2q4, type = "normalized"), main="Discharge adjusted, Raw SWSI p1q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp2q4, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp2q4, type = "normalized"))

Dillon_SP_ARMAp2q4 <- mod_ARMAp2q4

summary(Dillon_SP_ARMAp2q4) #p=0.1564

#Signficant temporal autocorrelation. 

#Plot result 
visreg(Dillon_SP_ARMAp2q4, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Outflow Discharge") +
  ggtitle("Dillon Outflows by South Platte SWSI")


# saving the plot as png 
ggsave("Dillon_SP_ARMAp2q4result.png", path = "results/graphs/")

