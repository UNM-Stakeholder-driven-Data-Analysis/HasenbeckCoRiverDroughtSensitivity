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
library(lmeresampler)

#These libraries didn't get used but may be helpful/are holdovers from past code. 
#library(car)
#library(beepr)
#library(gridExtra)
#library(MARSS)



#### load data  ####

#SWSI#
SWSI = read.csv("data/processed/SWSI1981to2023.csv", header = T) %>% arrange(Date) 
SWSI$Date = as.Date.character(SWSI$Date, format = "%Y-%m-%d")

#Pulling out Colorado SWSI. Will use this to interpolate diversions/outflows
SWSI_CO <- SWSI %>%
  dplyr::select(Date, Colorado) %>%
  rename("SWSI_values" = "Colorado") %>% 
  group_by(Date) %>%
  summarize(SWSI_values=mean(SWSI_values)) %>% #Some dates have two entries. Avg the duplicates here. 
  arrange(Date)

##Dillon Reservoir ## 
DillonReleases <- read_csv(file = "data/processed/DillonLakesMonthlyReleases") %>%
  rename("Discharge" = "Release") %>% #using the same column name as in diversion data to simplify replication 
  arrange(Date)

##Gross Reservoir ## 
GrossReleases <- read_csv(file = "data/processed/GrossLakesMonthlyReleases") %>%
  rename("Discharge" = "Release") %>% arrange(Date) #using the same column name as in diversion data to simplify replication

#Roberts Tunnel Diversions 
RobertsDiversions <- read_csv(file = "data/processed/RobertsMonthlyDischarge") %>% arrange(Date)

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
  summarize(SWSI_values=mean(SWSI_values)) %>% arrange(Date) #Some dates have two entries. Avg the duplicates here. 

# check for duplicate date/time stamps
anyDuplicated(SWSI_Platte$Date)

# check percentage of dataset with NAs 
sum(is.na(SWSI_Platte))/nrow(SWSI_Platte)*100
#No NAs! 

#### Data exploration + Interpolate missing values - Dillon Reservoir ####


#There are NAs when I combined data with SWSI. 
#Going to combine SWSI just to create a date column to introduce NAs where diversion data is missing.
#Then remove SWSI and interpolate missing values. 
DillonInterpolation <- full_join(DillonReleases,SWSI_CO, by = "Date") %>%#Combining SWSI by basin with diversion data
  select(Date,Discharge) %>% arrange(Date)

sum(is.na(DillonInterpolation$Discharge))
#93 NAs. 

plot(read.zoo(DillonInterpolation, index.column=1, format="%Y-%m-%d")) #plot as time series
#Big outlier in 06/2011

#Replace outlier with NA so it will interpolate. 
DillonInterpolation$Discharge = replace(DillonInterpolation$Discharge, DillonInterpolation$Discharge > 75000.00, NA)

## fill gaps with linear interpolation ##
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling

# Make univariate zoo time series #
DillonInterpolation <- DillonInterpolation %>% arrange(Date)
ts.temp<-read.zoo(DillonInterpolation, index.column=1, format="%Y-%m-%d")

# Apply NA interpolation method: Using max gap of 12 months 
Dillon_filled = na.approx(ts.temp, na.rm = T, maxgap = 12)


plot(Dillon_filled)
plot(ts.temp)

par(mfrow=c(1,1)) # reset plotting window
# revert back to dfs
Dillon_filled = as.data.frame(Dillon_filled)
Dillon_filled$Date = DillonInterpolation$Date
names(Dillon_filled) = c(colnames(DillonInterpolation)[2],colnames(DillonInterpolation)[1])
Dillon_filled = na.trim(Dillon_filled, "both")

Dillon_filled <- Dillon_filled %>% dplyr::select(Discharge, Date) %>% arrange(Date)

sum(is.na(Dillon_filled$Discharge))
#No more NAs.

Dillon_filled$Date = (as.Date(Dillon_filled$Date))


#### Data exploration + Interpolate missing values - Gross Reservoir ####

#There are NAs when I combined data with SWSI. 
#Going to combine SWSI just to create a date column to introduce NAs where diversion data is missing.
#Then remove SWSI and interpolate missing values. 
GrossInterpolation <- full_join(GrossReleases,SWSI_CO, by = "Date") %>%#Combining SWSI by basin with diversion data
  select(Date,Discharge) %>% arrange(Date)

sum(is.na(GrossInterpolation$Discharge))
#79 NAs. 

plot(read.zoo(GrossInterpolation, index.column=1, format="%Y-%m-%d")) #plot as time series
#Big outlier in 1997-07-01

#Replace outlier with NA so it will interpolate. 
GrossInterpolation$Discharge = replace(GrossInterpolation$Discharge, GrossInterpolation$Discharge > 11076, NA)

## fill gaps with linear interpolation ##
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling

# Make univariate zoo time series #
GrossInterpolation <- GrossInterpolation %>% arrange(Date)
ts.temp<-read.zoo(GrossInterpolation, index.column=1, format="%Y-%m-%d")

# Apply NA interpolation method: Using max gap of 12 months 
Gross_filled = na.approx(ts.temp, na.rm = T, maxgap = 12)


plot(Gross_filled)
plot(ts.temp)

par(mfrow=c(1,1)) # reset plotting window
# revert back to dfs
Gross_filled = as.data.frame(Gross_filled)
Gross_filled$Date = GrossInterpolation$Date
names(Gross_filled) = c(colnames(GrossInterpolation)[2],colnames(GrossInterpolation)[1])
Gross_filled = na.trim(Gross_filled, "both")

Gross_filled <- Gross_filled %>% dplyr::select(Discharge, Date)
  

sum(is.na(Gross_filled$Discharge))
#13 NAs 1989-11-01 to 1990-11-01. Going to trim to 1990-12-01.


Gross_filled <- Gross_filled %>% dplyr::select(Discharge, Date) %>% 
  filter(Date >= "1990-12-01") 

sum(is.na(Gross_filled$Discharge))
#0 NAs 

Gross_filled$Date = (as.Date(Gross_filled$Date))

###  Data exploration - Roberts Tunnel  #### 
#There are no NAs but when I combined data with SWSI, I found data gaps. 
#Going to combine SWSI just to create a date column to introduce NAs where diversion data is missing.
#Then remove SWSI and interpolate missing values. 
#Roberts Data start: 	
#1963-10-01
#Data end: 	
#2023-03-01
RobertsClean <- full_join(RobertsDiversions,SWSI_CO, by = "Date") %>% #Combining SWSI by basin with diversion data
  select(Date, Discharge) %>% arrange(Date)

sum(is.na(RobertsClean$Discharge))
#There are no NAS in discharge. 

plot(read.zoo(RobertsClean, index.column=1, format="%Y-%m-%d")) #plot as time series
#The June 1 2011 outlier is here. But it looks reasonable compared to runoff in neighboring months. 
#There is also a strange section with very little diversion between 1980-1990
#I am not going to interpolate any data in this set. 


#### Roberts Create time series and remove seasonality #### 

#prepping to remove seasonality: 

#set df:
Discharge_data <- RobertsClean %>% arrange(Date)

#create timeseries. IMPORTANT!!!!!!!!!! RESET START DATE TO DATA START DATE!!!! 
#Roberts start date: 1963-10-01. 
timeseries = ts(Discharge_data$Discharge, start = c(1963, 10), frequency = 12)
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
#decomposition introduced 11 NAs. Replace them with linear interpolation. 

## fill gaps with linear interpolation ##
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling
# Make univariate zoo time series #
ts.temp <- read.zoo(Discharge_data_DEs, index.column=1, format="%Y-%m-%d")
plot(ts.temp)
# Apply NA interpolation method: Using max gap of 12 mon 
Roberts_Decomp_filled = na.approx(ts.temp, na.rm = T, maxgap = 12)
plot(Roberts_Decomp_filled)

#There is a weird section between 1980-1990, but remember this was in the raw data, too, so I am not going to worry about it.

par(mfrow=c(1,1)) # reset plotting window

# revert back to df
Roberts_Decomp_filled = as.data.frame(Roberts_Decomp_filled)
Roberts_Decomp_filled$Date = as.Date(rownames(Roberts_Decomp_filled)) 
names(Roberts_Decomp_filled) = c(colnames(RobertsDiversions)[1],colnames(RobertsDiversions)[2])
Roberts_Decomp_filled = Roberts_Decomp_filled %>% dplyr::select(Discharge, Date) %>% arrange(Date)
Roberts_Decomp_filled = na.trim(Roberts_Decomp_filled)

sum(is.na(Roberts_Decomp_filled))
#0 NAs 
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
Discharge_data <- Gross_filled %>% arrange(Date)

#Check time period!!!!!!
# Starts 1990-12-01

timeseries = ts(Discharge_data$Discharge, start = c(1990, 12), frequency = 12)
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

sum(is.na(Discharge_data_DEs))
#No NAs. 

GrossDecomp <- Discharge_data_DEs

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
Discharge_data <- Dillon_filled %>% arrange(Date)

#Check time period!!!!!!
# Starts 1989-03-01
timeseries = ts(Discharge_data$Discharge, start = c(1989, 03), frequency = 12)
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

sum(is.na(Discharge_data_DEs))
#No NAs. 
Dillon_Decomp_filled <- Discharge_data_DEs %>% arrange(Date)

#### Roberts - S Platte SWSI using NLME p = 0 ####
Roberts_Decomp_SP_SWSI_Raw <- full_join(RobertsDecomp,SWSI_Platte, by = "Date") %>% arrange(Date) #Combining SWSI by basin with diversion data

#POR for SWSI is different than Roberts. Remove dates where there are no Discharge values. 
Roberts_Decomp_SP_SWSI_Raw <- na.trim(Roberts_Decomp_SP_SWSI_Raw) 

CombinedData <- Roberts_Decomp_SP_SWSI_Raw %>% arrange(Date)

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
  ggtitle("Roberts Outflows by S Platte SWSI")

# saving the plot as png 
ggsave("Roberts_SP_ARMAp1q2result.png", path = "results/graphs/")


# bootstrap confidence intervals
Roberts_SP_ARMAp1q2_boot<-lmeresampler::bootstrap(model = Roberts_SP_ARMAp1q2, .f = fixef, type = "reb", B = 1000, reb_type = 2)
Roberts_SP_ARMAp1q2_CIs = confint(Roberts_SP_ARMAp1q2_boot, type="norm",level = 0.95)
plot(Roberts_SP_ARMAp1q2_boot, "beta.SWSI_values")
Roberts_SP_ARMAp1q2_CIplot =  plot(Roberts_SP_ARMAp1q2_boot, "beta.SWSI_values")


#### Roberts - CO SWSI linear model w seasonal correction on Roberts data - ARIMA model p = 0.0123   ####

Roberts_Decomp_CO_SWSI_Raw <- full_join(RobertsDecomp,SWSI_CO, by = "Date") %>% arrange(Date) #Combining SWSI by basin with diversion data

#POR for SWSI is different than Roberts. Remove dates where there are no Discharge values. 
Roberts_Decomp_CO_SWSI_Raw <- na.trim(Roberts_Decomp_CO_SWSI_Raw)

CombinedData <- Roberts_Decomp_CO_SWSI_Raw %>% arrange(Date)
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

#some temporal autocorrelation at 24. Ok to use. 

Roberts_CO_ARMAp1q2 <- mod_ARMAp1q2

summary(mod_ARMAp1q2)

#Plot result 
visreg(Roberts_CO_ARMAp1q2, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Outflow Discharge") +
  ggtitle("Roberts Outflows by Colorado SWSI")


# saving the plot as png 
ggsave("Roberts_CO_ARMAp1q2result.png", path = "results/graphs/")


# bootstrap confidence intervals
Roberts_CO_ARMAp1q2_boot<-lmeresampler::bootstrap(model = Roberts_CO_ARMAp1q2, .f = fixef, type = "reb", B = 1000, reb_type = 2)
Roberts_CO_ARMAp1q2_CIs = confint(Roberts_CO_ARMAp1q2_boot, type="norm",level = 0.95)
plot(Roberts_CO_ARMAp1q2_boot, "beta.SWSI_values")
Roberts_CO_ARMAp1q2_CIplot =  plot(Roberts_SP_ARMAp1q2_boot, "beta.SWSI_values")


#### Roberts joint boot strap plots ###
#Plot both bootstrap plots for both river basins against eachother. Colorado on bottom.  
gridExtra::grid.arrange(Roberts_SP_ARMAp1q2_CIplot, Roberts_CO_ARMAp1q2_CIplot, ncol=1)

# saving the plot as png 
ggsave("Roberts_ARMAp1q2_boot.png", path = "results/graphs/")

####  Gross - CO SWSI linear model w seasonal correction on Gross data p = 0.0516 ####
Gross_Decomp_CO_SWSI_Raw <- full_join(GrossDecomp,SWSI_CO, by = "Date") %>% arrange(Date)  #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI


#POR for discharge data is older than for SWSI. Remove dates where there are no SWSI values. 
Gross_Decomp_CO_SWSI_Raw = na.trim(Gross_Decomp_CO_SWSI_Raw) 

CombinedData <- Gross_Decomp_CO_SWSI_Raw %>% arrange(Date)

###linear trend### 
#Ask auto-arima best fit. 
auto.arima(CombinedData$Discharge)

#ARIMA(1,0,1)
#first number is autoregressive coef 1
# middle is differencing data 0
#last number is moving average term 1

#Run ARMA p1q1 with scaled data 
CombinedData$yr = lubridate::year(CombinedData$Date)# extract just the year
CombinedData$scaled_yr = scale(CombinedData$yr, center = TRUE, scale = FALSE) #scale year to a 0 mean to incorporate as random effect
mod_ARMAp1q1 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=1,q=1), data = CombinedData) #run model

# extract and assess residuals: AMRAp1q1. 
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp1q1, type = "normalized"), main="Discharge adjusted, Raw SWSI ARMAp1q1 model residuals")
plot(resid(mod_ARMAp1q1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI ARMAp1q1 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp1q1, type = "normalized"), main="Discharge adjusted, Raw SWSI p1q1 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp1q1, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp1q1, type = "normalized"))

#NO AUTOCORRELATION WOOHOO A GREAT MODEL FIT! 

Gross_CO_ARMAp1q1 <- mod_ARMAp1q1
summary(mod_ARMAp1q1)

#Plot result 
visreg(Gross_CO_ARMAp1q1, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Outflow Discharge") +
  ggtitle("Gross Outflows by Colorado SWSI")


# saving the plot as png 
ggsave("Gross_CO_ARMAp1q1_result.png", path = "results/graphs/")

# bootstrap confidence intervals
Gross_CO_ARMAp1q1_boot<-lmeresampler::bootstrap(model = Roberts_CO_ARMAp1q2, .f = fixef, type = "reb", B = 1000, reb_type = 2)
Gross_CO_ARMAp1q1_CIs = confint(Gross_CO_ARMAp1q1_boot, type="norm",level = 0.95)
plot(Gross_CO_ARMAp1q1_boot, "beta.SWSI_values")
Gross_CO_ARMAp1q1_CIplot = plot(Gross_CO_ARMAp1q1_boot, "beta.SWSI_values")


####Gross - SP SWSI linear model w seasonal correction on Gross data p = 0.1025 ####
Gross_Decomp_SP_SWSI_Raw <- full_join(GrossDecomp,SWSI_Platte, by = "Date")  %>% arrange(Date) #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI

#POR for discharge data is older than for SWSI. Remove dates where there are no SWSI values. 
Gross_Decomp_SP_SWSI_Raw = na.trim(Gross_Decomp_SP_SWSI_Raw)

CombinedData <- Gross_Decomp_SP_SWSI_Raw  %>% arrange(Date)


### linear trends ###

###linear trend### 
#Ask auto-arima best fit. 
auto.arima(CombinedData$Discharge)

#ARIMA(1,0,1)
#first number is autoregressive coef 1
# middle is differencing data 0
#last number is moving average term 1

#Run ARMA p1q1 with scaled data 
CombinedData$yr = lubridate::year(CombinedData$Date)# extract just the year
CombinedData$scaled_yr = scale(CombinedData$yr, center = TRUE, scale = FALSE) #scale year to a 0 mean to incorporate as random effect
mod_ARMAp1q1 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=1,q=1), data = CombinedData) #run model

# extract and assess residuals: AMRAp1q1. 
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp1q1, type = "normalized"), main="Discharge adjusted, Raw SWSI ARMAp1q1 model residuals")
plot(resid(mod_ARMAp1q1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI ARMAp1q1 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp1q1, type = "normalized"), main="Discharge adjusted, Raw SWSI p1q1 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp1q1, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp1q1, type = "normalized"))

Gross_SP_ARMAp1q1 <- mod_ARMAp1q1
summary(Gross_SP_ARMAp1q1)

#NO AUTOCORRELATION GREAT MODEL FIT! 

#Plot result 
visreg(Gross_SP_ARMAp1q1, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Outflow Discharge") +
  ggtitle("Gross Outflows by South Platte SWSI")


# saving the plot as png 
ggsave("Gross_SP_ARMAp1q1_result.png", path = "results/graphs/")


# bootstrap confidence intervals
Gross_SP_ARMAp1q1_boot<-lmeresampler::bootstrap(model = Gross_SP_ARMAp1q1, .f = fixef, type = "reb", B = 1000, reb_type = 2)
Gross_SP_ARMAp1q1_CIs = confint(Gross_SP_ARMAp1q1_boot, type="norm",level = 0.95)
plot(Gross_SP_ARMAp1q1_boot, "beta.SWSI_values")
Gross_SP_ARMAp1q1_CIplot = plot(Gross_SP_ARMAp1q1_boot, "beta.SWSI_values")


### Gross - Plot both basins bootstrap intervals against eachother. Colorado on bottom ###
gridExtra::grid.arrange(Gross_SP_ARMAp1q1_CIplot, Gross_CO_ARMAp1q1_CIplot, ncol=1)

# saving the plot as png 
ggsave("Gross_ARMAp1q1_boot.png", path = "results/graphs/")

####Dillon - CO SWSI linear model w seasonal correction on Dillon data p =  0.1137 ####
Dillon_Decomp_CO_SWSI_Raw <- full_join(Dillon_Decomp_filled,SWSI_CO, by = "Date") %>% arrange(Date)  #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI

#POR for discharge data is different than for SWSI. Remove dates where there are no SWSI values. 
Dillon_Decomp_CO_SWSI_Raw = na.trim(Dillon_Decomp_CO_SWSI_Raw)

CombinedData <- Dillon_Decomp_CO_SWSI_Raw %>% arrange(Date)



###linear trend### 
#Ask auto-arima best fit. 
auto.arima(CombinedData$Discharge)

#ARIMA(0,1,3)
#first number is autoregressive coef 0
# middle is differencing data 1
#last number is moving average term 3

#Run AR3 with scaled data 
CombinedData$yr = lubridate::year(CombinedData$Date)# extract just the year
CombinedData$scaled_yr = scale(CombinedData$yr, center = TRUE, scale = FALSE) #scale year to a 0 mean to incorporate as random effect

#Scaled AR3 model
mod_ARMAp0q3 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=0,q=3), data = CombinedData) #run model

# extract and assess residuals: ARMAp0q3
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp0q3, type = "normalized"), main="Discharge adjusted, Raw SWSI ARMAp0q3 model residuals")
plot(resid(mod_ARMAp0q3, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI ARMAp0q3 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp0q3, type = "normalized"), main="Discharge adjusted, Raw SWSI p1q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp0q3, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp0q3, type = "normalized"))

#several lags have autocorrelation. Run some other models to test fit.I misspelled ARMA so these models have funky names. 
mod_Ar1 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corAR1(), data = CombinedData) #run mode
mod_AMRAp1q1 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=1,q=1), data = CombinedData) #run mode
mod_AMRAp2 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=2,q=0), data = CombinedData) 
mod_AMRAp3 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=3,q=0), data = CombinedData) 
mod_AMRAp0q2 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=0,q=2), data = CombinedData) 
mod_AMRAp1q2 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=1,q=2), data = CombinedData) 
mod_AMRAp2q2 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=2,q=2), data = CombinedData) 

#Compare models with BIC 
bbmle::AICtab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp1q2) 
#             dAIC df
# mod_AMRAp2   0.0  6 
# mod_AMRAp3   0.8  7 
# mod_AMRAp1q2 3.2  7 
# mod_AMRAp0q2 4.0  6 
# mod_AMRAp1q1 4.1  6 
# mod_Ar1      7.2  5 

# extract and assess residuals: mod_AMRAp2
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp2, type = "normalized"), main="Discharge adjusted, Raw SWSI ARMAp2 model residuals")
plot(resid(mod_AMRAp2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI ARMAp2 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp2, type = "normalized"), main="Discharge adjusted, Raw SWSI p1q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp2, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp2, type = "normalized"))

#Still significant autocorrelation

# extract and assess residuals: mod_AMRAp3. I misspelled ARMA so these models have funky names. 
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp3, type = "normalized"), main="Discharge adjusted, Raw SWSI ARMAp3 model residuals")
plot(resid(mod_AMRAp3, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI ARMAp3 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp3, type = "normalized"), main="Discharge adjusted, Raw SWSI p1q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp3, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp3, type = "normalized"))

#Less autocorrelation! 

# extract and assess residuals: mod_AMRAp1q2
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp1q2, type = "normalized"), main="Discharge adjusted, Raw SWSI ARMAp1q2 model residuals")
plot(resid(mod_AMRAp1q2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI ARMAp1q2 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp1q2, type = "normalized"), main="Discharge adjusted, Raw SWSI p1q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp1q2, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp1q2, type = "normalized"))

#More autocorrelation. Going to use ARMAp3 because has less significant aurocorrelation. Autocorrelation at lag 4,8,20. All under -0.15. 

Dillon_CO_ARMAp3 <- mod_AMRAp3

summary(Dillon_CO_ARMAp3)


#Plot result 
visreg(Dillon_CO_ARMAp3, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Outflow Discharge") +
  ggtitle("Dillon Outflows by Colorado SWSI")


# saving the plot as png 
ggsave("Dillon_CO_ARMAp3result.png", path = "results/graphs/")

# bootstrap confidence intervals
Dillon_CO_ARMAp3_boot<-lmeresampler::bootstrap(model = Dillon_CO_ARMAp3, .f = fixef, type = "reb", B = 1000, reb_type = 2)
Dillon_CO_ARMAp3_CIs = confint(Dillon_CO_ARMAp3_boot, type="norm",level = 0.95)
plot(Dillon_CO_ARMAp3_boot, "beta.SWSI_values")
Dillon_CO_ARMAp3_CIplot = plot(Dillon_CO_ARMAp3_boot, "beta.SWSI_values")



####Dillon - SP SWSI linear model w seasonal correction on Dillon data p = 0.0493 ####
Dillon_Decomp_SP_SWSI_Raw <- full_join(Dillon_Decomp_filled,SWSI_Platte, by = "Date") %>% arrange(Date) #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI

#POR for discharge data is different than for SWSI. Remove dates where there are no SWSI values. 
Dillon_Decomp_SP_SWSI_Raw = na.trim(Dillon_Decomp_SP_SWSI_Raw) 

CombinedData <- Dillon_Decomp_SP_SWSI_Raw %>% arrange(Date)

###linear trend### 
#using ARMAp3 because that's what I used with SP.

#Run ARMAp3 with scaled data 
CombinedData$yr = lubridate::year(CombinedData$Date)# extract just the year
CombinedData$scaled_yr = scale(CombinedData$yr, center = TRUE, scale = FALSE) #scale year to a 0 mean to incorporate as random effect


mod_AMRAp3 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=3,q=0), data = CombinedData) 
# extract and assess residuals: mod_AMRAp3. I misspelled ARMA so these models have funky names. 
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp3, type = "normalized"), main="Discharge adjusted, Raw SWSI ARMAp3 model residuals")
plot(resid(mod_AMRAp3, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI ARMAp3 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp3, type = "normalized"), main="Discharge adjusted, Raw SWSI p1q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp3, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp3, type = "normalized"))

#negative autocorrelation at lag 2,8,20. All under -0.15 

Dillon_SP_ARMAp3 <- mod_AMRAp3

summary(Dillon_SP_ARMAp3)

#Plot result 
visreg(Dillon_SP_ARMAp3, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Outflow Discharge") +
  ggtitle("Dillon Outflows by S Platte SWSI")


# saving the plot as png 
ggsave("Dillon_SP_ARMAp3result.png", path = "results/graphs/")

# bootstrap confidence intervals
Dillon_SP_ARMAp3_boot<-lmeresampler::bootstrap(model = Dillon_SP_ARMAp3, .f = fixef, type = "reb", B = 1000, reb_type = 2)
Dillon_SP_ARMAp3_CIs = confint(Dillon_SP_ARMAp3_boot, type="norm",level = 0.95)
plot(Dillon_SP_ARMAp3_boot, "beta.SWSI_values")
Dillon_SP_ARMAp3_CIplot = plot(Dillon_SP_ARMAp3_boot, "beta.SWSI_values")


#Dillon - Plot both bootstrap plots for both river basins against eachother. Colorado on bottom. 
gridExtra::grid.arrange(Dillon_SP_ARMAp3_CIplot, Dillon_CO_ARMAp3_CIplot, ncol=1)

#save plots
ggsave("Dillon_ARMAp3_boot.png", path = "results/graphs/")
