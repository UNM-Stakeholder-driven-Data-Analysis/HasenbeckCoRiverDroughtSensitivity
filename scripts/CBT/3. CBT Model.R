#### read me ####

# This script preps data for modeling and then models CBT data via linear regression

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
library(beepr)
#library(gridExtra)
#library(MARSS)



#### load data and format date/time ####

#SWSI#
SWSI = read.csv("data/processed/SWSI1981to2023.csv", header = T)
SWSI$Date = as.Date.character(SWSI$Date, format = "%Y-%m-%d")

#Pulling out Colorado SWSI. Will use this to interpolate diversions/outflows
SWSI_CO <- SWSI %>%
  dplyr::select(Date, Colorado) %>%
  rename("SWSI_values" = "Colorado") %>% 
  group_by(Date) %>%
  summarize(SWSI_values=mean(SWSI_values)) %>% arrange(Date) #Some dates have two entries. Avg the duplicates here. 

##Horsetooth Reservoir ## 
HorsetoothReleases <- read_csv(file = "data/processed/HorsetoothLakesMonthlyReleases") %>%
  rename("Discharge" = "Release") %>% arrange(Date) #using the same column name as in diversion data to simplify replication
HorsetoothReleases$Discharge = as.numeric(HorsetoothReleases$Discharge)
HorsetoothReleases$Date = as.Date(HorsetoothReleases$Date)
hist(HorsetoothReleases$Discharge, breaks = 100)

#AdamsDiversions# 
AdamsDiversions <- read_csv(file = "data/processed/AdamsMonthlyDischarge") %>% arrange(Date)


####Horesetooth Interpolation#####
sum(is.na(HorsetoothReleases$Discharge))
#There are NAs when I combined data with SWSI. 
#Going to combine SWSI just to create a date column to introduce NAs where diversion data is missing.
#Then remove SWSI and interpolate missing values. 
HorsetoothInterpolation <- full_join(HorsetoothReleases,SWSI_CO, by = "Date") %>%#Combining SWSI by basin with diversion data
  select(Date,Discharge) %>% arrange(Date)

plot(read.zoo(HorsetoothInterpolation, index.column=1, format="%Y-%m-%d")) #plot as time series
#Data starts in 1991-01-01. Missing data from 2012-06-01,2015-02-01. Eliminating 2012 on to have continous dataset
#Also has an outlier in the late 1990s, but it looks like there are multiple months of very high discharge
#So I'm not going to get rid of the outlier 

HorsetoothInterpolation <- full_join(HorsetoothReleases,SWSI_CO, by = "Date") %>%#Combining SWSI by basin with diversion data
  filter(Date <= "2012-05-01", Date >= "1991-01-01") %>% 
  select(Date,Discharge) %>% arrange(Date)


sum(is.na(HorsetoothInterpolation$Discharge))
#13 nas 

plot(read.zoo(HorsetoothInterpolation, index.column=1, format="%Y-%m-%d")) #plot as time series
## fill gaps with linear interpolation ##
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling

# Make univariate zoo time series #
ts.temp<-read.zoo(HorsetoothInterpolation, index.column=1, format="%Y-%m-%d")

# Apply NA interpolation method: Using max gap of 12 months 
Horsetooth_filled = na.approx(ts.temp, na.rm = T, maxgap = 12)

plot(Horsetooth_filled)
plot(ts.temp)

par(mfrow=c(1,1)) # reset plotting window
# revert back to dfs
Horsetooth_filled = as.data.frame(Horsetooth_filled)
Horsetooth_filled$Date <- as.Date(rownames(Horsetooth_filled))
names(Horsetooth_filled) = c(colnames(HorsetoothInterpolation)[2],colnames(HorsetoothInterpolation)[1])

Horsetooth_filled %>% dplyr::select(Discharge, Date)

sum(is.na(Horsetooth_filled$Discharge))
#No more NAs.

Horsetooth_filled$Date = (as.Date(Horsetooth_filled$Date))

## Adams Interpolation #### 
#There are no NAs but when I combined data with SWSI, I found data gaps. 
#Going to combine SWSI just to create a date column to introduce NAs where diversion data is missing.
#Then remove SWSI and interpolate missing values. 
#Data start: 	
#1993-11-01
#Data end: 	
#2022-10-01
AdamsClean <- full_join(AdamsDiversions,SWSI_CO, by = "Date") %>%#Combining SWSI by basin with diversion data
  select(Date,Discharge) %>% 
  filter(Date >= "1993-11-01", Date <= "2022-10-01") %>% 
  arrange(Date)

sum(is.na(AdamsClean))
#There are 3 nas. 

## fill gaps with linear interpolation ##
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling

# Make univariate zoo time series #
ts.temp <- read.zoo(AdamsClean, index.column=1, format="%Y-%m-%d")

# ‘order.by’ are not unique warning suggests duplicate time stamps. I found that this is due to time zone changes, so nothing to worry about for regular time steps. 
plot(ts.temp)

# Apply NA interpolation method: Using max gap of 12 mon 
Adams_filled = na.approx(ts.temp, na.rm = T, maxgap = 12)
plot(Adams_filled)

par(mfrow=c(1,1))
hist(AdamsDiversions$Discharge, breaks = 100)


par(mfrow=c(1,1)) # reset plotting window
# revert back to df
Adams_filled = as.data.frame(Adams_filled)
Adams_filled$Date = AdamsClean$Date
names(Adams_filled) = c(colnames(AdamsClean)[2],colnames(AdamsClean)[1])
Adams_filled = Adams_filled %>% dplyr::select(Discharge, Date) %>% arrange(Date)

sum(is.na(Adams_filled$Discharge))
#No more NAs.




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

#### Adams - Create time series and remove seasonality ####

## prep time series ##
sum(is.na(AdamsDiversions$Date))
#No NAs

sum(is.na(AdamsDiversions$Discharge))
#0 NAs

# check percentage of dataset with NAs 
sum(is.na(AdamsDiversions))/nrow(AdamsDiversions)*100
#0%


#prepping to remove seasonality: 

#set df:
Discharge_data <- Adams_filled

#create timeseries. IMPORTANT!!!!!!!!!! RESET START DATE TO DATA START DATE!!!! 
#Adams start date: 1993-11-01. Rembember I removed older data.
timeseries = ts(Discharge_data$Discharge, start = c(1993, 11), frequency = 12)
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

sum(is.na(Discharge_data_DEs))
#No NAs 

AdamsDecomp <- Discharge_data_DEs

#### Horsetooth - Create time series and remove seasonality ####
## prep time series ##

##data explore## 
hist(Horsetooth_filled$Discharge, breaks = 100)

sum(is.na(Horsetooth_filled$Date))
#No NAs

sum(is.na(Horsetooth_filled$Discharge))
#0 NAs

# check percentage of dataset with NAs 
sum(is.na(Horsetooth_filled))/nrow(Horsetooth_filled)*100
#0%


# need to do this to prep for removing seasonality
#set df:
Discharge_data <- Horsetooth_filled

#Check time period!!!!!!
# Starts 1991-01-01
timeseries = ts(Discharge_data$Discharge, start = c(1991, 01), frequency = 12)
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

#decomposition introduced 9 NAs. Replace them with via spine interpolation. 
sum(is.na(Discharge_data_DEs))

HorsetoothDecomp <- Discharge_data_DEs


#### Adams - CO SWSI linear model w seasonal correction on Adams data - ARIMA model 0.3442   ####

Adams_Decomp_CO_SWSI_Raw <- full_join(AdamsDecomp,SWSI_CO, by = "Date")  #Combining SWSI by basin with diversion data

#POR for SWSI is different than Adams. Remove dates where there are no Discharge values. 
Adams_Decomp_CO_SWSI_Raw <- na.trim(Adams_Decomp_CO_SWSI_Raw) %>% arrange(Date)
  
sum(is.na(Adams_Decomp_CO_SWSI_Raw))
#0 NAs
CombinedData <- Adams_Decomp_CO_SWSI_Raw

### linear trends ###

# add simple time steps to df
CombinedData$t = c(1:nrow(CombinedData))

#Ask auto-arima best fit. 
auto.arima(CombinedData$Discharge)
#ARIMA(1,0,0)
#first number is autoregressive coef 1
# middle is differencing data 0
#last number is moving average term 0

#Run AR1 with scaled data
CombinedData$yr = lubridate::year(CombinedData$Date)# extract just the year
CombinedData$scaled_yr = scale(CombinedData$yr, center = TRUE, scale = FALSE) #scale year to a 0 mean to incorporate as random effect
mod_AR1 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corAR1(), data = CombinedData) #run mode

# extract and assess residuals: AR1. 
par(mfrow=c(1,3))
Acf(resid(mod_AR1, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAR1 model residuals")
plot(resid(mod_AR1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSAR1 model residuals"); abline(h=0)
qqnorm(resid(mod_AR1, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAR1 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AR1, type = "normalized"))$statistic,2))); qqline(resid(mod_AR1, type = "normalized"))
#Very little autocorrelation at lag 12, 18, 24. 

#several lags have autocorrelation. Run some other models to test fit.
mod_ARMAp1q1 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=1,q=1), data = CombinedData) #run mode
mod_ARMAp2 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=2,q=0), data = CombinedData)
mod_ARMAp3 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=3,q=0), data = CombinedData)
mod_ARMAp0q2 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=0,q=2), data = CombinedData)
mod_ARMAp1q2 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=1,q=2), data = CombinedData)


#Compare AIC scores 
bbmle::AICtab(mod_AR1,mod_ARMAp1q1,mod_ARMAp2,mod_ARMAp3,mod_ARMAp0q2)
            # dAIC df
# mod_AR1      0.0  5 
# mod_ARMAp2   1.9  6 
# mod_ARMAp1q1 1.9  6 
# mod_ARMAp0q2 2.2  6 
# mod_ARMAp3   3.9  7 

# extract and assess residuals: ARMAp2. 
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSARMAp2 model residuals")
plot(resid(mod_ARMAp2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSARMAp2 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSARMAp2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp2, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp2, type = "normalized"))
#worse. 

# extract and assess residuals: ARMAp1q1. 
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp1q1, type = "normalized"), main="Discharge adjusted, Raw SWSI ARMAp1q1 model residuals")
plot(resid(mod_ARMAp1q1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI ARMAp1q1 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp1q1, type = "normalized"), main="Discharge adjusted, Raw SWSI p1q1 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp1q1, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp1q1, type = "normalized"))
#worse. Going to use ar1 

Adams_CO_AR1<- mod_AR1

cor(CombinedData$Discharge, fitted(Adams_CO_AR1), method = "kendall")



#Plot result 
Adams_CO_AR1_chart <- visreg(Adams_CO_AR1, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Discharge") +
  ggtitle("Adams Diversions by Colorado SWSI")




# bootstrap confidence intervals
Adams_CO_AR1_boot<-lmeresampler::bootstrap(model = Adams_CO_AR1, .f = fixef, type = "reb", B = 1000, reb_type = 2)
Adams_CO_AR1_CIs = confint(Adams_CO_AR1_boot, type="norm",level = 0.95)
Adams_CO_AR1_CIplot = plot(Adams_CO_AR1_boot, "beta.SWSI_values")
Adams_CO_AR1_CIplot_custom = Adams_CO_AR1_CIplot + ggtitle("Adams vs Colorado SWSI") + xlab("beta SWSI values")
plot(Adams_CO_AR1_CIplot_custom)




#### Adams - S Platte SWSI linear model w seasonal correction on Adams data - ARIMA model p = 0.0078  ####

Adams_Decomp_SP_SWSI_Raw <- full_join(AdamsDecomp,SWSI_Platte, by = "Date")  #Combining SWSI by basin with diversion data

#POR for SWSI is different than Adams. Remove dates where there are no Discharge values. 
Adams_Decomp_SP_SWSI_Raw <- na.trim(Adams_Decomp_SP_SWSI_Raw)

sum(is.na(Adams_Decomp_SP_SWSI_Raw))
#NO NAs

CombinedData <- Adams_Decomp_SP_SWSI_Raw %>% arrange(Date)

### linear trends ###
# add simple time steps to df
CombinedData$t = c(1:nrow(CombinedData))

#Run AR1 with scaled data
CombinedData$yr = lubridate::year(CombinedData$Date)# extract just the year
CombinedData$scaled_yr = scale(CombinedData$yr, center = TRUE, scale = FALSE) #scale year to a 0 mean to incorporate as random effect
mod_AR1 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corAR1(), data = CombinedData) #run mode

# extract and assess residuals: AR1. 
par(mfrow=c(1,3))
Acf(resid(mod_AR1, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAR1 model residuals")
plot(resid(mod_AR1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSAR1 model residuals"); abline(h=0)
qqnorm(resid(mod_AR1, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAR1 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AR1, type = "normalized"))$statistic,2))); qqline(resid(mod_AR1, type = "normalized"))
#autocorrelation at lag 12, 18, 24, all under 0.13

Adams_SP_AR1<- mod_AR1

cor(CombinedData$Discharge, fitted(Adams_SP_AR1), method = "kendall")

#Plot result 
Adams_SP_AR1_chart <- visreg(Adams_SP_AR1, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Discharge") +
  ggtitle("Adams Diversions by South Platte SWSI")

### Plot both basins against eachother. Colorado on bottom ###
Adams_AR1_Result <- gridExtra::grid.arrange(Adams_SP_AR1_chart, Adams_CO_AR1_chart, ncol=1)

# saving the plot as png 
ggsave("Adams_AR1_Result.png", plot = Adams_AR1_Result, path = "results/graphs/")


# bootstrap confidence intervals
Adams_SP_AR1_boot<-lmeresampler::bootstrap(model = Adams_SP_AR1, .f = fixef, type = "reb", B = 1000, reb_type = 2)
Adams_SP_AR1_CIs = confint(Adams_SP_AR1_boot, type="norm",level = 0.95)
Adams_SP_AR1_CIplot = plot(Adams_SP_AR1_boot, "beta.SWSI_values")
Adams_SP_AR1_CIplot_custom = Adams_SP_AR1_CIplot + ggtitle("Adams vs S. Plate SWSI") + xlab("beta SWSI values")
plot(Adams_SP_AR1_CIplot_custom)

#Adams - Plot both bootstrap plots for both river basins against eachother. Colorado on bottom. 
Adams_both <- gridExtra::grid.arrange(Adams_SP_AR1_CIplot_custom, Adams_CO_AR1_CIplot_custom, ncol=1)

# saving the plot as png 
ggsave("AdamsAR1result_boot.png", plot = Adams_both, path = "results/graphs/")


####Horsetooth - CO SWSI linear model w seasonal correction on HT data p = 0.6475 ####
Horsetooth_Decomp_CO_SWSI_Raw <- full_join(HorsetoothDecomp,SWSI_CO, by = "Date")  #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI

#POR for discharge data is different than for SWSI. 
Horsetooth_Decomp_CO_SWSI_Raw = na.trim(Horsetooth_Decomp_CO_SWSI_Raw)

sum(is.na(Horsetooth_Decomp_CO_SWSI_Raw))
#No NAs. 

CombinedData <- Horsetooth_Decomp_CO_SWSI_Raw %>% arrange(Date)

### linear trends ###
# add simple time steps to df
CombinedData$t = c(1:nrow(CombinedData))

auto.arima(CombinedData$Discharge)
#0,0,2 

#Run ARMA Q2  with scaled data
CombinedData$yr = lubridate::year(CombinedData$Date)# extract just the year
CombinedData$scaled_yr = scale(CombinedData$yr, center = TRUE, scale = FALSE) #scale year to a 0 mean to incorporate as random effect

#ARMAq2
mod_ARMAp0q2 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=0,q=2), data = CombinedData) 

#Extract residuals 
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp0q2, type = "normalized"), main="Discharge adjusted, Raw SWSI ARMAp0q2 model residuals")
plot(resid(mod_ARMAp0q2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI ARMAp0q2 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp0q2, type = "normalized"), main="Discharge adjusted, Raw SWSI p0q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp0q2, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp0q2, type = "normalized"))
#Pretty significant autocorrelation. 

#several lags have autocorrelation. Run some other models to test fit.
mod_ARMAp1q1 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=1,q=1), data = CombinedData) #run mode
mod_ARMAp2 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=2,q=0), data = CombinedData)
mod_ARMAp3 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=3,q=0), data = CombinedData)
mod_ARMAp0q2 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=0,q=2), data = CombinedData)
mod_ARMAp1q2 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=1,q=2), data = CombinedData)


#Compare AIC scores 
bbmle::AICtab(mod_AR1,mod_ARMAp1q1,mod_ARMAp2,mod_ARMAp3,mod_ARMAp0q2)
# dAIC df
# mod_ARMAp2    0.0 6 
# mod_ARMAp0q2  0.1 6 
# mod_ARMAp1q1  0.4 6 
# mod_ARMAp3    1.4 7 
# mod_AR1      11.1 5 

# extract and assess residuals: ARMAp2. 
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSARMAp2 model residuals")
plot(resid(mod_ARMAp2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSARMAp2 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSARMAp2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp2, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp2, type = "normalized"))
#Lag 2, 3, Both under 0.20

# extract and assess residuals: ARMAp1q1. 
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp1q1, type = "normalized"), main="Discharge adjusted, Raw SWSI ARMAp1q1 model residuals")
plot(resid(mod_ARMAp1q1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI ARMAp1q1 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp1q1, type = "normalized"), main="Discharge adjusted, Raw SWSI p1q1 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp1q1, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp1q1, type = "normalized"))
#Worse 

# extract and assess residuals: mod_ARMAp3. 
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp3, type = "normalized"), main="Discharge adjusted, Raw SWSI ARMAp3 model residuals")
plot(resid(mod_ARMAp3, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI ARMAp3 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp3, type = "normalized"), main="Discharge adjusted, Raw SWSI p1q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp3, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp3, type = "normalized"))
#even worse. Going to use p2

Horsetooth_CO_ARMAp2<- mod_ARMAp2

cor(CombinedData$Discharge, fitted(Horsetooth_CO_ARMAp2), method = "kendall")


#Plot result 
Horsetooth_CO_ARMAp2chart <- visreg(Horsetooth_CO_ARMAp2, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Discharge") +
  ggtitle("Horsetooth Diversions by Colorado SWSI")



# bootstrap confidence intervals
Horsetooth_CO_ARMAp2_boot<-lmeresampler::bootstrap(model = Horsetooth_CO_ARMAp2, .f = fixef, type = "reb", B = 1000, reb_type = 2)
Horsetooth_CO_ARMAp2_CIs = confint(Horsetooth_CO_ARMAp2_boot, type="norm",level = 0.95)
Horsetooth_CO_ARMAp2_CIplot = plot(Horsetooth_CO_ARMAp2_boot, "beta.SWSI_values")
Horsetooth_CO_ARMAp2_CIplot_custom = Horsetooth_CO_ARMAp2_CIplot + ggtitle("Horsetooth vs Colorado SWSI") + xlab("beta SWSI values")
plot(Horsetooth_CO_ARMAp2_CIplot_custom)

####Horsetooth - SP SWSI linear model w seasonal correction on HT data p = 0.6475 ####
Horsetooth_Decomp_SP_SWSI_Raw <- full_join(HorsetoothDecomp,SWSI_Platte, by = "Date")  #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI

#POR for discharge data is different than for SWSI. 
Horsetooth_Decomp_SP_SWSI_Raw = na.trim(Horsetooth_Decomp_SP_SWSI_Raw)

sum(is.na(Horsetooth_Decomp_SP_SWSI_Raw))
#No NAs. 

CombinedData <- Horsetooth_Decomp_SP_SWSI_Raw %>% arrange(Date)

### linear trends ###
# add simple time steps to df
CombinedData$t = c(1:nrow(CombinedData))


#Run ARMA P2  with scaled data
CombinedData$yr = lubridate::year(CombinedData$Date)# extract just the year
CombinedData$scaled_yr = scale(CombinedData$yr, center = TRUE, scale = FALSE) #scale year to a 0 mean to incorporate as random effect

# extract and assess residuals: ARMAp2. 
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSARMAp2 model residuals")
plot(resid(mod_ARMAp2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSARMAp2 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSARMAp2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp2, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp2, type = "normalized"))
#Lag 2, 3, Both under 0.20


Horsetooth_SP_ARMAp2<- mod_ARMAp2

cor(CombinedData$Discharge, fitted(Horsetooth_SP_ARMAp2), method = "kendall")


#Plot result 
Horsetooth_SP_ARMAp2_chart <- visreg(Horsetooth_SP_ARMAp2, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Discharge") +
  ggtitle("Horsetooth Diversions by South Platte SWSI")

### Plot both basins against eachother. Colorado on bottom ###
Horsetooth_ARMAp2_chart <- gridExtra::grid.arrange(Horsetooth_SP_ARMAp2_chart, Horsetooth_CO_ARMAp2chart, ncol=1)


# saving the plot as png 
ggsave("Horsetooth_ARMAp2_result.png", plot = Horsetooth_ARMAp2_chart, path = "results/graphs/")

# bootstrap confidence intervals
Horsetooth_SP_ARMAp2_boot<-lmeresampler::bootstrap(model = Horsetooth_SP_ARMAp2, .f = fixef, type = "reb", B = 1000, reb_type = 2)
Horsetooth_SP_ARMAp2_CIs = confint(Horsetooth_SP_ARMAp2_boot, type="norm",level = 0.95)
Horsetooth_SP_ARMAp2_CIplot = plot(Horsetooth_SP_ARMAp2_boot, "beta.SWSI_values")
Horsetooth_SP_ARMAp2_CIplot_custom = Horsetooth_SP_ARMAp2_CIplot + ggtitle("Horsetooth vs S. Plate SWSI") + xlab("beta SWSI values")
plot(Horsetooth_SP_ARMAp2_CIplot_custom)

# Plot both bootstrap plots for both river basins against eachother. Colorado on bottom. 
HorsetoothBoth <- gridExtra::grid.arrange(Horsetooth_SP_ARMAp2_CIplot_custom, Horsetooth_CO_ARMAp2_CIplot_custom, ncol=1)

# saving the plot as png 
ggsave("HorsetoothARMAp2result_boot.png", plot = HorsetoothBoth, path = "results/graphs/")

beep(sound = 5)