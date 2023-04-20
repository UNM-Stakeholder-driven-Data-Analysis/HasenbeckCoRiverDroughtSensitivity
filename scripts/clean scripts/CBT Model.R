#### read me ####

# This script models CBT data via linear regression

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



#### load data and format date/time ####

#SWSI#
SWSI = read.csv("data/processed/SWSI1981to2023.csv", header = T)
SWSI$Date = as.Date.character(SWSI$Date, format = "%Y-%m-%d")

#Pulling out Colorado SWSI. Will use this to interpolate diversions/outflows
SWSI_CO <- SWSI %>%
  dplyr::select(Date, Colorado) %>%
  rename("SWSI_values" = "Colorado") %>% 
  group_by(Date) %>%
  summarize(SWSI_values=mean(SWSI_values)) #Some dates have two entries. Avg the duplicates here. 

##Horsetooth Reservoir ## 
HorsetoothReleases <- read_csv(file = "data/processed/HorsetoothLakesMonthlyReleases") %>%
  rename("Discharge" = "Release") #using the same column name as in diversion data to simplify replication

sum(is.na(HorsetoothInterpolation$Discharge))
#There are NAs when I combined data with SWSI. 
#Going to combine SWSI just to create a date column to introduce NAs where diversion data is missing.
#Then remove SWSI and interpolate missing values. 
HorsetoothInterpolation <- full_join(HorsetoothReleases,SWSI_CO, by = "Date") %>%#Combining SWSI by basin with diversion data
  select(Date,Discharge)

plot(read.zoo(HorsetoothInterpolation, index.column=1, format="%Y-%m-%d")) #plot as time series

#Data starts in 1991-01-01. Missing data from 2012-06-01,2015-02-01. Eliminating 2012 on to have continous dataset
HorsetoothInterpolation <- HorsetoothInterpolation %>%
  filter(Date >= "1991-01-01", Date <= "2012-06-01")


## fill gaps with spline interpolation ##
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling

# Make univariate zoo time series #
ts.temp<-read.zoo(HorsetoothInterpolation, index.column=1, format="%Y-%m-%d")

# Apply NA interpolation method: Using max gap of 7 days 
Horsetooth_filled = na.spline(ts.temp, na.rm = T, maxgap = 7)

plot(Horsetooth_filled)
plot(ts.temp)

par(mfrow=c(1,1)) # reset plotting window
# revert back to dfs
Horsetooth_filled = as.data.frame(Horsetooth_filled)
Horsetooth_filled$Date = HorsetoothInterpolation$Date
Horsetooth_filled$Date = HorsetoothInterpolation$Date
names(Horsetooth_filled) = c(colnames(HorsetoothInterpolation)[2],colnames(HorsetoothInterpolation)[1])

Horsetooth_filled %>% dplyr::select(Discharge, Date)

sum(is.na(Horsetooth_filled$Discharge))
#No more NAs.

Horsetooth_filled$Date = (as.Date(Horsetooth_filled$Date))
#some interpolated values went negative. Replace negative values with 0.
Horsetooth_filled$Discharge[(Horsetooth_filled$Discharge < 0)] = 0 

# check NAs that are left
sum(is.na(Horsetooth_filled$Discharge))
#No more NAs.








## Adams ## 

AdamsDiversions <- read_csv(file = "data/processed/AdamsMonthlyDischarge") 

#There are no NAs but when I combined data with SWSI, I found data gaps. 
#Going to combine SWSI just to create a date column to introduce NAs where diversion data is missing.
#Then remove SWSI and interpolate missing values. 
#Data start: 	
#1993-11-01
#Data end: 	
#2022-10-01
AdamsClean <- full_join(AdamsDiversions,SWSI_CO, by = "Date") %>%#Combining SWSI by basin with diversion data
  select(Date,Discharge) %>% 
  filter(Date >= "1993-11-01", Date <= "2022-10-01")


sum(is.na(AdamsClean))
#There are 3 nas. 

## fill gaps with spline interpolation ##
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling

# Make univariate zoo time series #
ts.temp <- read.zoo(AdamsClean, index.column=1, format="%Y-%m-%d")

# ‘order.by’ are not unique warning suggests duplicate time stamps. I found that this is due to time zone changes, so nothing to worry about for regular time steps. 
plot(ts.temp)

# Apply NA interpolation method: Using max gap of 7 days 
Adams_filled = na.spline(ts.temp, na.rm = T, maxgap = 7)
plot(Adams_filled)

par(mfrow=c(1,1))
hist(AdamsDiversions$Discharge, breaks = 100)


par(mfrow=c(1,1)) # reset plotting window
# revert back to df
Adams_filled = as.data.frame(Adams_filled)
Adams_filled$Date = AdamsClean$Date
names(Adams_filled) = c(colnames(AdamsClean)[2],colnames(AdamsClean)[1])
Adams_filled = Adams_filled %>% dplyr::select(Discharge, Date)

sum(is.na(Adams_filled$Discharge))
#No more NAs.



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
timeseries = ts(Discharge_data$Discharge, start = c(1993-11-01), frequency = 12)
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


#decomposition introduced NAs. Replace them with via spine interpolation. 
sum(is.na(Discharge_data_DEs))
## fill gaps with spline interpolation ##
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling
# Make univariate zoo time series #
ts.temp <- read.zoo(Discharge_data_DEs, index.column=1, format="%Y-%m-%d")
plot(ts.temp)
# Apply NA interpolation method: Using max gap of 7 days 
Adams_Decomp_filled = na.spline(ts.temp, na.rm = T, maxgap = 7)
plot(Adams_Decomp_filled)
par(mfrow=c(1,1)) # reset plotting window
# revert back to df
Adams_Decomp_filled = as.data.frame(Adams_Decomp_filled)
Adams_Decomp_filled$Date = as.Date(rownames(Adams_Decomp_filled)) 
names(Adams_Decomp_filled) = c(colnames(AdamsDiversions)[2],colnames(AdamsDiversions)[1])
Adams_Decomp_filled = Adams_Decomp_filled %>% dplyr::select(Discharge, Date)


#In case some interpolated values went negative, replace negative values with 0.
Adams_Decomp_filled$Discharge[Adams_Decomp_filled$Discharge < 0] = 0 


AdamsDecomp <- Adams_Decomp_filled

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

timeseries = ts(Discharge_data$Discharge, start = c(1991-01-01), frequency = 12)
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
## fill gaps with spline interpolation ##
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling
# Make univariate zoo time series #
ts.temp <- read.zoo(Discharge_data_DEs, index.column=1, format="%Y-%m-%d")
plot(ts.temp)
# Apply NA interpolation method: Using max gap of 7 days 
Horsetooth_Decomp_filled = na.spline(ts.temp, na.rm = T, maxgap = 7)
plot(Horsetooth_Decomp_filled)
par(mfrow=c(1,1)) # reset plotting window
# revert back to df
Horsetooth_Decomp_filled = as.data.frame(Horsetooth_Decomp_filled)
Horsetooth_Decomp_filled$Date = as.Date(rownames(Horsetooth_Decomp_filled)) 
names(Horsetooth_Decomp_filled) = c(colnames(HorsetoothReleases)[1],colnames(HorsetoothReleases)[2])
Horsetooth_Decomp_filled = Horsetooth_Decomp_filled %>% dplyr::select(Discharge, Date)


#In case some interpolated values went negative, replace negative values with 0.
Horsetooth_Decomp_filled$Discharge[Horsetooth_Decomp_filled$Discharge < 0] = 0 


ggplot(Horsetooth_Decomp_filled, aes(x=Date, y=Discharge))+
  geom_path() + geom_point() + theme_bw()

HorsetoothDecomp <- Horsetooth_Decomp_filled

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

#### Adams - CO SWSI linear model w seasonal correction on Adams data - ARIMA model 0.3442   ####

Adams_Decomp_CO_SWSI_Raw <- full_join(AdamsDecomp,SWSI_CO, by = "Date")  #Combining SWSI by basin with diversion data

#POR for SWSI is different than Adams. Remove dates where there are no Discharge values. 
Adams_Decomp_CO_SWSI_Raw <- na.trim(Adams_Decomp_CO_SWSI_Raw)
  
#Replace NA values with 0. Assumping this is multiplication error in decomposition. 
Adams_Decomp_CO_SWSI_Raw$Discharge[is.na(Adams_Decomp_CO_SWSI_Raw$Discharge)] = 0 

CombinedData <- Adams_Decomp_CO_SWSI_Raw

### linear trends ###

# add simple time steps to df
CombinedData$t = c(1:nrow(CombinedData))

mod = lm(Discharge ~ SWSI_values, CombinedData)

summary(mod)

visreg(mod,"SWSI_values")

confint(mod, "SWSI_values", level=0.95)


## diagnostics ##
Acf(resid(mod))
forecast::checkresiduals(mod)
#Breusch-Godfrey test for serial correlation of order up to 10
#data:  Residuals
#LM test = 150.13, df = 10, p-value < 2.2e-16

#### test & calculate trends - nlme::gls ###

# see package manual: https://cran.r-project.org/web/packages/nlme/nlme.pdf


# run model 
mod_AMRAp3 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=3), method="ML") #selected structure


# intervals() for nlme is equivelant to confint() for lm
intervals(mod_AMRAp3)


par(mfrow=c(1,1))
visreg(mod_AMRAp3,"SWSI_values")

Acf(resid(mod_AMRAp3))


# extract and assess residuals: AMRAp3. p = 0.08
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp3, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp3 model residuals")
plot(resid(mod_AMRAp3, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSAMRAp3 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp3, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp3 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp3, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp3, type = "normalized"))
par(mfrow=c(1,1))
Acf(resid(mod_AMRAp3))
summary(mod_AMRAp3)
visreg(mod_AMRAp3)

Adams_CO_P3 <- mod_AMRAp3



#Plot result 
visreg(Adams_CO_P3, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Discharge") +
  ggtitle("Adams Diversions by Colorado SWSI")


# saving the plot as png 
ggsave("AdamsCOP3result.png", path = "results/graphs/")



#### Adams - S Platte SWSI linear model w seasonal correction on Adams data - ARIMA model p = 0.0078  ####

Adams_Decomp_SP_SWSI_Raw <- full_join(AdamsDecomp,SWSI_Platte, by = "Date")  #Combining SWSI by basin with diversion data

#POR for SWSI is different than Adams. Remove dates where there are no Discharge values. 
Adams_Decomp_SP_SWSI_Raw <- na.trim(Adams_Decomp_SP_SWSI_Raw)

#Replace NA values with 0. Assumping this is multiplication error in decomposition. 
Adams_Decomp_SP_SWSI_Raw$Discharge[is.na(Adams_Decomp_SP_SWSI_Raw$Discharge)] = 0 

CombinedData <- Adams_Decomp_SP_SWSI_Raw

### linear trends ###

# add simple time steps to df
CombinedData$t = c(1:nrow(CombinedData))

mod = lm(Discharge ~ SWSI_values, CombinedData)

summary(mod)

visreg(mod,"SWSI_values")

confint(mod, "SWSI_values", level=0.95)


## diagnostics ##
Acf(resid(mod))
forecast::checkresiduals(mod)
#Breusch-Godfrey test for serial correlation of order up to 10
#data:  Residuals
#LM test = 150.13, df = 10, p-value < 2.2e-16

#### test & calculate trends - nlme::gls ###

# see package manual: https://cran.r-project.org/web/packages/nlme/nlme.pdf


# run model 
mod_AMRAp3 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=3), method="ML") #selected structure


# intervals() for nlme is equivelant to confint() for lm
intervals(mod_AMRAp3)


par(mfrow=c(1,1))
visreg(mod_AMRAp3,"SWSI_values")

Acf(resid(mod_AMRAp3))


# extract and assess residuals: AMRAp3. p = 0.08
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp3, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp3 model residuals")
plot(resid(mod_AMRAp3, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSAMRAp3 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp3, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp3 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp3, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp3, type = "normalized"))
par(mfrow=c(1,1))
Acf(resid(mod_AMRAp3))
summary(mod_AMRAp3)
visreg(mod_AMRAp3)

Adams_SP_P3 <- mod_AMRAp3



#Plot result 
visreg(Adams_SP_P3, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Discharge") +
  ggtitle("Adams Diversions by S. Platte SWSI")


# saving the plot as png 
ggsave("AdamsPlatteP3result.png", path = "results/graphs/")




####Horsetooth - CO SWSI linear model w seasonal correction on HT data p = 0.6475 ####
Horsetooth_Decomp_CO_SWSI_Raw <- full_join(HorsetoothDecomp,SWSI_CO, by = "Date")  #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI

#Twin lakes data period: 1986-10-01 to 2018-11-01
#POR for discharge data is older than for SWSI. Remove dates where there are no SWSI values. 
Horsetooth_Decomp_CO_SWSI_Raw = na.trim(Horsetooth_Decomp_CO_SWSI_Raw)

CombinedData <- Horsetooth_Decomp_CO_SWSI_Raw


### linear trends ###

# add simple time steps to df
CombinedData$t = c(1:nrow(CombinedData))

mod = lm(Discharge ~ SWSI_values, CombinedData)

summary(mod)

visreg(mod,"SWSI_values")

confint(mod, "SWSI_values", level=0.95)


## diagnostics ##
Acf(resid(mod))
forecast::checkresiduals(mod)
#Breusch-Godfrey test for serial correlation of order up to 10
#data:  Residuals
#LM test = 150.13, df = 10, p-value < 2.2e-16

#### test & calculate trends - nlme::gls ###

# see package manual: https://cran.r-project.org/web/packages/nlme/nlme.pdf

# ask auto.arima what it thinks the autocorrelation structure is
auto.arima(CombinedData$Discharge)
#first number is autoregressive coef 0
# middle is differencing data 0
# last number is moving average term 2

# fit AR(1) regression model with SWSI as a predictor
mod_Ar1 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corAR1(), method="ML")

# extract and assess residuals: Ar1 p = 0.63
par(mfrow=c(1,3))
Acf(resid(mod_Ar1, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS Ar1model residuals")
plot(resid(mod_Ar1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main=" Discharge adjusted, raw SWSI GLS Ar1model residuals"); abline(h=0)
qqnorm(resid(mod_Ar1, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS Ar1model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_Ar1, type = "normalized"))$statistic,2))); qqline(resid(mod_Ar1, type = "normalized"))
summary(mod_Ar1)


Horsetooth_CO_AR1 <- mod_Ar1

#Plot result 
visreg(Horsetooth_CO_AR1, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Outflow Discharge") +
  ggtitle("Horsetooth Outflows by Colorado SWSI")


# saving the plot as png 
ggsave("Horsetooth_CO_AR1result.png", path = "results/graphs/")


####Horsetooth - SP SWSI linear model w seasonal correction on HT data p = 0.0051 ####
Horsetooth_Decomp_SP_SWSI_Raw <- full_join(HorsetoothDecomp,SWSI_Platte, by = "Date")  #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI

#Twin lakes data period: 1986-10-01 to 2018-11-01
#POR for discharge data is older than for SWSI. Remove dates where there are no SWSI values. 
Horsetooth_Decomp_SP_SWSI_Raw = na.trim(Horsetooth_Decomp_SP_SWSI_Raw)

CombinedData <- Horsetooth_Decomp_SP_SWSI_Raw


### linear trends ###

# add simple time steps to df
CombinedData$t = c(1:nrow(CombinedData))

mod = lm(Discharge ~ SWSI_values, CombinedData)

summary(mod)

visreg(mod,"SWSI_values")

confint(mod, "SWSI_values", level=0.95)


## diagnostics ##
Acf(resid(mod))
forecast::checkresiduals(mod)
#Breusch-Godfrey test for serial correlation of order up to 10
#data:  Residuals
#LM test = 150.13, df = 10, p-value < 2.2e-16

#### test & calculate trends - nlme::gls ###

# see package manual: https://cran.r-project.org/web/packages/nlme/nlme.pdf

# ask auto.arima what it thinks the autocorrelation structure is
auto.arima(CombinedData$Discharge)
#first number is autoregressive coef 0
# middle is differencing data 0
# last number is moving average term 2

# fit AR(1) regression model with SWSI as a predictor
mod_Ar1 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corAR1(), method="ML")

# extract and assess residuals: Ar1 p = 0.63
par(mfrow=c(1,3))
Acf(resid(mod_Ar1, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS Ar1model residuals")
plot(resid(mod_Ar1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main=" Discharge adjusted, raw SWSI GLS Ar1model residuals"); abline(h=0)
qqnorm(resid(mod_Ar1, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS Ar1model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_Ar1, type = "normalized"))$statistic,2))); qqline(resid(mod_Ar1, type = "normalized"))
summary(mod_Ar1)


Horsetooth_SP_AR1 <- mod_Ar1

#Plot result 
visreg(Horsetooth_SP_AR1, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Outflow Discharge") +
  ggtitle("Horsetooth Outflows by S. Platte SWSI")


# saving the plot as png 
ggsave("Horsetooth_SP_AR1result.png", path = "results/graphs/")



