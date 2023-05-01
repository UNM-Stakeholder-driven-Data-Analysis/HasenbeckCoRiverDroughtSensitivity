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

##Dillon Reservoir ## 
DillonReleases <- read_csv(file = "data/processed/DillonLakesMonthlyReleases") %>%
  rename("Discharge" = "Release") #using the same column name as in diversion data to simplify replication


#There are NAs when I combined data with SWSI. 
#Going to combine SWSI just to create a date column to introduce NAs where diversion data is missing.
#Then remove SWSI and interpolate missing values. 
DillonInterpolation <- full_join(DillonReleases,SWSI_CO, by = "Date") %>%#Combining SWSI by basin with diversion data
  select(Date,Discharge) %>%
  #Data starts in 1986-09-01, but year long data gap right before 1990. Continous data ends in 2023-03-01 
  filter(Date >= "1989-03-01", Date <= "2023-03-01")

sum(is.na(DillonInterpolation$Discharge))
#0 NAs. 


##Gross Reservoir ## 
GrossReleases <- read_csv(file = "data/processed/GrossLakesMonthlyReleases") %>%
  rename("Discharge" = "Release") #using the same column name as in diversion data to simplify replication


#There are NAs when I combined data with SWSI. 
#Going to combine SWSI just to create a date column to introduce NAs where diversion data is missing.
#Then remove SWSI and interpolate missing values. 
GrossInterpolation <- full_join(GrossReleases,SWSI_CO, by = "Date") %>%#Combining SWSI by basin with diversion data
  select(Date,Discharge) %>%
#Data starts in 1986-09-01, but year long data gap right before 1990. Continous data ends in 2023-03-01 
  filter(Date >= "1990-12-01", Date <= "2023-03-01")

sum(is.na(GrossInterpolation$Discharge))
#1 NAs. And one REALLY big outlier. 

plot(read.zoo(GrossInterpolation, index.column=1, format="%Y-%m-%d")) #plot as time series

#Replace outlier with NA so it will interpolate. 
GrossInterpolation$Discharge = replace(GrossInterpolation$Discharge, GrossInterpolation$Discharge > 110761, NA)

## fill gaps with spline interpolation ##
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling

# Make univariate zoo time series #
ts.temp<-read.zoo(GrossInterpolation, index.column=1, format="%Y-%m-%d")

# Apply NA interpolation method: Using max gap of 12 months 
Gross_filled = na.spline(ts.temp, na.rm = T, maxgap = 12)

plot(Gross_filled)
plot(ts.temp)

par(mfrow=c(1,1)) # reset plotting window
# revert back to dfs
Gross_filled = as.data.frame(Gross_filled)
Gross_filled$Date = GrossInterpolation$Date
Gross_filled$Date = GrossInterpolation$Date
names(Gross_filled) = c(colnames(GrossInterpolation)[2],colnames(GrossInterpolation)[1])

Gross_filled %>% dplyr::select(Discharge, Date)

sum(is.na(Gross_filled$Discharge))
#No more NAs.

Gross_filled$Date = (as.Date(Gross_filled$Date))
#In case some interpolated values went negative, replace negative values with 0.
Gross_filled$Discharge[(Gross_filled$Discharge < 0)] = 0 

## Roberts ## 

RobertsDiversions <- read_csv(file = "data/processed/RobertsMonthlyDischarge") 

#There are no NAs but when I combined data with SWSI, I found data gaps. 
#Going to combine SWSI just to create a date column to introduce NAs where diversion data is missing.
#Then remove SWSI and interpolate missing values. 
#Data start: 	
#1963-10-01
#Data end: 	
#2023-03-01
#SWSI has shorter period of record.1981-06-01 to 2022-08-01
RobertsClean <- full_join(RobertsDiversions,SWSI_CO, by = "Date") %>%#Combining SWSI by basin with diversion data
  select(Date, Discharge) %>% 
  filter(Date >= "1981-06-01", Date <= "2022-08-01")


sum(is.na(RobertsClean))
#There are no nas. 

#### Roberts Create time series and remove seasonality #### 

#prepping to remove seasonality: 

#set df:
Discharge_data <- RobertsDiversions

#create timeseries. IMPORTANT!!!!!!!!!! RESET START DATE TO DATA START DATE!!!! 
#Roberts start date: 1981-06-01. Rembember I removed older data.
timeseries = ts(Discharge_data$Discharge, start = c(1981-06-01), frequency = 12)
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
# Apply NA interpolation method: Using max gap of 12 mon 
Roberts_Decomp_filled = na.spline(ts.temp, na.rm = T, maxgap = 12)
plot(Roberts_Decomp_filled)
par(mfrow=c(1,1)) # reset plotting window
# revert back to df
Roberts_Decomp_filled = as.data.frame(Roberts_Decomp_filled)
Roberts_Decomp_filled$Date = as.Date(rownames(Roberts_Decomp_filled)) 
names(Roberts_Decomp_filled) = c(colnames(RobertsDiversions)[1],colnames(RobertsDiversions)[2])
Roberts_Decomp_filled = Roberts_Decomp_filled %>% dplyr::select(Discharge, Date)


#In case some interpolated values went negative, replace negative values with 0.
Roberts_Decomp_filled$Discharge[Roberts_Decomp_filled$Discharge < 0] = 0 


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
hist(DillonInterpolation$Discharge, breaks = 100)

sum(is.na(DillonInterpolation$Date))
#No NAs

sum(is.na(DillonInterpolation$Discharge))
#0 NAs

# check percentage of dataset with NAs 
sum(is.na(DillonInterpolation))/nrow(DillonInterpolation)*100
#0%


# need to do this to prep for removing seasonality
#set df:
Discharge_data <- DillonInterpolation

#Check time period!!!!!!
# Starts 1989-03-01

timeseries = ts(Discharge_data$Discharge, start = c(1989-03-01), frequency = 12)
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

DillonDecomp <- Discharge_data_DEs

ggplot(DillonDecomp, aes(x=Date, y=Discharge))+
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

#### Roberts - CO SWSI linear model w seasonal correction on Roberts data - ARIMA model p = 0.0174   ####

Roberts_Decomp_CO_SWSI_Raw <- full_join(RobertsDecomp,SWSI_CO, by = "Date")  #Combining SWSI by basin with diversion data

#POR for SWSI is different than Roberts. Remove dates where there are no Discharge values. 
Roberts_Decomp_CO_SWSI_Raw <- na.trim(Roberts_Decomp_CO_SWSI_Raw)

CombinedData <- Roberts_Decomp_CO_SWSI_Raw

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

Roberts_CO_P3 <- mod_AMRAp3



#Plot result 
visreg(Roberts_CO_P3, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Discharge") +
  ggtitle("Roberts Diversions by Colorado SWSI")


# saving the plot as png 
ggsave("RobertsCOP3result.png", path = "results/graphs/")



#### Roberts - S Platte SWSI linear model w seasonal correction on Roberts data - ARIMA model p = 0.0  ####

Roberts_Decomp_SP_SWSI_Raw <- full_join(RobertsDecomp,SWSI_Platte, by = "Date")  #Combining SWSI by basin with diversion data

#POR for SWSI is different than Roberts. Remove dates where there are no Discharge values. 
Roberts_Decomp_SP_SWSI_Raw <- na.trim(Roberts_Decomp_SP_SWSI_Raw)

#Replace NA values with 0. Assumping this is multiplication error in decomposition. 
Roberts_Decomp_SP_SWSI_Raw$Discharge[is.na(Roberts_Decomp_SP_SWSI_Raw$Discharge)] = 0 

CombinedData <- Roberts_Decomp_SP_SWSI_Raw

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

Roberts_SP_P3 <- mod_AMRAp3



#Plot result 
visreg(Roberts_SP_P3, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Discharge") +
  ggtitle("Roberts Diversions by S. Platte SWSI")


# saving the plot as png 
ggsave("RobertsPlatteP3result.png", path = "results/graphs/")




####Gross - CO SWSI linear model w seasonal correction on HT data p = 0.0298 ####
Gross_Decomp_CO_SWSI_Raw <- full_join(GrossDecomp,SWSI_CO, by = "Date")  #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI

#Twin lakes data period: 1986-10-01 to 2018-11-01
#POR for discharge data is older than for SWSI. Remove dates where there are no SWSI values. 
Gross_Decomp_CO_SWSI_Raw = na.trim(Gross_Decomp_CO_SWSI_Raw)

CombinedData <- Gross_Decomp_CO_SWSI_Raw


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


Gross_CO_AR1 <- mod_Ar1

#Plot result 
visreg(Gross_CO_AR1, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Outflow Discharge") +
  ggtitle("Gross Outflows by Colorado SWSI")


# saving the plot as png 
ggsave("Gross_CO_AR1result.png", path = "results/graphs/")


####Gross - SP SWSI linear model w seasonal correction on HT data p = 0.0638 ####
Gross_Decomp_SP_SWSI_Raw <- full_join(GrossDecomp,SWSI_Platte, by = "Date")  #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI

#Twin lakes data period: 1986-10-01 to 2018-11-01
#POR for discharge data is older than for SWSI. Remove dates where there are no SWSI values. 
Gross_Decomp_SP_SWSI_Raw = na.trim(Gross_Decomp_SP_SWSI_Raw)

CombinedData <- Gross_Decomp_SP_SWSI_Raw


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


Gross_SP_AR1 <- mod_Ar1

#Plot result 
visreg(Gross_SP_AR1, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Outflow Discharge") +
  ggtitle("Gross Outflows by S. Platte SWSI")


# saving the plot as png 
ggsave("Gross_SP_AR1result.png", path = "results/graphs/")




####Dillon - CO SWSI linear model w seasonal correction on HT data p = 0.5XXXX ####
Dillon_Decomp_CO_SWSI_Raw <- full_join(DillonDecomp,SWSI_CO, by = "Date")  #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI

#Twin lakes data period: 1986-10-01 to 2018-11-01
#POR for discharge data is older than for SWSI. Remove dates where there are no SWSI values. 
Dillon_Decomp_CO_SWSI_Raw = na.trim(Dillon_Decomp_CO_SWSI_Raw)

CombinedData <- Dillon_Decomp_CO_SWSI_Raw


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


Dillon_CO_AR1 <- mod_Ar1

#Plot result 
visreg(Dillon_CO_AR1, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Outflow Discharge") +
  ggtitle("Dillon Outflows by Colorado SWSI")


# saving the plot as png 
ggsave("Dillon_CO_AR1result.png", path = "results/graphs/")


####Dillon - SP SWSI linear model w seasonal correction on HT data p = 0.5XXXX ####
Dillon_Decomp_SP_SWSI_Raw <- full_join(DillonDecomp,SWSI_Platte, by = "Date")  #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI

#Twin lakes data period: 1986-10-01 to 2018-11-01
#POR for discharge data is older than for SWSI. Remove dates where there are no SWSI values. 
Dillon_Decomp_SP_SWSI_Raw = na.trim(Dillon_Decomp_SP_SWSI_Raw)

CombinedData <- Dillon_Decomp_SP_SWSI_Raw


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


Dillon_SP_AR1 <- mod_Ar1

#Plot result 
visreg(Dillon_SP_AR1, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Outflow Discharge") +
  ggtitle("Dillon Outflows by S Platte SWSI")


# saving the plot as png 
ggsave("Dillon_SP_AR1result.png", path = "results/graphs/")

