#### read me ####

# This script prepares data for linear modeling, creates a few model structures, and plots the best fitting structures based on evaluations in SJC linearmodelautocorrelation script.   

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

#Pulling out Colorado SWSI. Will use this to create date column to interpolate diversions/outflows
SWSI_CO <- SWSI %>%
  dplyr::select(Date, Colorado) %>%
  rename("SWSI_values" = "Colorado") %>% 
  group_by(Date) %>%
  summarize(SWSI_values=mean(SWSI_values)) #Some dates have two entries. Avg the duplicates here. 

##Twin Lakes ## 
TwinReleases <- read_csv(file = "data/processed/TwinLakesMonthlyReleases") %>%
  rename("Discharge" = "Release") #using the same column name as in diversion data to simplify replication

#There are no NAs but when I combined data with SWSI, I found data gaps. 
#Going to combine SWSI just to create a date column to find NAs where diversion data is missing.
#Then remove SWSI and interpolate missing values. 
TwinInterpolation <- full_join(TwinReleases,SWSI_CO, by = "Date") %>%#Combining SWSI by basin with diversion data
  select(Date,Discharge)

sum(is.na(TwinInterpolation$Discharge)) #79 NAs 
plot(read.zoo(TwinInterpolation, index.column=1, format="%Y-%m-%d")) #plot as time series

#Biiiiiiig data gap 1983-1986 and again after 2018-11-01 
#Setting new data period: 1986-10-01 to 2018-11-01

TwinInterpolationShort <- TwinInterpolation %>%#Combining SWSI by basin with diversion data
  filter(Date >= "1986-10-01", Date <= "2018-11-01")


## fill gaps with spline interpolation ##
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling

# Make univariate zoo time series #
ts.temp<-read.zoo(TwinInterpolationShort, index.column=1, format="%Y-%m-%d")

plot(ts.temp)


# Apply NA interpolation method: Using max gap of 12 months 
Twin_filled = na.spline(ts.temp, na.rm = T, maxgap = 12)
plot(Twin_filled)


par(mfrow=c(1,1)) # reset plotting window
# revert back to df
Twin_filled = as.data.frame(Twin_filled)
Twin_filled$Date = TwinInterpolationShort$Date
names(Twin_filled) = c(colnames(TwinReleases)[1],colnames(TwinReleases)[2])
Twin_filled = Twin_filled %>% dplyr::select(Discharge, Date)

sum(is.na(Twin_filled$Discharge))
#No more NAs.

#some interpolated values went negative. Replace negative values with 0.
Twin_filled$Discharge[Twin_filled$Discharge < 0] = 0 

# check NAs that are left
sum(is.na(Twin_filled$Discharge))
#No more NAs.








## Boustead ## 

BousteadDiversions <- read_csv(file = "data/processed/BousteadMonthlyDiversions")  

#There are no NAs but when I combined data with SWSI, I found data gaps. 
#Going to combine SWSI just to create a date column to find NAs where diversion data is missing.
#Then remove SWSI and interpolate missing values. 
BousteadInterpolation <- full_join(BousteadDiversions,SWSI_CO, by = "Date") %>%#Combining SWSI by basin with diversion data
  select(Date,Discharge) %>%
  #Values before 1989 are funky, so going to reduce the years I am considering. 
  #Am instead only going to use 1990-2023 to keep continuous data. 
  filter(Date >= "1989-11-01") 

## fill gaps with spline interpolation ##
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling

# Make univariate zoo time series #
ts.temp<-read.zoo(BousteadInterpolation, index.column=1, format="%Y-%m-%d")

# ‘order.by’ are not unique warning suggests duplicate time stamps. I found that this is due to time zone changes, so nothing to worry about for regular time steps. 
plot(ts.temp)

# Apply NA interpolation method: Using max gap of 7 months
Boustead_filled = na.spline(ts.temp, na.rm = T, maxgap = 7)
plot(Boustead_filled)


par(mfrow=c(1,1)) # reset plotting window
# revert back to df
Boustead_filled = as.data.frame(Boustead_filled)
Boustead_filled$Date = BousteadInterpolation$Date
names(Boustead_filled) = c(colnames(BousteadDiversions)[2],colnames(BousteadDiversions)[1])
Boustead_filled = Boustead_filled %>% dplyr::select(Discharge, Date)

sum(is.na(Boustead_filled$Discharge))
#No more NAs.

#some interpolated values went negative. Replace negative values with 0.
Boustead_filled$Discharge[Boustead_filled$Discharge < 0] = 0 

# check NAs that are left
sum(is.na(Boustead_filled$Discharge))
#No more NAs.




#### Boustead - Create time series and remove seasonality ####

## prep time series ##
sum(is.na(BousteadDiversions$Date))
#No NAs

sum(is.na(BousteadDiversions$Discharge))
#0 NAs

# check percentage of dataset with NAs 
sum(is.na(BousteadDiversions))/nrow(BousteadDiversions)*100
#0%



#prepping to remove seasonality: 

#set df:
Discharge_data <- Boustead_filled

#create timeseries. IMPORTANT!!!!!!!!!! RESET START DATE TO DATA START DATE!!!! 
#Boustead start date: 1989-11-01. Rembember I removed older data.
timeseries = ts(Discharge_data$Discharge, start = c(1989-11-01), frequency = 12)
#SERIOUSLY CHECK DATES!!!!!! 


##Did you check the date??? ###
#Are you SURE? 



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
# no NAs 

ggplot(Discharge_data_DEs, aes(x=Date, y=Discharge))+
  geom_path() + geom_point() + theme_bw()


BousteadDecomp <- Discharge_data_DEs

sum(is.na(BousteadDecomp$Date))
# no NAs 

#### Twin Lakes - Create time series and remove seasonality ####
## prep time series ##

##data explore## 
hist(TwinReleases$Discharge, breaks = 100)

sum(is.na(TwinReleases$Date))
#No NAs

sum(is.na(TwinReleases$Discharge))
#0 NAs No need to spline interpolate. 

# check percentage of dataset with NAs 
sum(is.na(TwinReleases))/nrow(TwinReleases)*100
#0%


# need to do this to prep for removing seasonality
#set df:
Discharge_data <- Twin_filled

#Check time period!!!!!!
#1986-10-01 to 2018-11-01
timeseries = ts(Discharge_data$Discharge, start = c(1986-10-01), frequency = 12)
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
#Decomposition introduced NAs. Spline interpolate them to fill them. 

## fill gaps with spline interpolation ##
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling
# Make univariate zoo time series #
ts.temp<-read.zoo(Discharge_data_DEs, index.column=1, format="%Y-%m-%d")
# ‘order.by’ are not unique warning suggests duplicate time stamps. I found that this is due to time zone changes, so nothing to worry about for regular time steps. 
plot(ts.temp)
# Apply NA interpolation method: Using max gap of 12 months 
Twin_Decomp = na.spline(ts.temp, na.rm = T, maxgap = 12)
plot(Twin_Decomp)
# revert back to df
Twin_Decomp = as.data.frame(Twin_Decomp)
Twin_Decomp$Date = as.Date(rownames(Twin_Decomp)) 
names(Twin_Decomp) = c(colnames(TwinReleases)[1],colnames(TwinReleases)[2])
Twin_Decomp = Twin_Decomp %>% dplyr::select(Discharge, Date)

sum(is.na(Twin_Decomp$Discharge))
#No more NAs.


ggplot(Twin_Decomp, aes(x=Date, y=Discharge))+
  geom_path() + geom_point() + theme_bw()

ggplot(TwinReleases, aes(x=Date, y=Discharge))+
  geom_path() + geom_point() + theme_bw()

TwinLakesDecomp <- Twin_Decomp

#### CO SWSI Prep for modeling ####

# check for duplicate date/time stamps
anyDuplicated(SWSI_CO$Date)

# check percentage of dataset with NAs 
sum(is.na(SWSI_CO))/nrow(SWSI_CO)*100
#No NAs! 

#### Arkansas SWSI - Prep for modeling ####
## load data and format date/time ##
SWSI_Ark <- SWSI %>%
  dplyr::select(Date, Arkansas) %>%
  rename("SWSI_values" = "Arkansas") %>% 
  group_by(Date) %>% 
  summarize(SWSI_values=mean(SWSI_values)) #Some dates have two entries. Avg the duplicates here. 

# check for duplicate date/time stamps
anyDuplicated(SWSI_Ark$Date)

# check percentage of dataset with NAs 
sum(is.na(SWSI_Ark))/nrow(SWSI_Ark)*100
#No NAs! 

#### Boustead - CO SWSI linear model w seasonal correction on Boustead data and scaled yr - ARMA model p= 0.8191 ####

Boustead_Decomp_CO_SWSI_Raw <- full_join(BousteadDecomp,SWSI_CO, by = "Date")  #Combining SWSI by basin with diversion data

#POR for Boustead data is older than for SWSI. Remove dates where there are no SWSI values. 
Boustead_Decomp_CO_SWSI_Raw  = na.trim(Boustead_Decomp_CO_SWSI_Raw, "both")

sum(is.na(Boustead_Decomp_CO_SWSI_Raw))
#No NAs

CombinedData <- Boustead_Decomp_CO_SWSI_Raw

### linear trends ###
# add simple time steps to df
CombinedData$t = c(1:nrow(CombinedData))

#Ask auto-arima best fit. 
auto.arima(CombinedData$Discharge)
#ARIMA(0,0,2)
#first number is autoregressive coef 0
# middle is differencing data 0
#last number is moving average term 2

#Run ARMA p2 with scaled data 
CombinedData$yr = lubridate::year(CombinedData$Date)# extract just the year
CombinedData$scaled_yr = scale(CombinedData$yr, center = TRUE, scale = FALSE) #scale year to a 0 mean to incorporate as random effect
mod_ARMAp2 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=2), data = CombinedData) #run model

# extract and assess residuals: AMRAp2. 
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSARMAp2 model residuals")
plot(resid(mod_ARMAp2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSARMAp2 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSARMAp2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp2, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp2, type = "normalized"))

par(mfrow=c(1,1))
Acf(resid(mod_ARMAp2))
summary(mod_ARMAp2) #p = 0.8172
visreg(mod_ARMAp2)

Boustead_CO_p2 <- mod_ARMAp2


#Run ARMA p3 with scaled data 
CombinedData$yr = lubridate::year(CombinedData$Date)# extract just the year
CombinedData$scaled_yr = scale(CombinedData$yr, center = TRUE, scale = FALSE) #scale year to a 0 mean to incorporate as random effect
mod_ARMAp3 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=3), data = CombinedData) #run model

# extract and assess residuals: AMRAp3. p = 0.8191
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp3, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSARMAp3 model residuals")
plot(resid(mod_ARMAp3, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSARMAp3 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp3, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSARMAp3 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp3, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp3, type = "normalized"))

par(mfrow=c(1,1))
Acf(resid(mod_ARMAp3))
summary(mod_ARMAp3) #p = 0.3511
visreg(mod_ARMAp3)

Boustead_CO_p3 <- mod_ARMAp3

#P2 has stronger autocorrelation at lag 2 compared to P3, so will use P3 for results. 

#Plot result 
visreg(Boustead_CO_p3, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Discharge") +
  ggtitle("Boustead Diversions by Colorado SWSI")


# saving the plot as png 
ggsave("BousteadCOp3result.png", path = "results/graphs/")


#### Boustead - Ark SWSI linear model w seasonal correction on Boustead data - ARMA model p = 0.6917   ####

Boustead_Decomp_Ark_SWSI_Raw <- full_join(BousteadDecomp,SWSI_Ark, by = "Date")  #Combining SWSI by basin with diversion data

#POR for Boustead data is different than for SWSI. Remove dates where there are no SWSI values. 
Boustead_Decomp_Ark_SWSI_Raw  = na.trim(Boustead_Decomp_Ark_SWSI_Raw, "both")

sum(is.na(Boustead_Decomp_Ark_SWSI_Raw))


CombinedData <- Boustead_Decomp_Ark_SWSI_Raw

### linear trends ###

# add simple time steps to df
CombinedData$t = c(1:nrow(CombinedData))

#Ask auto-arima best fit. 
auto.arima(CombinedData$Discharge)
#ARIMA(0,0,2)
#first number is autoregressive coef 0
# middle is differencing data 0
#last number is moving average term 2

#Run ARMA p2 with scaled data 
CombinedData$yr = lubridate::year(CombinedData$Date)# extract just the year
CombinedData$scaled_yr = scale(CombinedData$yr, center = TRUE, scale = FALSE) #scale year to a 0 mean to incorporate as random effect
mod_ARMAp2 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=2), data = CombinedData) #run model

# extract and assess residuals: AMRAp2. 
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSARMAp2 model residuals")
plot(resid(mod_ARMAp2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSARMAp2 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSARMAp2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp2, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp2, type = "normalized"))

par(mfrow=c(1,1))
Acf(resid(mod_ARMAp2))
summary(mod_ARMAp2) #p = 0.8172
visreg(mod_ARMAp2)

Boustead_Ark_p2 <- mod_ARMAp2


#Run ARMA p3 with scaled data 
CombinedData$yr = lubridate::year(CombinedData$Date)# extract just the year
CombinedData$scaled_yr = scale(CombinedData$yr, center = TRUE, scale = FALSE) #scale year to a 0 mean to incorporate as random effect
mod_ARMAp3 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=3), data = CombinedData) #run model

# extract and assess residuals: AMRAp3. p = 0.8191
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp3, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSARMAp3 model residuals")
plot(resid(mod_ARMAp3, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSARMAp3 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp3, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSARMAp3 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp3, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp3, type = "normalized"))

par(mfrow=c(1,1))
Acf(resid(mod_ARMAp3))
summary(mod_ARMAp3) #p = 0.8191
visreg(mod_ARMAp3)

Boustead_Ark_p3 <- mod_ARMAp3

#Stronger autocorrelation at lag 2 compared to P3, so will use P3 for results. 

#Plot result 
visreg(Boustead_Ark_p3, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Discharge") +
  ggtitle("Boustead Diversions by Arkansas SWSI")


# saving the plot as png 
ggsave("BousteadArkp3result.png", path = "results/graphs/")






####Twin - CO SWSI linear model w seasonal correction on Twin data p = 0.0124 ####
Twin_Decomp_CO_SWSI_Raw <- full_join(TwinLakesDecomp,SWSI_CO, by = "Date")  #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI

#Twin lakes data period: 1986-10-01 to 2018-11-01
#POR for discharge data is different than for SWSI. Remove dates where there are no SWSI values. 
Twin_Decomp_CO_SWSI_Raw  = na.trim(Twin_Decomp_CO_SWSI_Raw, "both")
sum(is.na(Twin_Decomp_CO_SWSI_Raw)) #No NAs. 

CombinedData <- Twin_Decomp_CO_SWSI_Raw


### linear trends ###

# add simple time steps to df
CombinedData$t = c(1:nrow(CombinedData))


#Ask auto-arima best fit. 
auto.arima(CombinedData$Discharge)
#ARIMA(1,0,1)
#first number is autoregressive coef 1
# middle is differencing data 0
#last number is moving average term 1

#Run ARMA p1q1 with scaled data 
CombinedData$yr = lubridate::year(CombinedData$Date)# extract just the year
CombinedData$scaled_yr = scale(CombinedData$yr, center = TRUE, scale = FALSE) #scale year to a 0 mean to incorporate as random effect
mod_ARMAp1q1 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=1, q=1), data = CombinedData) #run model

# extract and assess residuals: AMRAp2. 
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp1q1, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSARMAp1q1 model residuals")
plot(resid(mod_ARMAp1q1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSARMAp1q1 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp1q1, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSARMAp1q1 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp1q1, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp1q1, type = "normalized"))

par(mfrow=c(1,1))
Acf(resid(mod_ARMAp1q1))
summary(mod_ARMAp1q1) #p = 0.0124
visreg(mod_ARMAp1q1)

Twin_CO_p1q1 <- mod_ARMAp1q1


#Good reduction of autocorrelation so will use p1q1 

#Plot result 
visreg(Twin_CO_p1q1, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Discharge") +
  ggtitle("Twin Diversions by Colorado SWSI")


# saving the plot as png 
ggsave("TwinCOp1q1result.png", path = "results/graphs/")


####Twin - Ark SWSI linear model w seasonal correction on Twin data p = 0.0714 ####
Twin_Decomp_Ark_SWSI_Raw <- full_join(TwinLakesDecomp,SWSI_Ark, by = "Date")  #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI

#Twin lakes data period: 1986-10-01 to 2018-11-01
#POR for discharge data is different than for SWSI. Remove dates where there are no SWSI values. 
Twin_Decomp_Ark_SWSI_Raw = na.trim(Twin_Decomp_Ark_SWSI_Raw)  
sum(is.na(Twin_Decomp_Ark_SWSI_Raw)) #No more NAs 

CombinedData <- Twin_Decomp_Ark_SWSI_Raw

### linear trends ###

# add simple time steps to df
CombinedData$t = c(1:nrow(CombinedData))


#Ask auto-arima best fit. 
auto.arima(CombinedData$Discharge)
#ARIMA(1,0,1)
#first number is autoregressive coef 1
# middle is differencing data 0
#last number is moving average term 1

#Run ARMA p1q1 with scaled data 
CombinedData$yr = lubridate::year(CombinedData$Date)# extract just the year
CombinedData$scaled_yr = scale(CombinedData$yr, center = TRUE, scale = FALSE) #scale year to a 0 mean to incorporate as random effect
mod_ARMAp1q1 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=1, q=1), data = CombinedData) #run model

# extract and assess residuals: AMRAp2. 
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp1q1, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSARMAp1q1 model residuals")
plot(resid(mod_ARMAp1q1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSARMAp1q1 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp1q1, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSARMAp1q1 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp1q1, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp1q1, type = "normalized"))

par(mfrow=c(1,1))
Acf(resid(mod_ARMAp1q1))
summary(mod_ARMAp1q1) #p = 0.0714
visreg(mod_ARMAp1q1)

Twin_Ark_p1q1 <- mod_ARMAp1q1


#Good reduction of autocorrelation so will use p1q1 

#Plot result 
visreg(Twin_Ark_p1q1, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Discharge") +
  ggtitle("Twin Diversions by Arkansas SWSI")


# saving the plot as png 
ggsave("TwinArkp1q1result.png", path = "results/graphs/")

