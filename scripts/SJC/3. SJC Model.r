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
library(beepr)
#library(gridExtra)
#library(MARSS)



#### load data and format date/time ####

#SWSI#
SWSI = read.csv("data/processed/SWSI1981to2023.csv", header = T) %>% arrange(Date)
SWSI$Date = as.Date.character(SWSI$Date, format = "%Y-%m-%d")


## Azotea ## 
AzoteaDiversions <- read_csv(file = "data/processed/AzoteaMonthlyDischarge") %>% arrange(Date)
AzoteaDiversions$Date = as.Date(AzoteaDiversions$Date, "%y-%m-%d")

##Heron ## 
HeronReleases <- read_csv(file = "data/processed/HeronMonthlyReleases") %>%
  rename("Discharge" = "Release") %>% #using the same column name as in diversion data to simplify replication
  arrange(Date)

HeronReleases$Date = as.Date(HeronReleases$Date, "%y-%m-%d")

hist(HeronReleases$Discharge, breaks = 100)

#### CO SWSI Prep for modeling ####
SWSI_CO <- SWSI %>%
  dplyr::select(Date, Colorado) %>%
  rename("SWSI_values" = "Colorado") %>% 
  group_by(Date) %>%
  summarize(SWSI_values=mean(SWSI_values)) %>% #Some dates have two entries. Avg the duplicates here. 
  arrange(Date)

# check for duplicate date/time stamps
anyDuplicated(SWSI_CO$Date)

# check percentage of dataset with NAs 
sum(is.na(SWSI_CO))/nrow(SWSI_CO)*100
#No NAs! 

#### RG SWSI - Prep for modeling ####
## load data and format date/time ##
SWSI_RG <- SWSI %>%
  dplyr::select(Date, Rio_Grande) %>%
  rename("SWSI_values" = "Rio_Grande") %>% 
  group_by(Date) %>% 
  summarize(SWSI_values=mean(SWSI_values)) %>% #Some dates have two entries. Avg the duplicates here. 
  arrange(Date)

# check for duplicate date/time stamps
anyDuplicated(SWSI_RG$Date)

# check percentage of dataset with NAs 
sum(is.na(SWSI_RG))/nrow(SWSI_RG)*100
#No NAs! 


####Azotea Interpolation ####
#There are NAs when I combined data with SWSI. 
#Going to combine SWSI just to create a date column to introduce NAs where diversion data is missing.
#Then remove SWSI and interpolate missing values. 
AzoteaInterpolation <- full_join(AzoteaDiversions,SWSI_CO, by = "Date") %>%#Combining SWSI by basin with diversion data
  select(Date,Discharge) %>% arrange(Date)
AzoteaInterpolation = na.trim(AzoteaInterpolation, "both")

## prep time series ##
sum(is.na(AzoteaInterpolation$Date))
#No NAs

sum(is.na(AzoteaInterpolation$Discharge))
#8 NAs

plot(read.zoo(AzoteaInterpolation, index.column=1, format="%Y-%m-%d")) #plot as time series

# Make univariate zoo time series #
AzoteaInterpolation <- AzoteaInterpolation %>% arrange(Date)
ts.temp<-read.zoo(AzoteaInterpolation, index.column=1, format="%Y-%m-%d")

# Apply NA interpolation method: Using max gap of 12 months 
Azotea_filled = na.approx(ts.temp, na.rm = T, maxgap = 12)


par(mfrow=c(2,1))
plot(Azotea_filled)
plot(ts.temp)

par(mfrow=c(1,1)) # reset plotting window
# revert back to dfs
Azotea_filled = as.data.frame(Azotea_filled)
Azotea_filled$Date = AzoteaInterpolation$Date
names(Azotea_filled) = c(colnames(AzoteaInterpolation)[2],colnames(AzoteaInterpolation)[1])
Azotea_filled = na.trim(Azotea_filled, "both")

Azotea_filled <- Azotea_filled %>% dplyr::select(Discharge, Date) %>% arrange(Date)

sum(is.na(Azotea_filled$Discharge))
#No more NAs.

####Heron Interpolation ####
#There are NAs when I combined data with SWSI. 
#Going to combine SWSI just to create a date column to introduce NAs where diversion data is missing.
#Then remove SWSI and interpolate missing values. 
HeronInterpolation <- full_join(HeronReleases,SWSI_CO, by = "Date") %>%#Combining SWSI by basin with diversion data
  select(Date,Discharge) %>% arrange(Date)
HeronInterpolation = na.trim(HeronInterpolation, "both")

## prep time series ##
sum(is.na(HeronInterpolation$Date))
#No NAs

sum(is.na(HeronInterpolation$Discharge))
#2 NAs

plot(read.zoo(HeronInterpolation, index.column=1, format="%Y-%m-%d")) #plot as time series

# Make univariate zoo time series #
HeronInterpolation <- HeronInterpolation %>% arrange(Date)
ts.temp<-read.zoo(HeronInterpolation, index.column=1, format="%Y-%m-%d")

# Apply NA interpolation method: Using max gap of 12 months 
Heron_filled = na.approx(ts.temp, na.rm = T, maxgap = 12)

par(mfrow=c(2,1))
plot(Heron_filled)
plot(ts.temp)

par(mfrow=c(1,1)) # reset plotting window
# revert back to dfs
Heron_filled = as.data.frame(Heron_filled)
Heron_filled$Date = HeronInterpolation$Date
names(Heron_filled) = c(colnames(HeronInterpolation)[2],colnames(HeronInterpolation)[1])
Heron_filled = na.trim(Heron_filled, "both")

Heron_filled <- Heron_filled %>% dplyr::select(Discharge, Date) %>% arrange(Date)

sum(is.na(Heron_filled$Discharge))
#No more NAs.

#### Azotea - Create time series and remove seasonality ####

## prep time series ##
sum(is.na(Azotea_filled$Date))
#No NAs

sum(is.na(Azotea_filled$Discharge))
#0 NAs

# check percentage of dataset with NAs 
sum(is.na(Azotea_filled))/nrow(Azotea_filled)*100
#0

#prepping to remove seasonality: 
#set df:
Discharge_data <- Azotea_filled %>% arrange(Date)

#create timeseries BE SURE TO CHECK DATE START!!!!
#1970-10-01
timeseries = ts(Discharge_data$Discharge, start = c(1970, 10), frequency = 12)
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

AzoteaDecomp <- Discharge_data_DEs

#### Heron - Create time series and remove seasonality ####
## prep time series ##

##data explore## 
hist(Heron_filled$Discharge, breaks = 100)


sum(is.na(Heron_filled$Date))
#No NAs

sum(is.na(Heron_filled$Discharge))
#0 NAs

# check percentage of dataset with NAs 
sum(is.na(Heron_filled))/nrow(Heron_filled)*100
#0

# need to do this to prep for removing seasonality
#set df:
Discharge_data <- Heron_filled %>% arrange(Date)
#CHECK START DATE!!!!! 2008-01-01
timeseries = ts(Discharge_data$Discharge, start = c(2008, 01), frequency = 12)
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

HeronDecomp <- Discharge_data_DEs


####Azotea - CO SWSI linear model w seasonal correction on Azotea data - ARIMA model  ####

Azotea_Decomp_CO_SWSI_Raw <- full_join(AzoteaDecomp,SWSI_CO, by = "Date")  #Combining SWSI by basin with diversion data, Azotea Tunnel, CO SWSI
Azotea_Decomp_CO_SWSI_Raw$Discharge = as.numeric(Azotea_Decomp_CO_SWSI_Raw$Discharge)
Azotea_Decomp_CO_SWSI_Raw$SWSI_values = as.numeric(Azotea_Decomp_CO_SWSI_Raw$SWSI_values)

#POR for Azotea data is older than for SWSI. Remove dates where there are no SWSI values. 
Azotea_Decomp_CO_SWSI_Raw = na.trim(Azotea_Decomp_CO_SWSI_Raw, "both")

CombinedData <- Azotea_Decomp_CO_SWSI_Raw %>% arrange(Date)

### linear trends ###

# add simple time steps to df
CombinedData$t = c(1:nrow(CombinedData))

#Ask auto-arima best fit. 
auto.arima(CombinedData$Discharge)

#ARIMA(0,0,3)
#first number is autoregressive coef 0
# middle is differencing data 0
#last number is moving average term 3

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

#Pretty signficant autocorrelation. Going to try some other models. 
#several lags have autocorrelation. Run some other models to test fit.
mod_AR1 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corAR1(), data = CombinedData) #run mode
mod_ARMAp1q1 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=1,q=1), data = CombinedData) #run mode
mod_ARMAp2 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=2,q=0), data = CombinedData) 
mod_ARMAp3 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=3,q=0), data = CombinedData) 
mod_ARMAp0q2 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=0,q=2), data = CombinedData) 
mod_ARMAp1q2 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=1,q=2), data = CombinedData) 

#Compare models with BIC 
bbmle::AICtab(mod_AR1,mod_ARMAp1q1,mod_ARMAp2,mod_ARMAp3,mod_ARMAp0q2) 

            # dAIC df
# mod_ARMAp3   0.0  7 
# mod_ARMAp2   0.6  6 
# mod_ARMAp1q1 2.2  6 
# mod_ARMAp0q2 2.3  6 
# mod_Ar1      7.8  5 

# extract and assess residuals: AMRAp3. 
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp3, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSARMAp3 model residuals")
plot(resid(mod_ARMAp3, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSARMAp3 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp3, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSARMAp3 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp3, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp3, type = "normalized"))
#Good. Significant autocorrelation at lag 1, but is 0.10-0.15 

# extract and assess residuals: AR1. 
par(mfrow=c(1,3))
Acf(resid(mod_AR1, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAR1 model residuals")
plot(resid(mod_AR1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSAR1 model residuals"); abline(h=0)
qqnorm(resid(mod_AR1, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAR1 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AR1, type = "normalized"))$statistic,2))); qqline(resid(mod_AR1, type = "normalized"))
#Worse. 

# extract and assess residuals: AMRAp1q1. 
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp1q1, type = "normalized"), main="Discharge adjusted, Raw SWSI ARMAp1q1 model residuals")
plot(resid(mod_ARMAp1q1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI ARMAp1q1 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp1q1, type = "normalized"), main="Discharge adjusted, Raw SWSI p1q1 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp1q1, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp1q1, type = "normalized"))
#Worse. Won't use this one. 


# extract and assess residuals: AMRAp0q2. 
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp0q2, type = "normalized"), main="Discharge adjusted, Raw SWSI ARMAp0q2 model residuals")
plot(resid(mod_ARMAp0q2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI ARMAp0q2 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp0q2, type = "normalized"), main="Discharge adjusted, Raw SWSI p0q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp0q2, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp0q2, type = "normalized"))
#Worse still. Won't use this one. Going to use p3. 


Azotea_CO_ARMAp3 <- mod_ARMAp3

summary(Azotea_CO_ARMAp3)


cor(CombinedData$Discharge, fitted(Azotea_CO_ARMAp3), method = "kendall")
?cor


#Plot result 
Azotea_CO_p3_plot <- visreg(Azotea_CO_ARMAp3, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Outflow Discharge") +
  ggtitle("Azotea Diversions by CO SWSI")


# saving the plot as png 
ggsave("Azotea_CO_ARMAp3.png", plot = Azotea_CO_p3_plot, path = "results/graphs/")


# bootstrap confidence intervals
Azotea_CO_ARMAp3_boot<-lmeresampler::bootstrap(model = Azotea_CO_ARMAp3, .f = fixef, type = "reb", B = 1000, reb_type = 2)
Azotea_CO_ARMAp3_CIs = confint(Azotea_CO_ARMAp3_boot, type="norm",level = 0.95)
Azotea_CO_ARMAp3_CIplot = plot(Azotea_CO_ARMAp3_boot, "beta.SWSI_values")
Azotea_CO_ARMAp3_CIplot_custom = Azotea_CO_ARMAp3_CIplot + ggtitle("Azotea vs Colorado SWSI") + xlab("beta SWSI values")
plot(Azotea_CO_ARMAp3_CIplot_custom)

####Azotea - RG SWSI linear model w seasonal correction on Azotea data - ARIMA model r = 0.4052106  ####

Azotea_Decomp_RG_SWSI_Raw <- full_join(AzoteaDecomp,SWSI_RG, by = "Date")  #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI

#POR for Azotea data is older than for SWSI. Remove dates where there are no SWSI values. 
Azotea_Decomp_RG_SWSI_Raw = na.trim(Azotea_Decomp_RG_SWSI_Raw, "both")

Azotea_Decomp_RG_SWSI_Raw$Discharge = as.numeric(Azotea_Decomp_RG_SWSI_Raw$Discharge)
Azotea_Decomp_RG_SWSI_Raw$SWSI_values = as.numeric(Azotea_Decomp_RG_SWSI_Raw$SWSI_values) 

CombinedData <- Azotea_Decomp_RG_SWSI_Raw %>% arrange(Date)

### linear trends ###

CombinedData$t = c(1:nrow(CombinedData))

### linear trends ###
#Run ARMA p3 with scaled data 
CombinedData$yr = lubridate::year(CombinedData$Date)# extract just the year
CombinedData$scaled_yr = scale(CombinedData$yr, center = TRUE, scale = FALSE) #scale year to a 0 mean to incorporate as random effect
mod_ARMAp3 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=2), data = CombinedData) #run model


# extract and assess residuals: AMRAp3. 
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp3, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSARMAp3 model residuals")
plot(resid(mod_ARMAp3, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSARMAp3 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp3, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSARMAp3 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp3, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp3, type = "normalized"))
#Autocorrelation at lag 1 and 2 between 0.10-0.15.


Azotea_RG_p3 <- mod_ARMAp3

summary (Azotea_RG_p3)
cor(CombinedData$Discharge, fitted(Azotea_RG_p3),method = "kendall")

#Plot result 
Azotea_RG_p3_plot <- visreg(Azotea_RG_p3, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Discharge") +
  ggtitle("Azotea Diversions by Rio Grande SWSI")

### Azotea - Plot both basins against eachother. Colorado on bottom ###
Azoteap3result <- gridExtra::grid.arrange(Azotea_RG_p3_plot, Azotea_CO_p3_plot, ncol=1)

ggsave("Azoteap3result.png", plot = Azoteap3result, path = "results/graphs/")

# bootstrap confidence intervals
Azotea_RG_ARMAp3_boot<-lmeresampler::bootstrap(model = Azotea_RG_p3, .f = fixef, type = "reb", B = 1000, reb_type = 2)
Azotea_RG_ARMAp3_CIs = confint(Azotea_RG_ARMAp3_boot, type="norm",level = 0.95)
Azotea_RG_ARMAp3_CIplot = plot(Azotea_RG_ARMAp3_boot, "beta.SWSI_values")
Azotea_RG_ARMAp3_CIplot_custom = Azotea_RG_ARMAp3_CIplot + ggtitle("Azotea vs Rio Grande SWSI") + xlab("beta SWSI values")
plot(Azotea_RG_ARMAp3_CIplot_custom)
### Azotea - Plot both basins bootstrap intervals against eachother. Colorado on bottom ###
AzoteaARMP3_boot <- gridExtra::grid.arrange(Azotea_RG_ARMAp3_CIplot_custom, Azotea_CO_ARMAp3_CIplot_custom, ncol=1)

# saving the plot as png 
ggsave("Azotea_ARMAp3_boot.png", plot = AzoteaARMP3_boot, path = "results/graphs/")


####Heron - RG SWSI linear model w seasonal correction on Heron data r = 0.2370291  ####
Heron_Decomp_RG_SWSI_Raw <- full_join(HeronDecomp,SWSI_RG, by = "Date") %>% arrange(Date)  #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI

#POR for discharge data is older than for SWSI. Remove dates where there are no SWSI values. 
Heron_Decomp_RG_SWSI_Raw = na.trim(Heron_Decomp_RG_SWSI_Raw, "both")

CombinedData <- Heron_Decomp_RG_SWSI_Raw %>% arrange(Date)

### linear trends ###

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
#Signficant autocorrelation at lag 19. Will try other models.  

#several lags have autocorrelation. Run some other models to test fit.I misspelled ARMA so these models have funky names. 
mod_ARMAp1q1 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=1,q=1), data = CombinedData) #run mode
mod_ARMAp2 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=2,q=0), data = CombinedData) 
mod_ARMAp3 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=3,q=0), data = CombinedData) 
mod_ARMAp0q2 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=0,q=2), data = CombinedData) 
mod_ARMAp1q2 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=1,q=2), data = CombinedData) 
mod_ARMAp2q2 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=2,q=2), data = CombinedData) 

#Compare models with BIC 
bbmle::AICtab(mod_AR1,mod_ARMAp1q1,mod_ARMAp2,mod_ARMAp3,mod_ARMAp0q2,mod_ARMAp1q2) 

# #              dAIC   df
# mod_ARMAp1q1    0.0 6 
# mod_ARMAp2      0.3 6 
# mod_ARMAp1q2    0.6 7 
# mod_ARMAp3      1.1 7 
# mod_ARMAp0q2    2.8 6 
# mod_Ar1      7114.2 5


# extract and assess residuals: AMRAp1q1. 
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp1q1, type = "normalized"), main="Discharge adjusted, Raw SWSI ARMAp1q1 model residuals")
plot(resid(mod_ARMAp1q1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI ARMAp1q1 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp1q1, type = "normalized"), main="Discharge adjusted, Raw SWSI p1q1 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp1q1, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp1q1, type = "normalized"))
#Worse. 

# extract and assess residuals: mod_ARMAp2
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp2, type = "normalized"), main="Discharge adjusted, Raw SWSI ARMAp2 model residuals")
plot(resid(mod_ARMAp2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI ARMAp2 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp2, type = "normalized"), main="Discharge adjusted, Raw SWSI p1q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp2, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp2, type = "normalized"))
#Slightly better, but worse than AR1


# extract and assess residuals: mod_ARMAp1q2
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp1q2, type = "normalized"), main="Discharge adjusted, Raw SWSI ARMAp1q2 model residuals")
plot(resid(mod_ARMAp1q2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI ARMAp1q2 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp1q2, type = "normalized"), main="Discharge adjusted, Raw SWSI p1q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp1q2, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp1q2, type = "normalized"))
#Better than AR1. 


# extract and assess residuals: AMRAp3. 
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp3, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSARMAp3 model residuals")
plot(resid(mod_ARMAp3, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSARMAp3 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp3, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSARMAp3 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp3, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp3, type = "normalized"))
#Worse. Going to use p1q2. 


Heron_RG_p1q2 <- mod_ARMAp1q2

summary(Heron_RG_p1q2)
cor(CombinedData$Discharge, fitted(Heron_RG_p1q2), method = "kendall")

#Plot result 
Heron_RG_p1q2_chart <- visreg(Heron_RG_p1q2, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Discharge") +
  ggtitle("Heron Diversions by Rio Grande SWSI")

# saving the plot as png 
ggsave("Heron_RG_p1q2.png", plot = Heron_RG_p1q2_chart, path = "results/graphs/")

# bootstrap confidence intervals
Heron_RG_p1q2_boot<-lmeresampler::bootstrap(model = Heron_RG_p1q2, .f = fixef, type = "reb", B = 1000, reb_type = 2)
Heron_RG_p1q2_CIs = confint(Heron_RG_p1q2_boot, type="norm",level = 0.95)
Heron_RG_p1q2_CIplot = plot(Heron_RG_p1q2_boot, "beta.SWSI_values")
Heron_RG_p1q2_CIplot_custom = Heron_RG_p1q2_CIplot + ggtitle("Heron vs Rio Grande SWSI") + xlab("beta SWSI values")
plot(Heron_RG_p1q2_CIplot_custom)


####Heron - CO SWSI linear model w seasonal correction on Heron data r = 0.2370291  ####
Heron_Decomp_CO_SWSI_Raw <- full_join(HeronDecomp,SWSI_CO, by = "Date") %>% arrange(Date) #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI
Heron_Decomp_CO_SWSI_Raw$Discharge = as.numeric(Heron_Decomp_CO_SWSI_Raw$Discharge)
Heron_Decomp_CO_SWSI_Raw$SWSI_values = as.numeric(Heron_Decomp_CO_SWSI_Raw$SWSI_values)

#POR for discharge data is older than for SWSI. Remove dates where there are no SWSI values. 
Heron_Decomp_CO_SWSI_Raw = na.trim(Heron_Decomp_CO_SWSI_Raw, "both")

CombinedData <- Heron_Decomp_CO_SWSI_Raw %>% arrange(Date)
sum(is.na(CombinedData$Discharge))

### linear trends ###

#Scale data and run model
CombinedData$t = c(1:nrow(CombinedData))
CombinedData$yr = lubridate::year(CombinedData$Date)# extract just the year
CombinedData$scaled_yr = scale(CombinedData$yr, center = TRUE, scale = FALSE) #scale year to a 0 mean to incorporate as random effect

#Model ARMAp1q1
mod_ARMAp1q1 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=1,q=1), data = CombinedData) #run mode

# extract and assess residuals: mod_ARMAp1q2
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp1q2, type = "normalized"), main="Discharge adjusted, Raw SWSI ARMAp1q2 model residuals")
plot(resid(mod_ARMAp1q2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI ARMAp1q2 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp1q2, type = "normalized"), main="Discharge adjusted, Raw SWSI p1q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp1q2, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp1q2, type = "normalized"))
#Autocorrelation at lag 2 (under 0.2), and at lag 19 (under 0.2)



Heron_CO_p1q2 <- mod_ARMAp1q2

summary (Heron_CO_p1q2)
cor(CombinedData$Discharge, fitted(Heron_CO_p1q2), method = "kendall")

#Plot result 
Heron_CO_p1q2_chart <- visreg(Heron_CO_p1q2, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Discharge") +
  ggtitle("Heron Outflows by Colorado SWSI")

### Heron - Plot both basins against eachother. Colorado on bottom ###
HeronP1q2Result <- gridExtra::grid.arrange(Heron_RG_p1q2_chart, Heron_CO_p1q2_chart, ncol=1)


# saving the plot as png 
ggsave("HeronP1q2Result.png", plot = HeronP1q2Result, path = "results/graphs/")


# bootstrap confidence intervals
Heron_CO_p1q2_boot<-lmeresampler::bootstrap(model = Heron_CO_p1q2, .f = fixef, type = "reb", B = 1000, reb_type = 2)
Heron_CO_p1q2_CIs = confint(Heron_CO_p1q2_boot, type="norm",level = 0.95)
Heron_CO_p1q2_CIplot = plot(Heron_CO_p1q2_boot, "beta.SWSI_values")
Heron_CO_p1q2_CIplot_custom = Heron_CO_p1q2_CIplot + ggtitle("Heron vs Colorado SWSI") + xlab("beta SWSI values")
plot(Heron_CO_p1q2_CIplot_custom)

### Heron - Plot both basins bootstrap intervals against eachother. Colorado on bottom ###
Heronp1q2_boot <- gridExtra::grid.arrange(Heron_RG_p1q2_CIplot_custom, Heron_CO_p1q2_CIplot_custom, ncol=1)

# saving the plot as png 
ggsave("Heron_p1q2_boot.png",plot = Heronp1q2_boot, path = "results/graphs/")


beep(sound = 5)
