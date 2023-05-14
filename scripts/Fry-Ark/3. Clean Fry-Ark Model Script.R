#### read me ####

# This script prepares data for linear modeling, then models the data. 

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

##Twin Lakes ## 
TwinReleases <- read_csv(file = "data/processed/TwinLakesMonthlyReleases") %>%
  rename("Discharge" = "Release") %>% #using the same column name as in diversion data to simplify replication
  arrange (Date) 

## Boustead ## 
BousteadDiversions <- read_csv(file = "data/processed/BousteadMonthlyDiversions")  %>% 
  arrange(Date)

hist(TwinReleases$Discharge, breaks = 100)

#### CO SWSI Prep for modeling ####
#Pulling out Colorado SWSI. Will use this to create date column to interpolate diversions/outflows
SWSI_CO <- SWSI %>%
  dplyr::select(Date, Colorado) %>%
  rename("SWSI_values" = "Colorado") %>% 
  group_by(Date) %>%
  #Some dates have two entries. Avg the duplicates here. 
  summarize(SWSI_values=mean(SWSI_values)) %>% 
  arrange (Date) 

# check for duplicate date/time stamps
anyDuplicated(SWSI_CO$Date)

# check percentage of dataset with NAs 
sum(is.na(SWSI_CO))/nrow(SWSI_CO)*100
#No NAs! 

#### Arkansas SWSI - Prep for modeling ####
## Pull out Ark data and and format date/time ##
SWSI_Ark <- SWSI %>%
  dplyr::select(Date, Arkansas) %>%
  rename("SWSI_values" = "Arkansas") %>% 
  group_by(Date) %>% 
  summarize(SWSI_values=mean(SWSI_values)) %>% #Some dates have two entries. Avg the duplicates here. 
  arrange(Date)

# check for duplicate date/time stamps
anyDuplicated(SWSI_Ark$Date)

# check percentage of dataset with NAs 
sum(is.na(SWSI_Ark))/nrow(SWSI_Ark)*100
#No NAs! 


#### Twin Lakes Data exploration and Interpolation ####
#There are no NAs but when I combined data with SWSI, I found data gaps. 
#Going to combine SWSI just to create a date column to find NAs where diversion data is missing.
#Then remove SWSI and interpolate missing values. 
TwinInterpolation <- full_join(TwinReleases,SWSI_CO, by = "Date") %>%#Combining SWSI by basin with diversion data
  select(Date,Discharge) %>% arrange(Date)

sum(is.na(TwinInterpolation$Discharge)) #79 NAs 
plot(read.zoo(TwinInterpolation, index.column=1, format="%Y-%m-%d")) #plot as time series

#Biiiiiiig data gap 1983-1986 and again after 2018-11-01 
#Setting new data period: 1986-10-01 to 2018-11-01

TwinInterpolationShort <- TwinInterpolation %>%#Combining SWSI by basin with diversion data
  filter(Date >= "1986-10-01", Date <= "2018-11-01") %>% arrange(Date)


## fill gaps with spline interpolation ##
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling

# Make univariate zoo time series #
ts.temp<-read.zoo(TwinInterpolationShort, index.column=1, format="%Y-%m-%d")

plot(ts.temp)


# Apply NA linear interpolation method: Using max gap of 12 months 
Twin_filled = na.approx(ts.temp, na.rm = T, maxgap = 12)
plot(Twin_filled)
par(mfrow=c(1,1)) # reset plotting window

# revert back to df
Twin_filled = as.data.frame(Twin_filled)
Twin_filled$Date <- as.Date(rownames(Twin_filled))
names(Twin_filled) = c(colnames(TwinReleases)[1],colnames(TwinReleases)[2])
Twin_filled = Twin_filled %>% dplyr::select(Discharge, Date) %>% arrange(Date)

sum(is.na(Twin_filled$Discharge))
#No more NAs.


# check NAs that are left
sum(is.na(Twin_filled$Discharge))
#No more NAs.



#### Boustead Data exploration ####
#There are no NAs but when I combined data with SWSI, I found data gaps. 
#Going to combine SWSI just to create a date column to find NAs where diversion data is missing.
#Then remove SWSI and interpolate missing values. 
BousteadInterpolation <- full_join(BousteadDiversions,SWSI_CO, by = "Date") %>%#Combining SWSI by basin with diversion data
  select(Date,Discharge) %>% arrange(Date) 

# Make univariate zoo time series #
ts.temp<-read.zoo(BousteadInterpolation, index.column=1, format="%Y-%m-%d")
plot(ts.temp)

#Values before 1989 are funky and gaps are too large to interpolate. 
#Going to reduce the years I am considering to 1989-11-01 to present. 
BousteadInterpolation <- BousteadInterpolation %>% filter(Date >= "1989-11-01") %>% 
  arrange(Date) 

#Once I filtered it, there were no more NAs. No interpolation needed. 
sum(is.na(BousteadInterpolation))

Boustead_filled <- BousteadInterpolation

#### Boustead - Create time series and remove seasonality ####

## prep time series ##
sum(is.na(Boustead_filled$Date))
#No NAs

sum(is.na(Boustead_filled$Discharge))
#0 NAs

# check percentage of dataset with NAs 
sum(is.na(Boustead_filled))/nrow(Boustead_filled)*100
#0%



#prepping to remove seasonality: 

#set df:
Discharge_data <- Boustead_filled %>% arrange(Date)

#create timeseries. IMPORTANT!!!!!!!!!! RESET START DATE TO DATA START DATE!!!! 
#Boustead start date: 1989-11-01. Rembember I removed older data.
timeseries = ts(Discharge_data$Discharge, start = c(1989, 11), frequency = 12)
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
# no NAs 

ggplot(Discharge_data_DEs, aes(x=Date, y=Discharge))+
  geom_path() + geom_point() + theme_bw()

BousteadDecomp <- Discharge_data_DEs

sum(is.na(BousteadDecomp$Date))
# no NAs 

#### Twin Lakes - Create time series and remove seasonality ####
## prep time series ##

##data explore## 
hist(Twin_filled$Discharge, breaks = 100)

sum(is.na(Twin_filled$Date))
#No NAs

sum(is.na(Twin_filled$Discharge))
#0 NAs. 

# check percentage of dataset with NAs 
sum(is.na(Twin_filled))/nrow(Twin_filled)*100
#0%


# need to do this to prep for removing seasonality
#set df:
Discharge_data <- Twin_filled

#Check time period!!!!!!
#1986-10-01 to 2018-11-01
timeseries = ts(Discharge_data$Discharge, start = c(1986, 10), frequency = 12)
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

TwinLakesDecomp <- Discharge_data_DEs %>% arrange(Date)


#### Boustead - CO SWSI linear model w seasonal correction on Boustead data and scaled yr - ARMA model p= 0.0016 ####

Boustead_Decomp_CO_SWSI_Raw <- full_join(BousteadDecomp,SWSI_CO, by = "Date") %>% arrange(Date)  #Combining SWSI by basin with diversion data

#POR for Boustead data is older than for SWSI. Remove dates where there are no SWSI values. 
Boustead_Decomp_CO_SWSI_Raw  = na.trim(Boustead_Decomp_CO_SWSI_Raw, "both")

sum(is.na(Boustead_Decomp_CO_SWSI_Raw))
#No NAs

CombinedData <- Boustead_Decomp_CO_SWSI_Raw %>% arrange(Date)

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

#Autocorrelation at lag 24, Under 0.15. Ok to use. 

par(mfrow=c(1,1))
summary(mod_ARMAp2) 
visreg(mod_ARMAp2)

Boustead_CO_p2 <- mod_ARMAp2
cor(CombinedData$Discharge, fitted(Boustead_CO_p2), method = "kendall")


#Plot result 
Boustead_CO_p2_chart <- visreg(Boustead_CO_p2, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Discharge") +
  ggtitle("Boustead Diversions by Colorado SWSI")


# bootstrap confidence intervals
Boustead_CO_p2_boot<-lmeresampler::bootstrap(model = Boustead_CO_p2, .f = fixef, type = "reb", B = 1000, reb_type = 2)
Boustead_CO_p2_CIs = confint(Boustead_CO_p2_boot, type="norm",level = 0.95)
Boustead_CO_p2_CIplot = plot(Boustead_CO_p2_boot, "beta.SWSI_values")
Boustead_CO_p2_CIplot_custom = Boustead_CO_p2_CIplot + ggtitle("Boustead vs Colorado SWSI") + xlab("beta SWSI values")
plot(Boustead_CO_p2_CIplot_custom)



#### Boustead - Ark SWSI linear model w seasonal correction on Boustead data - ARMA model p = 0.6917   ####

Boustead_Decomp_Ark_SWSI_Raw <- full_join(BousteadDecomp,SWSI_Ark, by = "Date") %>% arrange(Date) #Combining SWSI by basin with diversion data

#POR for Boustead data is different than for SWSI. Remove dates where there are no SWSI values. 
Boustead_Decomp_Ark_SWSI_Raw  = na.trim(Boustead_Decomp_Ark_SWSI_Raw, "both")

sum(is.na(Boustead_Decomp_Ark_SWSI_Raw))
#No nas. 

CombinedData <- Boustead_Decomp_Ark_SWSI_Raw %>% arrange(Date)

### linear trends ###

# add simple time steps to df
CombinedData$t = c(1:nrow(CombinedData))

#use arma p2, same as boustead - colorado. 
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

#Autocorrelation at lag 24. Below 0.15. Ok to use. 

par(mfrow=c(1,1))
summary(mod_ARMAp2) 
visreg(mod_ARMAp2)

Boustead_Ark_p2 <- mod_ARMAp2
cor(CombinedData$Discharge, fitted(Boustead_Ark_p2), method = "kendall")


#Plot result 
Boustead_Ark_p2_chart <- visreg(Boustead_Ark_p2, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Discharge") +
  ggtitle("Boustead Diversions by Arkansas SWSI")

### Plot both basins against eachother. Colorado on bottom ###
Boustead_Ark_p2_Result <- gridExtra::grid.arrange(Boustead_Ark_p2_chart, Boustead_CO_p2_chart, ncol=1)


# saving the plot as png 
ggsave("Boustead_Ark_p2_Result.png", plot = Boustead_Ark_p2_Result, path = "results/graphs/")


# bootstrap confidence intervals
Boustead_Ark_p2_boot<-lmeresampler::bootstrap(model = Boustead_Ark_p2, .f = fixef, type = "reb", B = 1000, reb_type = 2)
Boustead_Ark_p2_CIs = confint(Boustead_Ark_p2_boot, type="norm",level = 0.95)
Boustead_Ark_p2_CIplot = plot(Boustead_Ark_p2_boot, "beta.SWSI_values")
Boustead_Ark_p2_CIplot_custom = Boustead_Ark_p2_CIplot + ggtitle("Boustead vs Arkansas SWSI") + xlab("beta SWSI values")
plot(Boustead_Ark_p2_CIplot_custom)

#Boustead - Plot both bootstrap plots for both river basins against eachother. Colorado on bottom. 
Boustead_p2_boot_chart <- gridExtra::grid.arrange(Boustead_Ark_p2_CIplot_custom, Boustead_CO_p2_CIplot_custom, ncol=1)

#save plots
ggsave("Boustead_p2_boot.png", plot = Boustead_p2_boot_chart, path = "results/graphs/")


####Twin - CO SWSI linear model w seasonal correction on Twin data p = 0.0124 ####
Twin_Decomp_CO_SWSI_Raw <- full_join(TwinLakesDecomp,SWSI_CO, by = "Date")  #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI

#Twin lakes data period: 1986-10-01 to 2018-11-01
#POR for discharge data is different than for SWSI. Remove dates where there are no SWSI values. 
Twin_Decomp_CO_SWSI_Raw  = na.trim(Twin_Decomp_CO_SWSI_Raw, "both")
sum(is.na(Twin_Decomp_CO_SWSI_Raw)) #No NAs. 

CombinedData <- Twin_Decomp_CO_SWSI_Raw %>% arrange(Date)


### linear trends ###

# add simple time steps to df
CombinedData$t = c(1:nrow(CombinedData))


#Ask auto-arima best fit. 
auto.arima(CombinedData$Discharge)
#ARIMA(1,0,0)
#first number is autoregressive coef 1
# middle is differencing data 0
#last number is moving average term 0

#Run ARMA p1 with scaled data
CombinedData$yr = lubridate::year(CombinedData$Date)# extract just the year
CombinedData$scaled_yr = scale(CombinedData$yr, center = TRUE, scale = FALSE) #scale year to a 0 mean to incorporate as random effect
mod_ARMAp1 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=1, q=0), data = CombinedData) #run model

# extract and assess residuals: AMRAp1.
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp1, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSARMAp1 model residuals")
plot(resid(mod_ARMAp1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSARMAp1 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp1, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSARMAp1 model residuals", pch=16,
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp1, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp1, type = "normalized"))

#Very little autocorrelation at lag 3 and 22. Ok to use. 

par(mfrow=c(1,1))
summary(mod_ARMAp1) #p = 0.0066
visreg(mod_ARMAp1)

Twin_CO_p1 <- mod_ARMAp1
cor(CombinedData$Discharge, fitted(Twin_CO_p1), method = "kendall")



#Plot result 
Twin_CO_p1_chart <- visreg(Twin_CO_p1, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Discharge") +
  ggtitle("Twin Diversions by Colorado SWSI")

# bootstrap confidence intervals
Twin_CO_p1_boot<-lmeresampler::bootstrap(model = Twin_CO_p1, .f = fixef, type = "reb", B = 1000, reb_type = 2)
Twin_CO_p1_CIs = confint(Twin_CO_p1_boot, type="norm",level = 0.95)
Twin_CO_p1_CIplot = plot(Twin_CO_p1_boot, "beta.SWSI_values")
Twin_CO_p1_CIplot_custom = Twin_CO_p1_CIplot + ggtitle("Twin vs Colorado SWSI") + xlab("beta SWSI values")
plot(Twin_CO_p1_CIplot_custom)


####Twin - Ark SWSI linear model w seasonal correction on Twin data p = 0.0714 ####
Twin_Decomp_Ark_SWSI_Raw <- full_join(TwinLakesDecomp,SWSI_Ark, by = "Date")  %>% arrange(Date) #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI


#Twin lakes data period: 1986-10-01 to 2018-11-01
#POR for discharge data is different than for SWSI. Remove dates where there are no SWSI values. 
Twin_Decomp_Ark_SWSI_Raw = na.trim(Twin_Decomp_Ark_SWSI_Raw)  
sum(is.na(Twin_Decomp_Ark_SWSI_Raw)) #No more NAs 

CombinedData <- Twin_Decomp_Ark_SWSI_Raw

### linear trends ###

# add simple time steps to df
CombinedData$t = c(1:nrow(CombinedData))

#Run ARMA p1 with scaled data
CombinedData$yr = lubridate::year(CombinedData$Date)# extract just the year
CombinedData$scaled_yr = scale(CombinedData$yr, center = TRUE, scale = FALSE) #scale year to a 0 mean to incorporate as random effect
mod_ARMAp1 <- lme(Discharge ~ SWSI_values, random = ~1 | scaled_yr, correlation = corARMA(p=1, q=0), data = CombinedData) #run model

# extract and assess residuals: AMRAp1.
par(mfrow=c(1,3))
Acf(resid(mod_ARMAp1, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSARMAp1 model residuals")
plot(resid(mod_ARMAp1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSARMAp1 model residuals"); abline(h=0)
qqnorm(resid(mod_ARMAp1, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSARMAp1 model residuals", pch=16,
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_ARMAp1, type = "normalized"))$statistic,2))); qqline(resid(mod_ARMAp1, type = "normalized"))

#No autocorrelation woohooo!!! 

par(mfrow=c(1,1))
summary(mod_ARMAp1) #p = 0.0247
visreg(mod_ARMAp1)

Twin_Ark_p1 <- mod_ARMAp1
cor(CombinedData$Discharge, fitted(Twin_Ark_p1), method = "kendall")



#Plot result 
Twin_Ark_p1_chart <- visreg(Twin_Ark_p1, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Discharge") +
  ggtitle("Twin Diversions by Arkansas SWSI")

### Plot both basins against eachother. Colorado on bottom ###
Twin_p1_chart <- gridExtra::grid.arrange(Twin_Ark_p1_chart, Twin_CO_p1_chart, ncol=1)


# saving the plot as png 
ggsave("Twin_p1_chart.png", plot = Twin_p1_chart, path = "results/graphs/")

# bootstrap confidence intervals
Twin_Ark_p1_boot<-lmeresampler::bootstrap(model = Twin_Ark_p1, .f = fixef, type = "reb", B = 1000, reb_type = 2)
Twin_Ark_p1_CIs = confint(Twin_Ark_p1_boot, type="norm",level = 0.95)
Twin_Ark_p1_CIplot = plot(Twin_Ark_p1_boot, "beta.SWSI_values")
Twin_Ark_p1_CIplot_custom = Twin_Ark_p1_CIplot + ggtitle("Twin vs Arkansas SWSI") + xlab("beta SWSI values")
plot(Twin_Ark_p1_CIplot_custom)

#Twin - Plot both bootstrap plots for both river basins against eachother. Colorado on bottom. 
Twinp1result_boot_chart <- gridExtra::grid.arrange(Twin_Ark_p1_CIplot_custom, Twin_CO_p1_CIplot_custom, ncol=1)

# saving the plot as png 
ggsave("Twinp1result_boot.png", plot = Twinp1result_boot_chart, path = "results/graphs/")

beep(sound = 5)