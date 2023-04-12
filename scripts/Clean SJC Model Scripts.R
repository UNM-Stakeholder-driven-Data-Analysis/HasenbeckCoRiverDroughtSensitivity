#### read me ####

# This script prepares data for linear modeling and then creates a comparison of a few different model structures. 

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



#### load data and format date/time ####

SWSI = read.csv("data/processed/SWSI1981to2023.csv", header = T)
SWSI$Date = as.Date.character(SWSI$Date, format = "%Y-%m-%d")

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

# check for duplicate date/time stamps
anyDuplicated(SWSI$Date)
# check percentage of dataset with NAs 
sum(is.na(SWSI))/nrow(SWSI)*100
#No NAs! 


#### Azotea - Create time series and remove seasonality ####
## load data and format date/time ##
AzoteaDiversions <- read_csv(file = "data/processed/AzoteaMonthlyDischarge")

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
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling

# Make univariate zoo time series #
ts.temp<-read.zoo(AzoteaDiversions, index.column=2, format="%Y-%m-%d")

# ‘order.by’ are not unique warning suggests duplicate time stamps. I found that this is due to time zone changes, so nothing to worry about for regular time steps. 
plot(ts.temp)
# Apply NA interpolation method: Using max gap of 7 days 
Azotea_filled = na.spline(ts.temp, na.rm = T, maxgap = 7)
plot(Azotea_filled)

#Converting negative values to 0. 
par(mfrow=c(1,1)) # reset plotting window
# revert back to df
Azotea_filled = as.data.frame(Azotea_filled)
Azotea_filled$Date = AzoteaDiversions$Date
names(Azotea_filled) = c(colnames(AzoteaDiversions)[1],colnames(AzoteaDiversions)[2])
Azotea_filled = Azotea_filled %>% dplyr::select(Discharge, Date)

#some interpolated values went negative. Replace negative values with 0.
Azotea_filled$Discharge[Azotea_filled$Discharge < 0] = 0 

# check NAs that are left
sum(is.na(Azotea_filled$Date))
#No more NAs. 

#prepping to remove seasonality: 

#set df:
Discharge_data <- Azotea_filled

#create timeseries 
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

AzoteaDecomp <- Discharge_data_DEs

#### Heron - Create time series and remove seasonality ####
## load data and format date/time ##
HeronReleases <- read_csv(file = "data/processed/HeronMonthlyReleases") %>%
  rename("Discharge" = "Release")

HeronReleases$Date = as.Date(HeronReleases$Date, "%y-%m-%d")

## prep time series ##
sum(is.na(HeronReleases$Date))
#No NAs

sum(is.na(HeronReleases$Discharge))
#3 NAs

# check percentage of dataset with NAs - this is important to document!
sum(is.na(HeronReleases))/nrow(HeronReleases)*100
#1.6%

## fill gaps with spline interpolation ##
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling

# Make univariate zoo time series #
ts.temp<-read.zoo(HeronReleases, index.column=2, format="%Y-%m-%d")
plot(ts.temp)

# Apply NA interpolation method
Heron_filled = na.spline(ts.temp, na.rm = T, maxgap = 7)
plot(Heron_filled)

par(mfrow=c(1,1)) # reset plotting window

# revert back to df
Heron_filled = as.data.frame(Heron_filled)
Heron_filled$Date = HeronReleases$Date
names(Heron_filled) = c(colnames(HeronReleases)[1],colnames(HeronReleases)[2])
Heron_filled = Heron_filled %>% dplyr::select(Discharge, Date)
Heron_filled$Discharge[Heron_filled$Discharge < 0] = 0 #some interpolated values are negative. Turn these into 0s. 
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

ggplot(Heron_filled, aes(x=Date, y=Discharge))+
  geom_path() + geom_point() + theme_bw()

HeronDecomp <- Discharge_data_DEs

#### CO SWSI Prep for modeling ####
## load data and format date/time ##
SWSI_CO <- read_csv(file = "data/processed/SWSI1981to2023.csv") %>%
  dplyr::select(Date,Colorado) %>%
  rename("SWSI_values" = "Colorado")

SWSI_CO$Date = as.Date(SWSI_CO$Date, "%y-%m-%d")


#### RG SWSI - Prep for modeling ####
## load data and format date/time ##
SWSI_RG <- read_csv(file = "data/processed/SWSI1981to2023.csv") %>%
  dplyr::select(Date,Rio_Grande) %>%
  rename("SWSI_values" = "Rio_Grande")

SWSI_RG$Date = as.Date(SWSI_RG$Date, "%y-%m-%d")

####Azotea - CO SWSI linear model w seasonal correction on Azotea data   ####
##Candidates p1q1, p0q2 (<-these two had best BIC comparison), p3, p1q2
Azotea_Decomp_CO_SWSI_Raw <- full_join(AzoteaDecomp,SWSI_CO, by = "Date")  #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI
Azotea_Decomp_CO_SWSI_Raw$Discharge = as.numeric(Azotea_Decomp_CO_SWSI_Raw$Discharge)
Azotea_Decomp_CO_SWSI_Raw$SWSI_values = as.numeric(Azotea_Decomp_CO_SWSI_Raw$SWSI_values)

#POR for Azotea data is older than for SWSI. Remove dates where there are no SWSI values. 
Azotea_Decomp_CO_SWSI_Raw <- 
  filter(Azotea_Decomp_CO_SWSI_Raw, Date >= "1981-06-01", Date <= "2022-08-01")  

CombinedData <- Azotea_Decomp_CO_SWSI_Raw

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
#first number is autoregressive coef 1
# middle is differencing data 0
# last number is moving average term 1

# fit AR(1) regression model with SWSI as a predictor
mod_Ar1 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corAR1(), method="ML")

# fit some other candidate structures
mod_AMRAp1q1 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=1,q=1), method="ML")
mod_AMRAp2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=2), method="ML")
mod_AMRAp3 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=3), method="ML")
mod_AMRAp0q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=0,q=2), method="ML") 
mod_AMRAp1q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=1,q=2), method="ML") 
mod_AMRAp2q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=2,q=2), method="ML") 
#p = regressive order, #q is moving average order #41:40#p = regressive order, #q is moving average order #41:40


# compare models with BIC
bbmle::BICtab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp1q2,mod_AMRAp2q2)
bbmle::AICtab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp1q2)

# dBIC df
# mod_AMRAp1q1  0.0 5 
# mod_AMRAp0q2  0.3 5 
# mod_AMRAp2    1.2 5 
# mod_AMRAp3    5.9 6 
# mod_AMRAp1q2  8.5 6 
# mod_AMRAp2q2 12.1 7 
# mod_Ar1      20.1 4 

summary(mod_AMRAp2q2)



# intervals() for nlme is equivelant to confint() for lm
intervals(mod_AMRAp2q2)


par(mfrow=c(1,1))
visreg(mod_AMRAp2q2,"SWSI_values")

Acf(resid(mod_AMRAp2q2))


# extract and assess residuals: AMRAp1q2. Chosing this one because it best satisfies autocorrelation. 
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp1q2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp1q2 model residuals")
plot(resid(mod_AMRAp1q2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSAMRAp1q2 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp1q2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp1q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp1q2, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp1q2, type = "normalized"))

Acf(resid(mod_AMRAp1q2))

summary(mod_AMRAp1q2)

# #Generalized least squares fit by maximum likelihood
# Model: Discharge ~ SWSI_values 
# Data: CombinedData 
# AIC      BIC    logLik
# 10163.26 10188.67 -5075.632
# 
# Correlation Structure: ARMA(1,2)
# Formula: ~1 
# Parameter estimate(s):
#   Phi1     Theta1     Theta2 
# 0.5908290 -0.3082652  0.5329362 
# 
# Coefficients:
#            Value.   Std.Error    t-value p-value
# (Intercept) 3526.536  434.4791 8.116700  0.0000
# SWSI_values  259.312  155.2671 1.670105  0.0955
# 
# Correlation: 
#   (Intr)
# SWSI_values -0.069
# 
# Standardized residuals:
#   Min         Q1        Med         Q3        Max 
# -0.7584343 -0.4978346 -0.2583953  0.1479463 12.5340680 
# 
# Residual standard error: 6017.378 
# Degrees of freedom: 510 total; 508 residual


####Azotea - RG SWSI linear model w seasonal correction on Azotea data   ####
#p1q2
Azotea_Decomp_RG_SWSI_Raw <- full_join(AzoteaDecomp,SWSI_RG, by = "Date")  #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI
Azotea_Decomp_RG_SWSI_Raw$Discharge = as.numeric(Azotea_Decomp_RG_SWSI_Raw$Discharge)
Azotea_Decomp_RG_SWSI_Raw$SWSI_values = as.numeric(Azotea_Decomp_RG_SWSI_Raw$SWSI_values)

#POR for Azotea data is older than for SWSI. Remove dates where there are no SWSI values. 
Azotea_Decomp_RG_SWSI_Raw <- 
  filter(Azotea_Decomp_RG_SWSI_Raw, Date >= "1981-06-01", Date <= "2022-08-01")  

CombinedData <- Azotea_Decomp_RG_SWSI_Raw

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
#first number is autoregressive coef 1
# middle is differencing data 0
# last number is moving average term 1

# fit  regression model with SWSI as a predictor
mod_AMRAp1q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=1,q=2), method="ML") 
#p = regressive order, #q is moving average order #41:40#p = regressive order, #q is moving average order #41:40

summary(mod_AMRAp1q2)



# intervals() for nlme is equivelant to confint() for lm
intervals(mod_AMRAp1q2)


par(mfrow=c(1,1))
visreg(mod_AMRAp1q2,"SWSI_values")

Acf(resid(mod_AMRAp1q2))





# extract and assess residuals: AMRAp1q2. Chosing this one because it best satisfies autocorrelation. 
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp1q2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp1q2 model residuals")
plot(resid(mod_AMRAp1q2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSAMRAp1q2 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp1q2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp1q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp1q2, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp1q2, type = "normalized"))

Acf(resid(mod_AMRAp1q2))

summary(mod_AMRAp1q2)

# #Generalized least squares fit by maximum likelihood
# Model: Discharge ~ SWSI_values 
# Data: CombinedData 
# AIC      BIC    logLik
# 10163.26 10188.67 -5075.632
# 
# Correlation Structure: ARMA(1,2)
# Formula: ~1 
# Parameter estimate(s):
#   Phi1     Theta1     Theta2 
# 0.5908290 -0.3082652  0.5329362 
# 
# Coefficients:
#            Value.   Std.Error    t-value p-value
# (Intercept) 3526.536  434.4791 8.116700  0.0000
# SWSI_values  259.312  155.2671 1.670105  0.0955
# 
# Correlation: 
#   (Intr)
# SWSI_values -0.069
# 
# Standardized residuals:
#   Min         Q1        Med         Q3        Max 
# -0.7584343 -0.4978346 -0.2583953  0.1479463 12.5340680 
# 
# Residual standard error: 6017.378 
# Degrees of freedom: 510 total; 508 residual


#first number is autoregressive coef 1
# middle is differencing data 0
# last number is moving average term 1

# fit AR(1) regression model with SWSI as a predictor
mod_Ar1 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corAR1(), method="ML")

# fit some other candidate structures
mod_AMRAp1q1 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=1,q=1), method="ML")
mod_AMRAp2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=2), method="ML")
mod_AMRAp3 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=3), method="ML")
mod_AMRAp0q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=0,q=2), method="ML") 
mod_AMRAp1q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=1,q=2), method="ML") 
mod_AMRAp2q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=2,q=2), method="ML") 
#p = regressive order, #q is moving average order #41:40#p = regressive order, #q is moving average order #41:40


# compare models with BIC
bbmle::BICtab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp1q2,mod_AMRAp2q2)
bbmle::AICtab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp1q2)

# dBIC df
# mod_AMRAp1q1  0.0 5 
# mod_AMRAp0q2  0.3 5 
# mod_AMRAp2    1.2 5 
# mod_AMRAp3    5.9 6 
# mod_AMRAp1q2  8.5 6 
# mod_AMRAp2q2 12.1 7 
# mod_Ar1      20.1 4 

summary(mod_AMRAp2q2)



# intervals() for nlme is equivelant to confint() for lm
intervals(mod_AMRAp2q2)


par(mfrow=c(1,1))
visreg(mod_AMRAp2q2,"SWSI_values")

Acf(resid(mod_AMRAp2q2))


# extract and assess residuals: AMRAp1q2. Chosing this one because it best satisfies autocorrelation. 
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp1q2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp1q2 model residuals")
plot(resid(mod_AMRAp1q2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSAMRAp1q2 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp1q2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp1q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp1q2, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp1q2, type = "normalized"))

Acf(resid(mod_AMRAp1q2))
# 
# summary(mod_AMRAp1q2)
# Generalized least squares fit by maximum likelihood
# Model: Discharge ~ SWSI_values 
# Data: CombinedData 
# AIC      BIC    logLik
# 10163.05 10188.46 -5075.526
# 
# Correlation Structure: ARMA(1,2)
# Formula: ~1 
# Parameter estimate(s):
#   Phi1     Theta1     Theta2 
# 0.5786877 -0.2953304  0.5439725 
# 
# Coefficients:
#                Value Std.Error  t-value p-value
# (Intercept) 3537.598  430.1511 8.224082  0.0000
# SWSI_values  260.111  147.9731 1.757827  0.0794
# 
# Correlation: 
#   (Intr)
# SWSI_values -0.051
# 
# Standardized residuals:
#   Min         Q1        Med         Q3        Max 
# -0.7679689 -0.5145950 -0.2511092  0.1381228 12.4706044 
# 
# Residual standard error: 5995.103 
# Degrees of freedom: 510 total; 508 residual



####Heron - CO SWSI linear model w seasonal correction on Heron data  ####
Heron_Adjust_CO_SWSI_Raw <- full_join(Heron_filled,SWSI_CO, by = "Date")  #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI
Heron_Adjust_CO_SWSI_Raw$Discharge = as.numeric(Heron_Adjust_CO_SWSI_Raw$Discharge)
Heron_Adjust_CO_SWSI_Raw$SWSI_values = as.numeric(Heron_Adjust_CO_SWSI_Raw$SWSI_values)

#POR for discharge data is older than for SWSI. Remove dates where there are no SWSI values. 
Heron_Adjust_CO_SWSI_Raw <- 
  filter(Heron_Adjust_CO_SWSI_Raw, Date >= "2008-01-01", Date <= "2022-08-01")  

CombinedData <- Heron_Adjust_CO_SWSI_Raw

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
#first number is autoregressive coef 2
# middle is differencing data 0
# last number is moving average term 2

# fit AR(1) regression model with SWSI as a predictor
mod_Ar1 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corAR1(), method="ML")

# fit some other candidate structures
mod_AMRAp1q1 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=1,q=1), method="ML")
mod_AMRAp2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=2), method="ML")
mod_AMRAp3 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=3), method="ML")
mod_AMRAp0q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=0,q=2), method="ML") 
#doesn't run mod_AMRAp1q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=1,q=2), method="ML") 
mod_AMRAp2q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=2,q=2), method="ML") 
#p = regressive order, #q is moving average order #41:40#p = regressive order, #q is moving average order #41:40


# compare models with AIC, AICc, and BIC
# For small data, use AICc – the small sample correction which provides greater penalty for each parameter but approaches AIC as n becomes large. If it makes a difference, you should use it. 
# For large data and especially time series data, consider BIC. BIC is better in situations where a false positive is more misleading than a false negative. Remember that false positives are more common with time series. 
bbmle::AICtab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp1q2,mod_AMRAp2q2)
bbmle::AICctab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp1q2,mod_AMRAp2q2)
bbmle::BICtab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp1q2,mod_AMRAp2q2)

#BIC result:
#dAIC   df
#mod_AMRAp2q2    0.0 7 
#mod_AMRAp1q1    1.7 5 
#mod_AMRAp0q2    1.8 5 
#mod_AMRAp2      3.6 5 
#mod_AMRAp3      5.4 6 
#mod_Ar1         8.0 4 
#mod_AMRAp1q2 6387.3 6 

summary(mod_AMRAp2q2)



# intervals() for nlme is equivelant to confint() for lm
intervals(mod_AMRAp2q2)


par(mfrow=c(1,1))
visreg(mod_AMRAp2q2,"SWSI_values")

Acf(resid(mod_AMRAp2q2))

# extract and assess residuals: AMRAp1q1
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp1q1, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS AMRAp1q1model residuals")
plot(resid(mod_AMRAp1q1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main=" Discharge adjusted, raw SWSI GLS AMRAp1q1model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp1q1, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS AMRAp1q1model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp1q1, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp1q1, type = "normalized"))



# extract and assess residuals: AMRAp0q2
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp0q2, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS AMRAp0q2 model residuals")
plot(resid(mod_AMRAp0q2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main=" Discharge adjusted, raw SWSI GLS AMRAp0q2 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp0q2, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS AMRAp0q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp0q2, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp0q2, type = "normalized"))

# extract and assess residuals: #mod_AMRAp2 
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp2, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS AMRAp2 model residuals")
plot(resid(mod_AMRAp2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main=" Discharge adjusted, raw SWSI GLS AMRAp2 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp2, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS AMRAp2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp2, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp2, type = "normalized"))



# extract and assess residuals: mod_AMRAp3 
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp3, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS AMRAp3 model residuals")
plot(resid(mod_AMRAp3, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main=" Discharge adjusted, raw SWSI GLS AMRAp3 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp3, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS AMRAp3 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp3, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp3, type = "normalized"))


# extract and assess residuals: AMRAp1q2: 
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp1q2, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS AMRAp1q2 model residuals")
plot(resid(mod_AMRAp1q2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main=" Discharge adjusted, raw SWSI GLS AMRAp1q2 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp1q2, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS AMRAp1q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp1q2, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp1q2, type = "normalized"))



# extract and assess residuals: AMRAp2q2: 
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp2q2, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS AMRAp2q2 model residuals")
plot(resid(mod_AMRAp2q2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main=" Discharge adjusted, raw SWSI GLS AMRAp2q2 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp2q2, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS AMRAp2q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp2q2, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp2q2, type = "normalized"))




# extract and assess residuals: Ar1
par(mfrow=c(1,3))
Acf(resid(mod_Ar1, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS Ar1model residuals")
plot(resid(mod_Ar1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main=" Discharge adjusted, raw SWSI GLS Ar1model residuals"); abline(h=0)
qqnorm(resid(mod_Ar1, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS Ar1model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_Ar1, type = "normalized"))$statistic,2))); qqline(resid(mod_Ar1, type = "normalized"))



