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


## Azotea ## 
AzoteaDiversions <- read_csv(file = "data/processed/AzoteaMonthlyDischarge")
AzoteaDiversions$Date = as.Date(AzoteaDiversions$Date, "%y-%m-%d")

##Heron ## 
HeronReleases <- read_csv(file = "data/processed/HeronMonthlyReleases") %>%
  rename("Discharge" = "Release") #using the same column name as in diversion data to simplify replication

HeronReleases$Date = as.Date(HeronReleases$Date, "%y-%m-%d")

#### Azotea - Create time series and remove seasonality ####

## prep time series ##
sum(is.na(AzoteaDiversions$Date))
#No NAs

sum(is.na(AzoteaDiversions$Discharge))
#46 NAs

# check percentage of dataset with NAs 
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
?na.spline
par(mfrow=c(1,1))
hist(AzoteaDiversions$Discharge, breaks = 100)

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
## prep time series ##

##data explore## 
hist(HeronReleases$Discharge, breaks = 100)


sum(is.na(HeronReleases$Date))
#No NAs

sum(is.na(HeronReleases$Discharge))
#3 NAs

# check percentage of dataset with NAs 
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
SWSI_CO <- SWSI %>%
  dplyr::select(Date, Colorado) %>%
  rename("SWSI_values" = "Colorado") %>% 
  group_by(Date) %>%
  summarize(SWSI_values=mean(SWSI_values)) #Some dates have two entries. Avg the duplicates here. 

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
  summarize(SWSI_values=mean(SWSI_values)) #Some dates have two entries. Avg the duplicates here. 

# check for duplicate date/time stamps
anyDuplicated(SWSI_RG$Date)

# check percentage of dataset with NAs 
sum(is.na(SWSI_RG))/nrow(SWSI_RG)*100
#No NAs! 

####Azotea - CO SWSI linear model w seasonal correction on Azotea data - ARIMA model   ####

Azotea_Decomp_CO_SWSI_Raw <- full_join(AzoteaDecomp,SWSI_CO, by = "Date")  #Combining SWSI by basin with diversion data, Azotea Tunnel, CO SWSI
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
#mod_Ar1 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corAR1(), method="ML")

# fit some other candidate structures - run these if needed
#mod_AMRAp1q1 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=1,q=1), method="ML")
#mod_AMRAp2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=2), method="ML")
mod_AMRAp3 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=3), method="ML") #selected structure
#mod_AMRAp0q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=0,q=2), method="ML") 
#mod_AMRAp1q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=1,q=2), method="ML") 
#mod_AMRAp2q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=2,q=2), method="ML") 
#p = regressive order, #q is moving average order #41:40#p = regressive order, #q is moving average order #41:40


# compare models with BIC
bbmle::BICtab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp1q2,mod_AMRAp2q2)
bbmle::AICtab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp1q2)
bbmle::pAICtab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp1q2)
# dBIC df
# mod_AMRAp1q1  0.0 5 ****
# mod_AMRAp0q2  0.3 5 
# mod_AMRAp2    1.2 5 
# mod_AMRAp3    5.9 6 **** 
# mod_AMRAp1q2  8.5 6 
# mod_AMRAp2q2 12.1 7 
# mod_Ar1      20.1 4 

summary(mod_AMRAp2q2)



# intervals() for nlme is equivelant to confint() for lm
intervals(mod_AMRAp2q2)


par(mfrow=c(1,1))
visreg(mod_AMRAp2q2,"SWSI_values")

Acf(resid(mod_AMRAp2q2))

# AMRAp0q2, P2 has too much autocorrelation


# extract and assess residuals: AMRAp1q1. p = 0.09
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp1q1, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp1q1 model residuals")
plot(resid(mod_AMRAp1q1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSAMRAp1q1 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp1q1, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp1q1 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp1q1, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp1q1, type = "normalized"))
par(mfrow=c(1,1))
Acf(resid(mod_AMRAp1q1))
summary(mod_AMRAp1q1)

# extract and assess residuals: AMRAp1q2. Chosing this one because it best satisfies autocorrelation. p = .1095
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp1q2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp1q2 model residuals")
plot(resid(mod_AMRAp1q2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSAMRAp1q2 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp1q2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp1q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp1q2, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp1q2, type = "normalized"))
par(mfrow=c(1,1))
Acf(resid(mod_AMRAp1q2))

visreg(mod_AMRAp1q2,"SWSI_values")
summary(mod_AMRAp1q2)


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

Azotea_CO_P3 <- mod_AMRAp3



#Plot result 
visreg(Azotea_CO_P3, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Discharge") +
  ggtitle("Azotea Diversions by Colorado SWSI")


# saving the plot as png 
ggsave("AzoteaCOP3result.png", path = "results/graphs/")



# Generalized least squares fit by maximum likelihood
# Model: Discharge ~ SWSI_values 
# Data: CombinedData 
# AIC      BIC    logLik
# 9877.753 9902.981 -4932.877
# 
# Correlation Structure: ARMA(1,2)
# Formula: ~1 
# Parameter estimate(s):
#   Phi1     Theta1     Theta2 
# 0.5879469 -0.3084663  0.5311136 
# 
# Coefficients:
#   Value Std.Error  t-value p-value
# (Intercept) 3551.910  441.1508 8.051464  0.0000
# SWSI_values  262.156  163.0104 1.608219  0.1084
# 
# Correlation: 
#   (Intr)
# SWSI_values -0.045
# 
# Standardized residuals:
#   Min         Q1        Med         Q3        Max 
# -0.7571319 -0.4974943 -0.2594877  0.1457208 12.4093990 
# 
# Residual standard error: 6076.267 
# Degrees of freedom: 495 total; 493 residual


####Azotea - RG SWSI linear model w seasonal correction on Azotea data - ARIMA model   ####

Azotea_Decomp_RG_SWSI_Raw <- full_join(AzoteaDecomp,SWSI_RG, by = "Date")  #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI

#POR for Azotea data is older than for SWSI. Remove dates where there are no SWSI values. 
Azotea_Decomp_RG_SWSI_Raw <- 
  filter(Azotea_Decomp_RG_SWSI_Raw, Date >= "1981-06-01", Date <= "2022-08-01") 
Azotea_Decomp_RG_SWSI_Raw$Discharge = as.numeric(Azotea_Decomp_RG_SWSI_Raw$Discharge)
Azotea_Decomp_RG_SWSI_Raw$SWSI_values = as.numeric(Azotea_Decomp_RG_SWSI_Raw$SWSI_values)

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
#mod_AMRAp1q1 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=1,q=2), method="ML") 
#mod_AMRAp1q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=1,q=2), method="ML") 
mod_AMRAp3 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=3), method="ML")
#p = regressive order, #q is moving average order #41:40#p = regressive order, #q is moving average order #41:40

summary(mod_AMRAp1q2)



# intervals() for nlme is equivelant to confint() for lm
intervals(mod_AMRAp1q2)


par(mfrow=c(1,1))
visreg(mod_AMRAp1q2,"SWSI_values")

Acf(resid(mod_AMRAp1q2))


# extract and assess residuals: AMRAp1q1. p = 0.0673
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp1q1, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp1q1 model residuals")
plot(resid(mod_AMRAp1q1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSAMRAp1q1 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp1q1, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp1q1 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp1q1, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp1q1, type = "normalized"))
par(mfrow=c(1,1))
Acf(resid(mod_AMRAp1q1))
summary(mod_AMRAp1q1)

# extract and assess residuals: AMRAp1q2. p = 0.673
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp1q2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp1q2 model residuals")
plot(resid(mod_AMRAp1q2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSAMRAp1q2 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp1q2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp1q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp1q2, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp1q2, type = "normalized"))
par(mfrow=c(1,1))
Acf(resid(mod_AMRAp1q2))

visreg(mod_AMRAp1q2,"SWSI_values")
summary(mod_AMRAp1q2)


# extract and assess residuals: AMRAp3. p = 0.559
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp3, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp3 model residuals")
plot(resid(mod_AMRAp3, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSAMRAp3 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp3, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp3 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp3, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp3, type = "normalized"))
par(mfrow=c(1,1))
Acf(resid(mod_AMRAp3))
summary(mod_AMRAp3)
visreg(mod_AMRAp3)

Azotea_RG_P3 <- mod_AMRAp3


#Plot result 
Azotea_RG_P3_plot <- visreg(Azotea_RG_P3, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Dsicharge") +
  ggtitle("Azotea Diversions by Rio Grande SWSI")

Azotea_RG_P3_plot
# saving the plot as png 
ggsave("AzoteaRGP3result.png", plot = Azotea_RG_P3_plot, path = "results/graphs/")



# Generalized least squares fit by maximum likelihood
# Model: Discharge ~ SWSI_values 
# Data: CombinedData 
# AIC      BIC    logLik
# 9877.064 9902.291 -4932.532
# 
# Correlation Structure: ARMA(1,2)
# Formula: ~1 
# Parameter estimate(s):
#   Phi1     Theta1     Theta2 
# 0.5756965 -0.2958421  0.5413013 
# 
# Coefficients:
#                  Value Std.Error  t-value p-value
# (Intercept) 3538.612  436.7897 8.101408  0.0000
# SWSI_values  278.363  151.4172 1.838384  0.0666
# 
# Correlation: 
#   (Intr)
# SWSI_values -0.056
# 
# Standardized residuals:
#   Min         Q1        Med         Q3        Max 
# -0.7736897 -0.5098446 -0.2519243  0.1300213 12.3585083 
# 
# Residual standard error: 6048.808 
# Degrees of freedom: 495 total; 493 residual


#first number is autoregressive coef 1
# middle is differencing data 0
# last number is moving average term 1

####Azotea - RG SWSI linear model w seasonal correction on Azotea data - ARIMA model 
####Heron - CO SWSI linear model w seasonal correction on Heron data  ####
Heron_Decomp_CO_SWSI_Raw <- full_join(HeronDecomp,SWSI_CO, by = "Date")  #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI
Heron_Decomp_CO_SWSI_Raw$Discharge = as.numeric(Heron_Decomp_CO_SWSI_Raw$Discharge)
Heron_Decomp_CO_SWSI_Raw$SWSI_values = as.numeric(Heron_Decomp_CO_SWSI_Raw$SWSI_values)

#POR for discharge data is older than for SWSI. Remove dates where there are no SWSI values. 
Heron_Decomp_CO_SWSI_Raw <- 
  filter(Heron_Decomp_CO_SWSI_Raw, Date >= "2008-07-01", Date <= "2022-08-01")  

CombinedData <- Heron_Decomp_CO_SWSI_Raw

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

# fit some other candidate structures
# mod_AMRAp1q1 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=1,q=1), method="ML")
# mod_AMRAp2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=2), method="ML")
# mod_AMRAp3 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=3), method="ML")
# mod_AMRAp0q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=0,q=2), method="ML") 
# mod_AMRAp1q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=1,q=2), method="ML") 
# mod_AMRAp2q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=2,q=2), method="ML") 
# #p = regressive order, #q is moving average order #41:40#p = regressive order, #q is moving average order #41:40


# compare models with AIC, AICc, and BIC
# For small data, use AICc – the small sample correction which provides greater penalty for each parameter but approaches AIC as n becomes large. If it makes a difference, you should use it. 
# For large data and especially time series data, consider BIC. BIC is better in situations where a false positive is more misleading than a false negative. Remember that false positives are more common with time series. 
bbmle::AICtab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp1q2,mod_AMRAp2q2)
bbmle::AICctab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp1q2,mod_AMRAp2q2)
bbmle::BICtab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp2q2)

# dAIC df
# mod_Ar1      0.0  4 
# mod_AMRAp0q2 0.7  5 
# mod_AMRAp1q1 0.8  5 
# mod_AMRAp2   1.0  5 
# mod_AMRAp3   2.5  6 
# mod_AMRAp1q2 2.8  6 
# mod_AMRAp2q2 4.4  7

# extract and assess residuals: ArP2 p = 0.59
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp2, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS AMRAp2model residuals")
plot(resid(mod_AMRAp2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main=" Discharge adjusted, raw SWSI GLS AMRAp2model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp2, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS AMRAp2model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp2, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp2, type = "normalized"))
summary(mod_AMRAp2)

# extract and assess residuals: ArP0q2 p = 0.59
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp0q2, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS AMRAp0q2model residuals")
plot(resid(mod_AMRAp0q2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main=" Discharge adjusted, raw SWSI GLS AMRAp0q2model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp0q2, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS AMRAp0q2model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp0q2, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp0q2, type = "normalized"))
summary(mod_AMRAp0q2)


# extract and assess residuals: ArP1Q1 p = 0.63
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp1q1, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS AMRAp1q1model residuals")
plot(resid(mod_AMRAp1q1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main=" Discharge adjusted, raw SWSI GLS AMRAp1q1model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp1q1, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS AMRAp1q1model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp1q1, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp1q1, type = "normalized"))
summary(mod_AMRAp1q1)


# extract and assess residuals: Ar1 p = 0.63
par(mfrow=c(1,3))
Acf(resid(mod_Ar1, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS Ar1model residuals")
plot(resid(mod_Ar1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main=" Discharge adjusted, raw SWSI GLS Ar1model residuals"); abline(h=0)
qqnorm(resid(mod_Ar1, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS Ar1model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_Ar1, type = "normalized"))$statistic,2))); qqline(resid(mod_Ar1, type = "normalized"))
summary(mod_Ar1)


Heron_CO_AR1 <- mod_Ar1

# Generalized least squares fit by maximum likelihood
# Model: Discharge ~ SWSI_values 
# Data: CombinedData 
# AIC      BIC    logLik
# 3669.752 3682.633 -1830.876
# 
# Correlation Structure: AR(1)
# Formula: ~1 
# Parameter estimate(s):
#   Phi 
# 0.4640426 
# 
# Coefficients:
#   Value Std.Error   t-value p-value
# (Intercept) 4411.575  659.4013  6.690274  0.0000
# SWSI_values  -51.570  237.4815 -0.217156  0.8283
# 
# Correlation: 
#   (Intr)
# SWSI_values 0     
# 
# Standardized residuals:
#   Min         Q1        Med         Q3        Max 
# -0.8456439 -0.7055374 -0.2797005  0.2994502  6.2544986 
# 
# Residual standard error: 5422.338 
# Degrees of freedom: 185 total; 183 residual


####Heron - RG SWSI linear model w seasonal correction on Heron data ####
Heron_Decomp_RG_SWSI_Raw <- full_join(HeronDecomp,SWSI_RG, by = "Date")  #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI
Heron_Decomp_RG_SWSI_Raw$Discharge = as.numeric(Heron_Decomp_RG_SWSI_Raw$Discharge)
Heron_Decomp_RG_SWSI_Raw$SWSI_values = as.numeric(Heron_Decomp_RG_SWSI_Raw$SWSI_values)

#POR for discharge data is older than for SWSI. Remove dates where there are no SWSI values. 
Heron_Decomp_RG_SWSI_Raw <- 
  filter(Heron_Decomp_RG_SWSI_Raw, Date >= "2008-07-01", Date <= "2022-08-01")  

CombinedData <- Heron_Decomp_RG_SWSI_Raw


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
# mod_AMRAp1q1 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=1,q=1), method="ML")
# mod_AMRAp2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=2), method="ML")
# mod_AMRAp3 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=3), method="ML")
# mod_AMRAp0q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=0,q=2), method="ML") 
# #mod_AMRAp1q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=1,q=2), method="ML") 
# mod_AMRAp2q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=2,q=2), method="ML") 
# #p = regressive order, #q is moving average order #41:40#p = regressive order, #q is moving average order #41:40


# compare models with AIC, AICc, and BIC
# For small data, use AICc – the small sample correction which provides greater penalty for each parameter but approaches AIC as n becomes large. If it makes a difference, you should use it. 
# For large data and especially time series data, consider BIC. BIC is better in situations where a false positive is more misleading than a false negative. Remember that false positives are more common with time series. 
bbmle::AICtab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp1q2,mod_AMRAp2q2)
bbmle::AICctab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp1q2,mod_AMRAp2q2)
bbmle::BICtab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp2q2)


# extract and assess residuals: Ar1 p =0.0044
par(mfrow=c(1,3))
Acf(resid(mod_Ar1, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS Ar1model residuals")
plot(resid(mod_Ar1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main=" Discharge adjusted, raw SWSI GLS Ar1model residuals"); abline(h=0)
qqnorm(resid(mod_Ar1, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS Ar1model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_Ar1, type = "normalized"))$statistic,2))); qqline(resid(mod_Ar1, type = "normalized"))
summary(mod_Ar1)

# extract and assess residuals: ArP2 p = 0.0045
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp2, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS AMRAp2model residuals")
plot(resid(mod_AMRAp2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main=" Discharge adjusted, raw SWSI GLS AMRAp2model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp2, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS AMRAp2model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp2, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp2, type = "normalized"))
summary(mod_AMRAp2)

# extract and assess residuals: ArP0q2 p = 0.0046
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp0q2, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS AMRAp0q2model residuals")
plot(resid(mod_AMRAp0q2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main=" Discharge adjusted, raw SWSI GLS AMRAp0q2model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp0q2, type = "normalized"), main=" Discharge adjusted, raw SWSI GLS AMRAp0q2model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp0q2, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp0q2, type = "normalized"))
summary(mod_AMRAp0q2)

par(mfrow=c(1,1))
visreg(mod_Ar1)

summary(mod_Ar1)

Heron_RG_AR1 <- mod_Ar1


#Plot result 
visreg(Heron_RG_AR1, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Outflow Discharge") +
  ggtitle("Heron Outflows by Rio Grande SWSI")


# saving the plot as png 
ggsave("Heron_RG_AR1result.png", path = "results/graphs/")

# # Generalized least squares fit by maximum likelihood
# Model: Discharge ~ SWSI_values 
# Data: CombinedData 
# AIC      BIC    logLik
# 3372.106 3384.649 -1682.053
# 
# Correlation Structure: AR(1)
# Formula: ~1 
# Parameter estimate(s):
#   Phi 
# 0.3848703 
# 
# Coefficients:
#   Value Std.Error   t-value p-value
# (Intercept) 4108.739  604.6264  6.795500  0.0000
# SWSI_values -759.013  263.1307 -2.884548  0.0044
# 
# Correlation: 
#   (Intr)
# SWSI_values 0.136 
# 
# Standardized residuals:
#   Min         Q1        Med         Q3        Max 
# -1.3274709 -0.6337984 -0.2557167  0.2739218  6.2644673 
# 
# Residual standard error: 5193.573 
# Degrees of freedom: 170 total; 168 residual

####Plot models #### 

visreg(Heron_RG_AR1, "SWSI_values", gg = T) +
  theme(axis.line = element_line(colour = "black")) +
  xlab("SWSI Values") +
  ylab("Predicted Dsicharge") +
  ggtitle("Heron Outlows by Rio Grande SWSI")

