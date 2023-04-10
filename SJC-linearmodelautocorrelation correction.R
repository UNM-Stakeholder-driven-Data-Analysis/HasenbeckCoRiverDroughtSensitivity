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
View(SWSI)
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

# chec for duplicate date/time stamps
anyDuplicated(SWSI$Date)
# check percentage of dataset with NAs - this is important to document!
sum(is.na(SWSI))/nrow(SWSI)*100
#No NAs! 

#### create time series ####

# need to do this to prep for removing seasonality

SWSI_ts = ts(SWSI$SWSI_values, start = c(1981-06-01), frequency = 12)
head(SWSI)

par(mfrow=c(1,1))
plot(SWSI)

#### remove seasonality ####

# examine seasonality
par(mfrow=c(3,1))
plot(SWSI_ts)
Acf(SWSI_ts)
Pacf(SWSI_ts)

# decompose into additive components
plot(decompose(SWSI_ts))
# decompose into multiplicative components
plot(decompose(SWSI_ts, type="multiplicative"))
# extract components from multiplicative
SWSI_ts_decomp = decompose(SWSI_ts, type="multiplicative")
SWSI_ts_trend = SWSI_ts_decomp$trend
SWSI_ts_remainder = SWSI_ts_decomp$random
# save de-seasoned ts
SWSI_ts_DEs = SWSI_ts_trend * SWSI_ts_remainder

# compare original to de-seasoned ts
par(mfrow=c(3,2))
plot(SWSI_ts)
plot(SWSI_ts_DEs)
Acf(SWSI_ts)
Acf(SWSI_ts_DEs)
Pacf(SWSI_ts)
Pacf(SWSI_ts_DEs)
View(SWSI_DEs)
# revert back to df
SWSI_DEs = as.data.frame(SWSI_ts_DEs)
SWSI_DEs$Date = SWSI$Date
names(SWSI_DEs) = c("SWSI_values","Date")
SWSI_DEs = SWSI_DEs %>% dplyr::select(Date, SWSI_values) %>% arrange(Date)
SWSI_DEs = na.trim(SWSI_DEs, "both")

ggplot(SWSI_DEs, aes(x=Date, y=SWSI_values, color = ))+
  geom_path() + geom_point() + theme_bw()


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
# for calculating long-term trends, you can be pretty liberal with how large of gaps you fill. However, if you go too big with a spline interpolation, you'll get wacky results (ALWAYS EXAMINE THE RESULTS OF GAP FILLING!!). To strike this balance, I'm filling gaps of up to five days here. 
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling

# Make univariate zoo time series #
ts.temp<-read.zoo(AzoteaDiversions, index.column=2, format="%Y-%m-%d")

# ‘order.by’ are not unique warning suggests duplicate time stamps. I found that this is due to time zone changes, so nothing to worry about for regular time steps. 
plot(ts.temp)
# Apply NA interpolation method
Azotea_filled = na.spline(ts.temp, na.rm = T, maxgap = 7)
plot(Azotea_filled)

#Converting negative values to 0. 
par(mfrow=c(1,1)) # reset plotting window
# revert back to df
Azotea_filled = as.data.frame(Azotea_filled)
Azotea_filled$Date = AzoteaDiversions$Date
names(Azotea_filled) = c(colnames(AzoteaDiversions)[1],colnames(AzoteaDiversions)[2])
Azotea_filled = Azotea_filled %>% dplyr::select(Discharge, Date)
Azotea_filled$Discharge[Azotea_filled$Discharge < 0] = 0 #replace negative extrapolated values with 0.

# check NAs that are left
sum(is.na(Azotea_filled$Date))
#No more NAs. 

# need to do this to prep for removing seasonality
#set df:
Discharge_data <- Azotea_filled

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


par(mfrow=c(1,3))

ggplot(Discharge_data_DEs, aes(x=Date, y=Discharge))+
    geom_path() + geom_point() + theme_bw() + ggtitle("Seasonally Adjusted Azotea")

Azotea_Corrected <-Discharge_data_DEs

ggplot(AzoteaDiversions, aes(x=Date, y=Discharge))+
  geom_path() + geom_point() + theme_bw() + ggtitle("Raw Azotea")

#### Heron - Create time series and remove seasonality ####
## load data and format date/time ##
HeronReleases <- read_csv(file = "data/processed/HeronMonthlyReleases") %>%
  rename("Discharge" = "Release")
View(HeronReleases)

HeronReleases$Date = as.Date(HeronReleases$Date, "%y-%m-%d")

## prep time series ##
sum(is.na(HeronReleases$Date))
#No NAs

sum(is.na(HeronReleases$Discharge))
#2 NAs

# check percentage of dataset with NAs - this is important to document!
sum(is.na(HeronReleases))/nrow(HeronReleases)*100
#1%

## fill gaps with spline interpolation ##
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling

# Make univariate zoo time series #
ts.temp<-read.zoo(HeronReleases, index.column=2, format="%Y-%m-%d")

# ‘order.by’ are not unique warning suggests duplicate time stamps. I found that this is due to time zone changes, so nothing to worry about for regular time steps. 
plot(ts.temp)
# Apply NA interpolation method
Heron_filled = na.spline(ts.temp, na.rm = T, maxgap = 7)
plot(Heron_filled)

#Converting negative values to 0. 
par(mfrow=c(1,1)) # reset plotting window
# revert back to df
Heron_filled = as.data.frame(Heron_filled)
Heron_filled$Date = HeronReleases$Date
names(Heron_filled) = c(colnames(HeronReleases)[1],colnames(HeronReleases)[2])
Heron_filled = Heron_filled %>% dplyr::select(Discharge, Date)
View(Heron_filled)
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

HeronCorrected <- Discharge_data_DEs

#### CO SWSI - Create time series and remove seasonality ####
## load data and format date/time ##
SWSI_CO <- read_csv(file = "data/processed/SWSI1981to2023.csv") %>%
  dplyr::select(Date,Colorado) %>%
  rename("SWSI_values" = "Colorado")

View(SWSI_CO)

SWSI_CO$Date = as.Date(SWSI_CO$Date, "%y-%m-%d")

## prep time series ##
sum(is.na(SWSI_CO$Date))
#No NAs

sum(is.na(SWSI_CO$SWSI_values))
#No NAs

# need to do this to prep for removing seasonality
#set df:
SWSI_data <- SWSI_CO

timeseries = ts(SWSI_data$SWSI_values, start = c(2008-01-01), frequency = 12)

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
SWSI_data_DEs = as.data.frame(timeseries_DEs)
SWSI_data_DEs$Date = SWSI_data$Date
names(SWSI_data_DEs) = c("SWSI_values","Date")
SWSI_data_DEs = SWSI_data_DEs %>% dplyr::select(Date, SWSI_values) %>% arrange(Date)
SWSI_data_DEs = na.trim(SWSI_data_DEs, "both")

ggplot(SWSI_data_DEs, aes(x=Date, y=SWSI_values))+
  geom_path() + geom_point() + theme_bw()

CO_SWSI_Corrected <- SWSI_data_DEs

#### RG SWSI - Create time series and remove seasonality ####
## load data and format date/time ##
SWSI_RG <- read_csv(file = "data/processed/SWSI1981to2023.csv") %>%
  dplyr::select(Date,Rio_Grande) %>%
  rename("SWSI_values" = "Rio_Grande")


SWSI_RG$Date = as.Date(SWSI_RG$Date, "%y-%m-%d")

## prep time series ##
sum(is.na(SWSI_RG$Date))
#No NAs

sum(is.na(SWSI_RG$SWSI_values))
#No NAs

# need to do this to prep for removing seasonality
#set df:
SWSI_data <- SWSI_RG

timeseries = ts(SWSI_data$SWSI_values, start = c(2008-01-01), frequency = 12)

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
SWSI_data_DEs = as.data.frame(timeseries_DEs)
SWSI_data_DEs$Date = SWSI_data$Date
names(SWSI_data_DEs) = c("SWSI_values","Date")
SWSI_data_DEs = SWSI_data_DEs %>% dplyr::select(Date, SWSI_values) %>% arrange(Date)
SWSI_data_DEs = na.trim(SWSI_data_DEs, "both")

ggplot(SWSI_data_DEs, aes(x=Date, y=SWSI_values))+
  geom_path() + geom_point() + theme_bw()

RG_SWSI_Corrected <- SWSI_data_DEs


###Azotea - CO SWSI - Linear model ####
Azotea_CO_SWSI <- 
  full_join(Azotea_Corrected,CO_SWSI_Corrected, by = "Date") %>%
  filter(Date >= "1981-12-01") %>% #No SWSI data before this date.
  filter(Date <= "2022-02-01") #No SWSI data after this date.

CombinedData <- Azotea_CO_SWSI
View(CombinedData)
## plot the data ###
CombinedData %>%
  ggplot(aes(x = Discharge, y = SWSI_values)) + 
  geom_point()


Azotea_CO_SWSI <- full_join(Azotea_Corrected,CO_SWSI_Corrected, by = "Date") %>% #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI
  filter(Azotea_CO_SWSI$Date >= "1981-12-01", Azotea_CO_SWSI$Date <= "2022-02-01")  #POR for Azotea data is older than for SWSI. Remove dates where there are no SWSI values. 

Azotea_CO_SWSI$Discharge <- as.numeric(Azotea_CO_SWSI$Discharge)
Azotea_CO_SWSI$SWSI_values <- as.numeric(Azotea_CO_SWSI$SWSI_values)

CombinedData <- Azotea_CO_SWSI
### linear trends ###

# add simple time steps to df
CombinedData$t = c(1:nrow(CombinedData))

mod = lm(Discharge ~ SWSI_values, CombinedData)

summary(mod)
#                         Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                3556.8      276.3  12.873   <2e-16 ***
# CombinedData$SWSI_values    246.2      122.1   2.017   0.0442 *

#Can't get the below code to work
visreg(mod,"SWSI_values")

confint(mod, "SWSI_values", level=0.95)
#               2.5 %   97.5 %
#SWSI_values 57.82108 628.5478

## diagnostics ##
Acf(resid(mod))
forecast::checkresiduals(mod)
#Breusch-Godfrey test for serial correlation of order up to 10
#data:  Residuals
#LM test = 150.13, df = 10, p-value < 2.2e-16

### test & calculate trends - nlme::gls ###

# see package manual: https://cran.r-project.org/web/packages/nlme/nlme.pdf

# ask auto.arima what it thinks the autocorrelation structure is
auto.arima(CombinedData$Discharge)
#first number is autoregressive coef 0
# middle is differencing data 0
# last number is moving average term 2

# fit AR(1) regression model with SWSI as a predictor
mod_Ar1 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corAR1(), method="ML")

# fit some other candidate structures
mod_AMRAp1q1 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=1,q=1), method="ML")
mod_AMRAp2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=2), method="ML")
mod_AMRAp3 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=3), method="ML")
mod_AMRAp0q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=0,q=2), method="ML") 
mod_AMRAp1q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=1,q=2), method="ML") 
#p = regressive order, #q is moving average order #41:40


# compare models with AIC, AICc, and BIC
# For small data, use AICc – the small sample correction which provides greater penalty for each parameter but approaches AIC as n becomes large. If it makes a difference, you should use it. 
# For large data and especially time series data, consider BIC. BIC is better in situations where a false positive is more misleading than a false negative. Remember that false positives are more common with time series. 
bbmle::AICtab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp1q2)
bbmle::AICctab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp1q2)
bbmle::BICtab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp1q2)

summary(mod_AMRAp1q1)

# intervals() for nlme is equivelant to confint() for lm
intervals(mod_AMRAp1q1)


par(mfrow=c(1,1))
visreg(mod_AMRAp1q1,"SWSI_values")

### Important notes about extracting residuals from model fits!! ###

# It's important to understand that many extraction fxns in R, such as residuals(modelfit) (same as resid(modelfit)), will detect the object type and call on methods from that package appropriate for that object. So, residuals(modelfit) is using different methods for different model types when the model package requires it, and you need to look up the options for these different methods.
# E.g., residuals(nlme model) calls residuals.lme(nlme model), which has different options than if you call residuals(model fit) on a different kind of model. 
# see ?residuals.gls for the methods avaiable for this model type
# type ?residuals. into your console and scroll through the options for other residuals methods for loaded packages

# For gls, you want to assess assumptions on normalized residuals, which is not an option for standard linear models.
# normalized residuals = standardized residuals pre-multiplied by the inverse square-root factor of the estimated error correlation matrix
# see https://stats.stackexchange.com/questions/80823/do-autocorrelated-residual-patterns-remain-even-in-models-with-appropriate-corre

Acf(resid(mod_AMRAp1q1))

# extract and assess residuals
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp1q1, type = "normalized"), main="GLS AR(1) model residuals")
plot(resid(mod_AMRAp1q1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="GLS AR(1) model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp1q1, type = "normalized"), main="GLS AR(1) model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp1q1, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp1q1, type = "normalized"))

# exctract parameter estimates for comparison with MARSS
mod_AMRAp1q1.phi = coef(mod_AMRAp1q1$modelStruct[[1]], unconstrained=FALSE)
ests.gls = c(b=mod_AMRAp1q1.phi, alpha=coef(mod_Ar1)[1],
             time=coef(mod_AMRAp1q1)[2],
             logLik=logLik(mod_AMRAp1q1))

### test & calculate trends - UARSS ###

## MARSS ##
# User;s guide: https://cran.r-project.org/web/packages/MARSS/vignettes/UserGuide.pdf
# Package manual: https://cran.r-project.org/web/packages/MARSS/MARSS.pdf
# Lectures: https://nwfsc-timeseries.github.io/atsa/
# Lab book: https://nwfsc-timeseries.github.io/atsa-labs/
# Quick start guide: https://cran.r-project.org/web/packages/MARSS/vignettes/Quick_Start.pdf
# MARSS is a extremely flexible ts modeling framework with a steep learning curve requiring lots of matrix algebra for many applications, so wade into these documents with caution and prepare to get overwhelmed quickly! Also the user's guide is written, in my opinion, pretty poorly for the average user. This class will give you a few common "recipes", but know that almost anything is possible if you take time to learn the modeling framework in full.


## MARSS equivelent to gls corAR1 model ##

## format response var y as a vector
dat = as.vector(CombinedData$SWSI_values)
# remove leading and trailing NAs #
dat = na.trim(dat, is.na="any")

## set model parameters - see powerpoint for 
mod.list.AMRAp1q1 = list(
  B=matrix("b"),          # state process model
  Q=matrix("q"),          # state process model
  U="zero",               # state process model
  C="zero",               # state process model
  Z="identity",           # observation process model
  R=matrix("r"),          # observation process model
  A=matrix("intercept"),  # observation process model
  D=matrix("SWSI_values"),       # observation process model
  d=matrix(c(1:length(dat)), nrow=1), # observation process model
  x0=matrix(dat[1]), 
  tinitx=0
)

## fit model
# mod.AMRAp1q1 <- MARSS(dat, model=mod.list.AMRAp1q1, method="BFGS")
mod.AMRAp1q1 <- MARSS(dat, model=mod.list.AMRAp1q1, control=list(maxit=10000))
beep(2)
# est.AMRAp1q1 <- MARSSparamCIs(mod.AMRAp1q1, method = "parametric", alpha = 0.05, nboot = 2000, silent=F)
est.AMRAp1q1 <- MARSSparamCIs(mod.AMRAp1q1, method = "hessian", alpha = 0.05)

## extract parameter estimates for comparison to gls
ests.marss = c(b=coef(mod.AMRAp1q1)$B, alpha=coef(mod.AMRAp1q1)$A,
               time=coef(mod.AMRAp1q1)$D[1],
               logLik=logLik(mod.AMRAp1q1))

## compare UARSS and gls results
# parameter estimates
ests.marss
ests.gls
# 95% CIs on trend estimate
intervals(mod_AMRAp1q1)
est.AMRAp1q1
# notice that UARSS provides a slightly narrower confidence interval

## test residuals for ac
# extract residuals
resids.1 <- residuals(mod.AMRAp1q1) # see ?residuals.marssMLE
# plot residuals
par(mfrow=c(2,3))
### Trend detection - This code doesnt work for me! Ts error! ###
Acf(resids.1$model.residuals[1,], main="Observation process model residuals")
plot(resids.1$model.residuals[1,]~c(1:length(dat)), main="Observation process model residuals"); abline(h=0)
qqnorm(resids.1$model.residuals[1,], main="Observation process model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resids.1$model.residuals[1,])$statistic,2))); qqline(resids.1$model.residuals[1,])
#
Acf(resids.1$state.residuals[1,], main="State process model residuals", na.action = na.pass)
plot(resids.1$state.residuals[1,]~c(1:length(dat)), main="State process model residuals"); abline(h=0)
qqnorm(resids.1$state.residuals[1,], main="Observation process model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resids.1$state.residuals[1,])$statistic,2))); qqline(resids.1$state.residuals[1,])


# MARSS is focused on explaining temporal dynamics and is not "willing" to shunt all remaining autocorrelation to error, unlike nlme options. If it can't be explained by an autocorrelated process or covars, it will retain the remaining autocorrelation. This is good information! But not often the most practical option if you can't include the right covars. 
# Here, this tells us that a trend over time in observation model is inadequate to capture all the systematic variation in Rio Grande discharge, suggests additional covars are needed.  


## Plot fitted values over observations ###
# extract MARSS results
kf=print(mod.AMRAp1q1, what="kfs") # Kalman filter and smoother output
# plot observed data (y)
par(mfrow=c(1,1),oma = c(0, 0, 2, 0))
plot(as.vector(dat) ~ CombinedData$Date, type="p", pch=19,
     main = "UARSS model predictions conditioned on all y",
     ylab = "Discharge (cfs)", xlab="")
# calc and plot predicted values
predicts = as.vector(kf$xtT) + 
  coef(mod.AMRAp1q1)$A[1] + 
  (as.vector(mod.AMRAp1q1[["model"]][["fixed"]][["d"]])* coef(mod.AMRAp1q1)$D[1])
lines(predicts ~ CombinedData$Date, col="blue",lwd=2) 
lines(CombinedData$Date, predicts-1.96*mod.AMRAp1q1$states.se,
      type="l",lwd=1,lty=2,col="blue")
lines(CombinedData$Date, predicts+1.96*mod.AMRAp1q1$states.se,
      type="l",lwd=1,lty=2,col="blue")
# calc and plot predicted values without trend
predicts.trendless = as.vector(kf$xtT) + 
  coef(mod.AMRAp1q1)$A[1] 
lines(predicts.trendless ~ CombinedData$Date, col="red",lwd=2) 
lines(CombinedData$Date, predicts.trendless-1.96*mod.AMRAp1q1$states.se,
      type="l",lwd=1,lty=2,col="red")
lines(CombinedData$Date, predicts.trendless+1.96*mod.AMRAp1q1$states.se,
      type="l",lwd=1,lty=2,col="red")
mtext("Fitted values over observations", outer = TRUE, cex = 1.5)
# dashed lines are 95% CIs, calculated from the standard error. The value of 1.96 is based on the fact that 95% of the area of a normal distribution is within 1.96 standard deviations of the mean.

# trend model predictions and non-trend model predictions start diverging ~1996
# red line indicates Rio Grande discharge if there weren't a long-term decline. 

# you can make a similar plot from nlme::gls results! go try and figure that out :)



####Azotea - CO SWSI linear model WITHOUT SEASONALITY CORRECTION ####
Azotea_CO_SWSI_NO_adjust <- full_join(Azotea_filled,SWSI_CO, by = "Date")  #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI
Azotea_CO_SWSI_NO_adjust$Discharge = as.numeric(Azotea_CO_SWSI_NO_adjust$Discharge)
Azotea_CO_SWSI_NO_adjust$SWSI_values = as.numeric(Azotea_CO_SWSI_NO_adjust$SWSI_values)

#POR for Azotea data is older than for SWSI. Remove dates where there are no SWSI values. 

Azotea_CO_SWSI_NO_adjust <- 
  filter(Azotea_CO_SWSI_NO_adjust, Date >= "1981-12-01", Date <= "2022-02-01")  

CombinedData <- Azotea_CO_SWSI_NO_adjust
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
mod_AMRAp1q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=1,q=2), method="ML") 
mod_AMRAp2q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=2,q=2), method="ML") 
#p = regressive order, #q is moving average order #41:40#p = regressive order, #q is moving average order #41:40


# compare models with AIC, AICc, and BIC
# For small data, use AICc – the small sample correction which provides greater penalty for each parameter but approaches AIC as n becomes large. If it makes a difference, you should use it. 
# For large data and especially time series data, consider BIC. BIC is better in situations where a false positive is more misleading than a false negative. Remember that false positives are more common with time series. 
bbmle::AICtab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp1q2,mod_AMRAp2q2)
bbmle::AICctab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp1q2,mod_AMRAp2q2)
bbmle::BICtab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp1q2,mod_AMRAp2q2)

#BIC result:
#dBIC  df
#mod_AMRAp2q2   0.0 7 
#mod_AMRAp2    18.1 5 
#mod_AMRAp3    24.1 6 
#mod_AMRAp0q2  31.5 5 
#mod_AMRAp1q1  38.7 5 
#mod_Ar1      129.0 4 

summary(mod_AMRAp2q2)



# intervals() for nlme is equivelant to confint() for lm
intervals(mod_AMRAp2q2)


par(mfrow=c(1,1))
visreg(mod_AMRAp2q2,"SWSI_values")

### Important notes about extracting residuals from model fits!! ###

# It's important to understand that many extraction fxns in R, such as residuals(modelfit) (same as resid(modelfit)), will detect the object type and call on methods from that package appropriate for that object. So, residuals(modelfit) is using different methods for different model types when the model package requires it, and you need to look up the options for these different methods.
# E.g., residuals(nlme model) calls residuals.lme(nlme model), which has different options than if you call residuals(model fit) on a different kind of model. 
# see ?residuals.gls for the methods avaiable for this model type
# type ?residuals. into your console and scroll through the options for other residuals methods for loaded packages

# For gls, you want to assess assumptions on normalized residuals, which is not an option for standard linear models.
# normalized residuals = standardized residuals pre-multiplied by the inverse square-root factor of the estimated error correlation matrix
# see https://stats.stackexchange.com/questions/80823/do-autocorrelated-residual-patterns-remain-even-in-models-with-appropriate-corre

Acf(resid(mod_AMRAp2q2))

# extract and assess residuals: AMRAp2q2: 
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp2q2, type = "normalized"), main="RAW Azotea & SWSI GLS AMRAp2q2 model residuals")
plot(resid(mod_AMRAp2q2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="RAW Azotea & SWSI GLS AMRAp2q2 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp2q2, type = "normalized"), main="RAW Azotea & SWSI GLS AMRAp2q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp2q2, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp2q2, type = "normalized"))


# extract and assess residuals: #mod_AMRAp2 
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp2, type = "normalized"), main="RAW Azotea & SWSI GLS AMRAp2 model residuals")
plot(resid(mod_AMRAp2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="RAW Azotea & SWSI GLS AMRAp2 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp2, type = "normalized"), main="RAW Azotea & SWSI GLS AMRAp2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp2, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp2, type = "normalized"))

# extract and assess residuals: mod_AMRAp3 
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp3, type = "normalized"), main="RAW Azotea & SWSI GLS AMRAp3 model residuals")
plot(resid(mod_AMRAp3, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="RAW Azotea & SWSI GLS AMRAp3 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp3, type = "normalized"), main="RAW Azotea & SWSI GLS AMRAp3 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp3, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp3, type = "normalized"))

# extract and assess residuals: AMRAp0q2
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp0q2, type = "normalized"), main="RAW Azotea & SWSI GLS AMRAp0q2 model residuals")
plot(resid(mod_AMRAp0q2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="RAW Azotea & SWSI GLS AMRAp0q2 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp0q2, type = "normalized"), main="RAW Azotea & SWSI GLS AMRAp0q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp0q2, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp0q2, type = "normalized"))


# extract and assess residuals: AMRAp1q1
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp1q1, type = "normalized"), main="RAW Azotea & SWSI GLS AMRAp1q1model residuals")
plot(resid(mod_AMRAp1q1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="RAW Azotea & SWSI GLS AMRAp1q1model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp1q1, type = "normalized"), main="RAW Azotea & SWSI GLS AMRAp1q1model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp1q1, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp1q1, type = "normalized"))




# exctract parameter estimates for comparison with MARSS
mod_AMRAp1q1.phi = coef(mod_AMRAp1q1$modelStruct[[1]], unconstrained=FALSE)
ests.gls = c(b=mod_AMRAp1q1.phi, alpha=coef(mod_Ar1)[1],
             time=coef(mod_AMRAp1q1)[2],
             logLik=logLik(mod_AMRAp1q1))

####Azotea - CO SWSI linear model w seasonal correction on Azotea data  ##Candidates p1q1, p0q2 ####
##Candidates p1q1, p0q2 (<-these two had best BIC comparison), p3, p1q2
Azotea_CO_SWSI_NO_adjust <- full_join(Azotea_Corrected,SWSI_CO, by = "Date")  #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI
Azotea_CO_SWSI_NO_adjust$Discharge = as.numeric(Azotea_CO_SWSI_NO_adjust$Discharge)
Azotea_CO_SWSI_NO_adjust$SWSI_values = as.numeric(Azotea_CO_SWSI_NO_adjust$SWSI_values)

#POR for Azotea data is older than for SWSI. Remove dates where there are no SWSI values. 

Azotea_CO_SWSI_NO_adjust <- 
  filter(Azotea_CO_SWSI_NO_adjust, Date >= "1981-12-01", Date <= "2022-02-01")  

CombinedData <- Azotea_CO_SWSI_NO_adjust
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
mod_AMRAp1q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=1,q=2), method="ML") 
mod_AMRAp2q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=2,q=2), method="ML") 
#p = regressive order, #q is moving average order #41:40#p = regressive order, #q is moving average order #41:40


# compare models with AIC, AICc, and BIC
# For small data, use AICc – the small sample correction which provides greater penalty for each parameter but approaches AIC as n becomes large. If it makes a difference, you should use it. 
# For large data and especially time series data, consider BIC. BIC is better in situations where a false positive is more misleading than a false negative. Remember that false positives are more common with time series. 
bbmle::AICtab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp1q2,mod_AMRAp2q2)
bbmle::AICctab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp1q2,mod_AMRAp2q2)
bbmle::BICtab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp1q2,mod_AMRAp2q2)

#BIC result:
#mod_AMRAp1q1  0.0 5 
#mod_AMRAp0q2  0.2 5 
#mod_AMRAp2    1.1 5 
#mod_AMRAp3    5.8 6 
#mod_AMRAp1q2  8.4 6 
#mod_AMRAp2q2 12.0 7 
#mod_Ar1      19.5 4 

summary(mod_AMRAp2q2)



# intervals() for nlme is equivelant to confint() for lm
intervals(mod_AMRAp2q2)


par(mfrow=c(1,1))
visreg(mod_AMRAp2q2,"SWSI_values")

Acf(resid(mod_AMRAp2q2))

# extract and assess residuals: AMRAp1q1
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp1q1, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp1q1 model residuals")
plot(resid(mod_AMRAp1q1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSAMRAp1q1model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp1q1, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp1q1model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp1q1, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp1q1, type = "normalized"))



# extract and assess residuals: AMRAp0q2
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp0q2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp0q2 model residuals")
plot(resid(mod_AMRAp0q2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSAMRAp0q2 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp0q2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp0q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp0q2, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp0q2, type = "normalized"))

# extract and assess residuals: #mod_AMRAp2 
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp2 model residuals")
plot(resid(mod_AMRAp2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSAMRAp2 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp2, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp2, type = "normalized"))



# extract and assess residuals: mod_AMRAp3 
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp3, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp3 model residuals")
plot(resid(mod_AMRAp3, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSAMRAp3 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp3, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp3 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp3, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp3, type = "normalized"))


# extract and assess residuals: AMRAp1q2: 
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp1q2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp1q2 model residuals")
plot(resid(mod_AMRAp1q2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSAMRAp1q2 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp1q2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp1q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp1q2, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp1q2, type = "normalized"))



# extract and assess residuals: AMRAp2q2: 
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp2q2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp2q2 model residuals")
plot(resid(mod_AMRAp2q2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSAMRAp2q2 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp2q2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp2q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp2q2, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp2q2, type = "normalized"))




# extract and assess residuals: Ar1
par(mfrow=c(1,3))
Acf(resid(mod_Ar1, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAr1model residuals")
plot(resid(mod_Ar1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSAr1model residuals"); abline(h=0)
qqnorm(resid(mod_Ar1, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAr1model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_Ar1, type = "normalized"))$statistic,2))); qqline(resid(mod_Ar1, type = "normalized"))



####Azotea - RG SWSI linear model w seasonal correction on Azotea data ##Candidates: p3, p1q2,(p2q2, p0q2 over the line)  ####
##Candidates: p3, p1q2
Azotea_RG_SWSI <- full_join(Azotea_Corrected,SWSI_RG, by = "Date")  #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI
Azotea_RG_SWSI$Discharge = as.numeric(Azotea_RG_SWSI$Discharge)
Azotea_RG_SWSI$SWSI_values = as.numeric(Azotea_RG_SWSI$SWSI_values)


#POR for Azotea data is older than for SWSI. Remove dates where there are no SWSI values. 
Azotea_RG_SWSI <- 
  filter(Azotea_RG_SWSI, Date >= "1981-06-01", Date <= "2022-08-01")  

CombinedData <- Azotea_RG_SWSI
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
mod_AMRAp1q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=1,q=2), method="ML") 
mod_AMRAp2q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=2,q=2), method="ML") 
#p = regressive order, #q is moving average order #41:40#p = regressive order, #q is moving average order #41:40


# compare models with AIC, AICc, and BIC
# For small data, use AICc – the small sample correction which provides greater penalty for each parameter but approaches AIC as n becomes large. If it makes a difference, you should use it. 
# For large data and especially time series data, consider BIC. BIC is better in situations where a false positive is more misleading than a false negative. Remember that false positives are more common with time series. 
bbmle::AICtab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp1q2,mod_AMRAp2q2)
bbmle::AICctab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp1q2,mod_AMRAp2q2)
bbmle::BICtab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp1q2,mod_AMRAp2q2)

#BIC result:
#mod_AMRAp1q1  0.0 5 
#mod_AMRAp0q2  0.3 5 
#mod_AMRAp2    1.1 5 
#mod_AMRAp3    5.9 6 
#mod_AMRAp1q2  8.2 6 
#mod_AMRAp2q2 12.1 7 
#mod_Ar1      18.2 4 

summary(mod_AMRAp2q2)



# intervals() for nlme is equivelant to confint() for lm
intervals(mod_AMRAp2q2)


par(mfrow=c(1,1))
visreg(mod_AMRAp2q2,"SWSI_values")

### Important notes about extracting residuals from model fits!! ###

Acf(resid(mod_AMRAp2q2))

# extract and assess residuals: AMRAp1q1
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp1q1, type = "normalized"), main="Discharge adjusted, Raw SWSI GLS-RGRG AMRAp1q1model residuals")
plot(resid(mod_AMRAp1q1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLS-RGRG AMRAp1q1model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp1q1, type = "normalized"), main="Discharge adjusted, Raw SWSI GLS-RGRG AMRAp1q1model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp1q1, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp1q1, type = "normalized"))



# extract and assess residuals: AMRAp0q2
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp0q2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLS-RGAMRAp0q2 model residuals")
plot(resid(mod_AMRAp0q2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLS-RGAMRAp0q2 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp0q2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLS-RGAMRAp0q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp0q2, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp0q2, type = "normalized"))

# extract and assess residuals: #mod_AMRAp2 
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLS-RGAMRAp2 model residuals")
plot(resid(mod_AMRAp2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLS-RGAMRAp2 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLS-RGAMRAp2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp2, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp2, type = "normalized"))



# extract and assess residuals: mod_AMRAp3 
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp3, type = "normalized"), main="Discharge adjusted, Raw SWSI GLS-RGAMRAp3 model residuals")
plot(resid(mod_AMRAp3, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLS-RGAMRAp3 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp3, type = "normalized"), main="Discharge adjusted, Raw SWSI GLS-RGAMRAp3 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp3, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp3, type = "normalized"))


# extract and assess residuals: AMRAp1q2: 
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp1q2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLS-RGAMRAp1q2 model residuals")
plot(resid(mod_AMRAp1q2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLS-RGAMRAp1q2 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp1q2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLS-RGAMRAp1q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp1q2, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp1q2, type = "normalized"))



# extract and assess residuals: AMRAp2q2: 
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp2q2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLS-RGAMRAp2q2 model residuals")
plot(resid(mod_AMRAp2q2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLS-RGAMRAp2q2 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp2q2, type = "normalized"), main="Discharge adjusted, Raw SWSI GLS-RGAMRAp2q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp2q2, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp2q2, type = "normalized"))




# extract and assess residuals: Ar1
par(mfrow=c(1,3))
Acf(resid(mod_Ar1, type = "normalized"), main="Discharge adjusted, Raw SWSI GLS-RGAr1model residuals")
plot(resid(mod_Ar1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLS-RGAr1model residuals"); abline(h=0)
qqnorm(resid(mod_Ar1, type = "normalized"), main="Discharge adjusted, Raw SWSI GLS-RGAr1model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_Ar1, type = "normalized"))$statistic,2))); qqline(resid(mod_Ar1, type = "normalized"))



####Heron - CO SWSI linear model WITHOUT SEASONALITY CORRECTION ####
Heron_CO_SWSI_Raw <- full_join(Heron_filled,SWSI_CO, by = "Date")  #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI
Heron_CO_SWSI_Raw$Discharge = as.numeric(Heron_CO_SWSI_Raw$Discharge)
Heron_CO_SWSI_Raw$SWSI_values = as.numeric(Heron_CO_SWSI_Raw$SWSI_values)

#POR for Heron data is older than for SWSI. Remove dates where there are no SWSI values. 

Heron_CO_SWSI_Raw <- 
  filter(Heron_CO_SWSI_Raw, Date >= "2008-01-01", Date <= "2022-08-01")  

CombinedData <- Heron_CO_SWSI_Raw
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
#Doesnt run: mod_AMRAp1q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=1,q=2), method="ML") 
mod_AMRAp2q2 = gls(Discharge ~ SWSI_values, data=CombinedData, correlation=corARMA(p=2,q=2), method="ML") 
#p = regressive order, #q is moving average order #41:40#p = regressive order, #q is moving average order #41:40


# compare models with AIC, AICc, and BIC
# For small data, use AICc – the small sample correction which provides greater penalty for each parameter but approaches AIC as n becomes large. If it makes a difference, you should use it. 
# For large data and especially time series data, consider BIC. BIC is better in situations where a false positive is more misleading than a false negative. Remember that false positives are more common with time series. 
bbmle::AICtab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp2q2)
bbmle::AICctab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp2q2)
bbmle::BICtab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3,mod_AMRAp0q2,mod_AMRAp2q2)

#dAIC   df
#mod_AMRAp2q2    0.0 7 
#mod_AMRAp1q1    1.7 5 
#mod_AMRAp0q2    1.8 5 
#mod_AMRAp2      3.6 5 
#mod_AMRAp3      5.4 6 
#mod_Ar1      6405.7 4 

summary(mod_AMRAp2q2)


# intervals() for nlme is equivelant to confint() for lm
intervals(mod_AMRAp2q2)


par(mfrow=c(1,1))
visreg(mod_AMRAp2q2,"SWSI_values")


Acf(resid(mod_AMRAp2q2))

# extract and assess residuals: AMRAp2q2: 
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp2q2, type = "normalized"), main="Raw discharge & SWSI GLS AMRAp2q2 model residuals")
plot(resid(mod_AMRAp2q2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Raw discharge & SWSI GLS AMRAp2q2 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp2q2, type = "normalized"), main="Raw discharge & SWSI GLS AMRAp2q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp2q2, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp2q2, type = "normalized"))


# extract and assess residuals: #mod_AMRAp2 
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp2, type = "normalized"), main="Raw discharge & SWSI GLS AMRAp2 model residuals")
plot(resid(mod_AMRAp2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Raw discharge & SWSI GLS AMRAp2 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp2, type = "normalized"), main="Raw discharge & SWSI GLS AMRAp2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp2, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp2, type = "normalized"))

# extract and assess residuals: mod_AMRAp3 
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp3, type = "normalized"), main="Raw discharge & SWSI GLS AMRAp3 model residuals")
plot(resid(mod_AMRAp3, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Raw discharge & SWSI GLS AMRAp3 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp3, type = "normalized"), main="Raw discharge & SWSI GLS AMRAp3 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp3, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp3, type = "normalized"))

# extract and assess residuals: AMRAp0q2
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp0q2, type = "normalized"), main="Raw discharge & SWSI GLS AMRAp0q2 model residuals")
plot(resid(mod_AMRAp0q2, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Raw discharge & SWSI GLS AMRAp0q2 model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp0q2, type = "normalized"), main="Raw discharge & SWSI GLS AMRAp0q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp0q2, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp0q2, type = "normalized"))


# extract and assess residuals: AMRAp1q1
par(mfrow=c(1,3))
Acf(resid(mod_AMRAp1q1, type = "normalized"), main="Raw discharge & SWSI GLS AMRAp1q1model residuals")
plot(resid(mod_AMRAp1q1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Raw discharge & SWSI GLS AMRAp1q1model residuals"); abline(h=0)
qqnorm(resid(mod_AMRAp1q1, type = "normalized"), main="Raw discharge & SWSI GLS AMRAp1q1model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_AMRAp1q1, type = "normalized"))$statistic,2))); qqline(resid(mod_AMRAp1q1, type = "normalized"))



# extract and assess residuals: Ar1
par(mfrow=c(1,3))
Acf(resid(mod_Ar1, type = "normalized"), main="Raw discharge & SWSI GLS Ar1model residuals")
plot(resid(mod_Ar1, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Raw discharge & SWSI GLS Ar1model residuals"); abline(h=0)
qqnorm(resid(mod_Ar1, type = "normalized"), main="Raw discharge & SWSI GLS Ar1model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_Ar1, type = "normalized"))$statistic,2))); qqline(resid(mod_Ar1, type = "normalized"))




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


