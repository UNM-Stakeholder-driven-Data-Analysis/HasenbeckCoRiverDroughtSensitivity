####READ ME#### 
#THis model tests hurdle and Poisson ZIP models to id which is appropriate for further exploration using data from the SJC project. 


####Libraries#### 
library(readr)
library(tidyverse)
library(lubridate)

library(AER) #Poisson Model Package
library(pscl) #Hurdle and ZIP Model package 
# For Hurdle Plotting: Need to install from R-Forge instead of CRAN
#install.packages("countreg", repos="http://R-Forge.R-project.org")
library(countreg)

library(forecast)
library(MARSS)
library(nlme)
library(zoo)
library(beepr)
library(gridExtra)
library(lme4)
library(car)
library(visreg)

####Read in data#### 

AzoteaDiversions <- read_csv(file = "data/processed/AzoteaMonthlyDischarge")
HeronReleases <- read_csv(file="data/processed/HeronMonthlyReleases") %>% 
  rename(Discharge = Release) #Renaming columns to same name as header in Tunnel data so I can recycle code more easily later
AllSWSI <- read_csv(file= "data/processed/SWSI1981to2023.csv")

#### Prepping Data for model #### 

SWSI_CORG <- AllSWSI %>% #Creating dfs by basin
  dplyr::select(Date,Rio_Grande,Colorado) %>% 
  pivot_longer(cols = "Rio_Grande":"Colorado", 
               names_to = "basin",
               values_to = "SWSI")
SWSI_CORG$SWSI = as.numeric(SWSI_CORG$SWSI) # Set data type


SWSI_Colorado <- AllSWSI %>% #Creating dfs by basin
  dplyr::select(Date,Colorado) %>% 
  rename(SWSI = Colorado) #rename column to logical name 
SWSI_Colorado$SWSI = as.numeric(SWSI_Colorado$SWSI) # Set data type

SWSI_RG <- AllSWSI %>% #Creating dfs by basin
  dplyr::select(Date,Rio_Grande) %>%
  rename(SWSI = Rio_Grande) #rename column to logical name
SWSI_RG$SWSI = as.numeric(SWSI_RG$SWSI) # Set data type

sum(is.na(SWSI_CORG))
#0
sum(is.na(HeronReleases))
#3
sum(is.na(AzoteaDiversions))/nrow(AzoteaDiversions)*100
#46
View(AzoteaDiversions)
####Azotea and CO LME w temporal autocorrelation#####


## fill gaps with spline interpolation ##
# for calculating long-term trends, you can be pretty liberal with how large of gaps you fill. However, if you go too big with a spline interpolation, you'll get wacky results (ALWAYS EXAMINE THE RESULTS OF GAP FILLING!!). To strike this balance, I'm filling gaps of up to five days here. 
par(mfrow=c(2,1)) # set up plotting window to comapare ts before and after gap filling

# Apply NA interpolation method
plot(AzoteaDiversions)
Azotea_fill <- AzoteaDiversions
Azotea_fill$Discharge = na.spline(AzoteaDiversions$Discharge, na.rm = T, maxgap = 10)
Azotea_fill$Discharge[Azotea_fill$Discharge < 0] = 0 
Azotea_fill <- as.data.frame(Azotea_fill)
plot(Azotea_fill)
view(Azotea_fill)
sum(is.na(Azotea_fill))/nrow(Azotea_fill)*100

#Replaced NA by spine interpolation. Then converted interpolated negative values to 0. 

par(mfrow=c(1,1)) # reset plotting window

### create time series ###

# need to do this to prep for removing seasonality

Azotea_ts = ts(Azotea_fill$Discharge, start = c(1970-10-1), frequency = 12)
head(Azotea_ts)

par(mfrow=c(1,1))
plot(Azotea_ts)

#### remove seasonality ####

# examine seasonality
par(mfrow=c(3,1))
plot(Azotea_ts)
Acf(Azotea_ts)
Pacf(Azotea_ts)

# decompose into additive components
plot(decompose(Azotea_ts))
# decompose into multiplicative components
plot(decompose(Azotea_ts, type="multiplicative"))
# extract components from multiplicative
Azotea_ts_decomp = decompose(Azotea_ts, type="multiplicative")
Azotea_ts_trend = Azotea_ts_decomp$trend
Azotea_ts_remainder = Azotea_ts_decomp$random
# save de-seasoned ts
Azotea_ts_DEs = Azotea_ts_trend * Azotea_ts_remainder

# compare original to de-seasoned ts
par(mfrow=c(3,2))
plot(Azotea_ts)
plot(Azotea_ts_DEs)
Acf(Azotea_ts)
Acf(Azotea_ts_DEs)
Pacf(Azotea_ts)
Pacf(Azotea_ts_DEs)

####WHY DO MY VALUES TURN INTO NAS#### 
# revert back to df
Azotea_DEs = as.data.frame(Azotea_ts_DEs)
Azotea_DEs$Date = AzoteaDiversions$Date
view(Azotea_DEs)
names(Azotea_DEs) = c("Discharge","Date")
Azotea_DEs = Azotea_DEs %>% dplyr::select(Date, Discharge) %>% arrange(Date)
Azotea_DEs = na.trim(Azotea_DEs, "both")

ggplot(Azotea_DEs, aes(x=Date, y=Discharge))+
  geom_path() + geom_point() + theme_bw()


#### linear trends ####

# add simple time steps to df
Azotea_DEs$t = c(1:nrow(Azotea_DEs))

mod = lm(Discharge ~ SWSI_Colorado, Azotea_DEs)

summary(mod)

visreg(mod,"t")

confint(mod, 't', level=0.95)

## diagnostics ##
Acf(resid(mod))
forecast::checkresiduals(mod)

#### test & calculate trends - nlme::gls ####

# see package manual: https://cran.r-project.org/web/packages/nlme/nlme.pdf

# ask auto.arima what it thinks the autocorrelation structure is
auto.arima(RG_abq_mo_DEs$Q_cfs)

# fit AR(1) regression model with time as a predictor
mod_Ar1 = gls(Q_cfs ~ t, data=RG_abq_mo_DEs, correlation=corAR1(), method="ML")

# fit some other candidate structures
mod_AMRAp1q1 = gls(Q_cfs ~ t, data=RG_abq_mo_DEs, correlation=corARMA(p=1,q=1), method="ML")
mod_AMRAp2 = gls(Q_cfs ~ t, data=RG_abq_mo_DEs, correlation=corARMA(p=2), method="ML")
mod_AMRAp3 = gls(Q_cfs ~ t, data=RG_abq_mo_DEs, correlation=corARMA(p=3), method="ML")

# compare models with AIC, AICc, and BIC
# For small data, use AICc – the small sample correction which provides greater penalty for each parameter but approaches AIC as n becomes large. If it makes a difference, you should use it. 
# For large data and especially time series data, consider BIC. BIC is better in situations where a false positive is more misleading than a false negative. Remember that false positives are more common with time series. 
bbmle::AICtab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3)
bbmle::AICctab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3)
bbmle::BICtab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3)

summary(mod_Ar1)
# intervals() for nlme is equivelant to confint() for lm
intervals(mod_Ar1)

# notice that p value 0.0002 for mod_Ar1 is much higher than in linear model
# and 95% CIs are much broader for mod_Ar1
# this demonstrates the type 1 error of lm with ts data!

par(mfrow=c(1,1))
visreg(mod_Ar1,"t")

### Important notes about extracting residuals from model fits!! ###

# It's important to understand that many extraction fxns in R, such as residuals(modelfit) (same as resid(modelfit)), will detect the object type and call on methods from that package appropriate for that object. So, residuals(modelfit) is using different methods for different model types when the model package requires it, and you need to look up the options for these different methods.
# E.g., residuals(nlme model) calls residuals.lme(nlme model), which has different options than if you call residuals(model fit) on a different kind of model. 
# see ?residuals.gls for the methods avaiable for this model type
# type ?residuals. into your console and scroll through the options for other residuals methods for loaded packages

# For gls, you want to assess assumptions on normalized residuals, which is not an option for standard linear models.
# normalized residuals = standardized residuals pre-multiplied by the inverse square-root factor of the estimated error correlation matrix
# see https://stats.stackexchange.com/questions/80823/do-autocorrelated-residual-patterns-remain-even-in-models-with-appropriate-corre

Acf(resid(mod_Ar1))

# extract and assess residuals
par(mfrow=c(1,3))
Acf(resid(mod_Ar1, type = "normalized"), main="GLS AR(1) model residuals")
plot(resid(mod_Ar1, type = "normalized")~c(1:length(RG_abq_mo_DEs$t)), main="GLS AR(1) model residuals"); abline(h=0)
qqnorm(resid(mod_Ar1, type = "normalized"), main="GLS AR(1) model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_Ar1, type = "normalized"))$statistic,2))); qqline(resid(mod_Ar1, type = "normalized"))

# exctract parameter estimates for comparison with MARSS
mod_Ar1.phi = coef(mod_Ar1$modelStruct[[1]], unconstrained=FALSE)
ests.gls = c(b=mod_Ar1.phi, alpha=coef(mod_Ar1)[1],
             time=coef(mod_Ar1)[2],
             logLik=logLik(mod_Ar1))

#### test & calculate trends - UARSS ####

## MARSS ##
# User;s guide: https://cran.r-project.org/web/packages/MARSS/vignettes/UserGuide.pdf
# Package manual: https://cran.r-project.org/web/packages/MARSS/MARSS.pdf
# Lectures: https://nwfsc-timeseries.github.io/atsa/
# Lab book: https://nwfsc-timeseries.github.io/atsa-labs/
# Quick start guide: https://cran.r-project.org/web/packages/MARSS/vignettes/Quick_Start.pdf
# MARSS is a extremely flexible ts modeling framework with a steep learning curve requiring lots of matrix algebra for many applications, so wade into these documents with caution and prepare to get overwhelmed quickly! Also the user's guide is written, in my opinion, pretty poorly for the average user. This class will give you a few common "recipes", but know that almost anything is possible if you take time to learn the modeling framework in full.


## MARSS equivelent to gls corAR1 model ##

## format response var y as a vector
dat = as.vector(RG_abq_mo_DEs$Q_cfs)
# remove leading and trailing NAs #
dat = na.trim(dat, is.na="any")

## set model parameters - see powerpoint for 
mod.list.AR1 = list(
  B=matrix("b"),          # state process model
  Q=matrix("q"),          # state process model
  U="zero",               # state process model
  C="zero",               # state process model
  Z="identity",           # observation process model
  R=matrix("r"),          # observation process model
  A=matrix("intercept"),  # observation process model
  D=matrix("time"),       # observation process model
  d=matrix(c(1:length(dat)), nrow=1), # observation process model
  x0=matrix(dat[1]), 
  tinitx=0
)

## fit model
# mod.AR1 <- MARSS(dat, model=mod.list.AR1, method="BFGS")
mod.AR1 <- MARSS(dat, model=mod.list.AR1, control=list(maxit=10000))
beep(2)
# est.AR1 <- MARSSparamCIs(mod.AR1, method = "parametric", alpha = 0.05, nboot = 2000, silent=F)
est.AR1 <- MARSSparamCIs(mod.AR1, method = "hessian", alpha = 0.05)

## extract parameter estimates for comparison to gls
ests.marss = c(b=coef(mod.AR1)$B, alpha=coef(mod.AR1)$A,
               time=coef(mod.AR1)$D[1],
               logLik=logLik(mod.AR1))

## compare UARSS and gls results
# parameter estimates
ests.marss
ests.gls
# 95% CIs on trend estimate
intervals(mod_Ar1)
est.AR1
# notice that UARSS provides a slightly narrower confidence interval

## test residuals for ac
# extract residuals
resids.1 <- residuals(mod.AR1) # see ?residuals.marssMLE
# plot residuals
par(mfrow=c(2,3))
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
kf=print(mod.AR1, what="kfs") # Kalman filter and smoother output
# plot observed data (y)
par(mfrow=c(1,1),oma = c(0, 0, 2, 0))
plot(as.vector(dat) ~ RG_abq_mo_DEs$date, type="p", pch=19,
     main = "UARSS model predictions conditioned on all y",
     ylab = "Discharge (cfs)", xlab="")
# calc and plot predicted values
predicts = as.vector(kf$xtT) + 
  coef(mod.AR1)$A[1] + 
  (as.vector(mod.AR1[["model"]][["fixed"]][["d"]])* coef(mod.AR1)$D[1])
lines(predicts ~ RG_abq_mo_DEs$date, col="blue",lwd=2) 
lines(RG_abq_mo_DEs$date, predicts-1.96*mod.AR1$states.se,
      type="l",lwd=1,lty=2,col="blue")
lines(RG_abq_mo_DEs$date, predicts+1.96*mod.AR1$states.se,
      type="l",lwd=1,lty=2,col="blue")
# calc and plot predicted values without trend
predicts.trendless = as.vector(kf$xtT) + 
  coef(mod.AR1)$A[1] 
lines(predicts.trendless ~ RG_abq_mo_DEs$date, col="red",lwd=2) 
lines(RG_abq_mo_DEs$date, predicts.trendless-1.96*mod.AR1$states.se,
      type="l",lwd=1,lty=2,col="red")
lines(RG_abq_mo_DEs$date, predicts.trendless+1.96*mod.AR1$states.se,
      type="l",lwd=1,lty=2,col="red")
mtext("Fitted values over observations", outer = TRUE, cex = 1.5)
# dashed lines are 95% CIs, calculated from the standard error. The value of 1.96 is based on the fact that 95% of the area of a normal distribution is within 1.96 standard deviations of the mean.

# trend model predictions and non-trend model predictions start diverging ~1996
# red line indicates Rio Grande discharge if there weren't a long-term decline. 

# you can make a similar plot from nlme::gls results! go try and figure that out :)








####Azotea and both SWSI Poisson: Not Useful, ignore this section ####
#resource and code: https://data.library.virginia.edu/getting-started-with-hurdle-models/

Azotea_CO_RG <- full_join(AzoteaDiversions,SWSI_CORG, by = "Date") #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI

CombinedData <- Azotea_CO_RG
CombinedData$Discharge = as.integer(CombinedData$Discharge)
CombinedData$SWSI = as.integer(CombinedData$SWSI)

mod1 <- glm(Discharge ~ SWSI, data = CombinedData, family = "poisson")

# predict expected mean count
mu <- predict(mod1, type = "response")

# sum the probabilities of a 0 count for each mean
exp <- sum(dpois(x = 0, lambda = mu))

# predicted number of 0's
round(exp) 
#[1] 0



#Set df: 
CombinedData <- full_join(AzoteaDiversions,SWSI_RG, by = "Date") #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI


#Model:
mod.hurdle <- hurdle(Discharge ~ SWSI, data = CombinedData)#### Hurdel: Azotea RG###

#negative bionomial distribution: 
mod.hurdle.nb <- hurdle(Discharge ~ SWSI, data = CombinedData, dist = "negbin")
rootogram(mod.hurdle.nb) #plot

AIC(mod.hurdle)
#[1] 6519198
AIC(mod.hurdle.nb) # lower is better
#[1] 7262.279

mod.hurdle.nb2 <- hurdle(Discharge ~ SWSI | basin + SWSI, 
                         data = CombinedData, dist = "negbin")
rootogram(mod.hurdle.nb2)

View(AzoteaDiversions)

#### Azotea and CO SWSI Hurdle - HUrdle nb AIC: 7261.97 #### 
CombinedData <-  full_join(AzoteaDiversions,SWSI_Colorado, by = "Date") #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI
CombinedData$Discharge = as.integer(CombinedData$Discharge)#Set data types
CombinedData$SWSI = as.integer(CombinedData$SWSI) #Set data types

mod.hurdle <- hurdle(Discharge ~ SWSI, data = CombinedData)
summary(mod.hurdle)
sum(predict(mod.hurdle, type = "prob",na.rm=T)[,1])
#[1] 70

sum(CombinedData$Discharge < 1, na.rm=T)
#82

# First 5 expected counts
predict(mod.hurdle, type = "response")[1:5]

rootogram(mod.hurdle) # plot - above the line on far positive. 

#negative bionomial distribution: 
mod.hurdle.nb <- hurdle(Discharge ~ SWSI, data = CombinedData, dist = "negbin")
rootogram(mod.hurdle.nb) #plot

AIC(mod.hurdle)
#[1] 3296987
AIC(mod.hurdle.nb) # lower is better
#[1] 7261.974


#### Azotea and RG SWSI Hurdle - Hurdle NB AIC: 7262.279 #### 
CombinedData <- full_join(AzoteaDiversions,SWSI_RG, by = "Date") #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI
CombinedData$Discharge = as.integer(CombinedData$Discharge) #Set data types
CombinedData$SWSI = as.integer(CombinedData$SWSI) #Set data types

mod.hurdle <- hurdle(Discharge ~ SWSI, data = CombinedData)
summary(mod.hurdle)
sum(predict(mod.hurdle, type = "prob",na.rm=T)[,1])
#[1] 70

sum(CombinedData$Discharge < 1, na.rm=T)
#82

# First 5 expected counts
predict(mod.hurdle, type = "response")[1:5]

rootogram(mod.hurdle) # plot - above the line on far positive. 

#negative bionomial distribution: 
mod.hurdle.nb <- hurdle(Discharge ~ SWSI, data = CombinedData, dist = "negbin")
rootogram(mod.hurdle.nb) #plot

AIC(mod.hurdle)
#[1] 3205061
AIC(mod.hurdle.nb) # lower is better
#[1] 7262.279


par(mfrow=c(1,3))
Acf(resid(mod.hurdle.nb, type = "pearson"), main="Discharge adjusted, Raw SWSI GLSAMRAp1q2 model residuals")
plot(resid(mod.hurdle.nb, type = "normalized")~c(1:length(CombinedData$SWSI_values)), main="Discharge adjusted, Raw SWSI GLSAMRAp1q2 model residuals"); abline(h=0)
qqnorm(resid(mod.hurdle.nb, type = "normalized"), main="Discharge adjusted, Raw SWSI GLSAMRAp1q2 model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod.hurdle.nb, type = "normalized"))$statistic,2))); qqline(resid(mod.hurdle.nb, type = "normalized"))


#### Heron and CO SWSI Hurdle - Hurdle NB AIC : 3015.786 #### 
CombinedData <-  full_join(HeronReleases,SWSI_Colorado, by = "Date") #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI
CombinedData$Discharge = as.integer(CombinedData$Discharge) #Set data types
CombinedData$SWSI = as.integer(CombinedData$SWSI) #Set data types

mod.hurdle <- hurdle(Discharge ~ SWSI, data = CombinedData)
summary(mod.hurdle)
sum(predict(mod.hurdle, type = "prob",na.rm=T)[,1])
#[1] 37

sum(CombinedData$Discharge < 1, na.rm=T)
#37

# First 5 expected counts
predict(mod.hurdle, type = "response")[1:5]

rootogram(mod.hurdle) # plot - good match???  

#negative bionomial distribution: 
mod.hurdle.nb <- hurdle(Discharge ~ SWSI, data = CombinedData, dist = "negbin") 
rootogram(mod.hurdle.nb) #plot - still a good match??? 

AIC(mod.hurdle)
#[1] 791204.9
AIC(mod.hurdle.nb) # lower is better
#[1] 3015.786

#### Heron and RG SWSI Hurdle - Hudle-NB AIC: 3012.062 #### 
CombinedData <- full_join(HeronReleases,SWSI_RG, by = "Date") #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI
CombinedData$Discharge = as.integer(CombinedData$Discharge) #Set data types
CombinedData$SWSI = as.integer(CombinedData$SWSI) #Set data types

mod.hurdle <- hurdle(Discharge ~ SWSI, data = CombinedData)
summary(mod.hurdle)
sum(predict(mod.hurdle, type = "prob",na.rm=T)[,1])
#[1] 37

sum(CombinedData$Discharge < 1, na.rm=T)
#37

# First 5 expected counts
predict(mod.hurdle, type = "response")[1:5]

rootogram(mod.hurdle) # plot - above the line on far positive. 

#negative bionomial distribution: 
mod.hurdle.nb <- hurdle(Discharge ~ SWSI, data = CombinedData, dist = "negbin")
rootogram(mod.hurdle.nb) #plot - good match?? 

AIC(mod.hurdle)
#[1] 754015.6
AIC(mod.hurdle.nb) # lower is better
#[1] 3012.062



#### Azotea and CO SWSI ZIP - ZIP-nb AIC: 7261.973 #### 
CombinedData <-  full_join(AzoteaDiversions,SWSI_Colorado, by = "Date") #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI
CombinedData$Discharge = as.integer(CombinedData$Discharge) #Set data types
CombinedData$SWSI = as.integer(CombinedData$SWSI) #Set data types

#ZIP 
summary(m1 <- zeroinfl(Discharge ~ SWSI, data = CombinedData)) #ZIP Model 

## More ZIP-NB trying: 
summary(fit1 <- zeroinfl((Discharge ~ SWSI),data = CombinedData,dist="negbin",link="logit"))

AIC(m1)
#[1] 3296987
AIC(fit1) # lower is better, slightly better than Hurdle-NB 
#[1] 7261.973
#### Azotea and RG SWSI ZIP - Zip-NB AIC: 7262.803 #### 
CombinedData <-  full_join(AzoteaDiversions,SWSI_RG, by = "Date") #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI
CombinedData$Discharge = as.integer(CombinedData$Discharge) #Set data types
CombinedData$SWSI = as.integer(CombinedData$SWSI) #Set data types

#ZIP 
summary(m1 <- zeroinfl(Discharge ~ SWSI, data = CombinedData)) #ZIP Model 

## More ZIP-NB trying: 
summary(fit1 <- zeroinfl((Discharge ~ SWSI),data = CombinedData,dist="negbin",link="logit"))

AIC(m1)
#[1] 3205061
AIC(fit1) # lower is better, slightly better than Hurdle-NB 
#[1] 7262.803
#### Heron and CO SWSI ZIP - ZIP-nb AIC: 3015.786 #### 
CombinedData <-  full_join(HeronReleases,SWSI_Colorado, by = "Date") #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI
CombinedData$Discharge = as.integer(CombinedData$Discharge) #Set data types
CombinedData$SWSI = as.integer(CombinedData$SWSI) #Set data types

#ZIP 
summary(m1 <- zeroinfl(Discharge ~ SWSI, data = CombinedData)) #ZIP Model 

## More ZIP-NB trying: 
summary(fit1 <- zeroinfl((Discharge ~ SWSI),data = CombinedData,dist="negbin",link="logit"))

AIC(m1)
#[1] 791204.9
AIC(fit1) # lower is better, slightly better than Hurdle-NB 
#[1] 3015.786
#### Heron and RG SWSI ZIP - ZIP-nb AIC: 3012.062#### 
CombinedData <-  full_join(HeronReleases,SWSI_RG, by = "Date") #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI
CombinedData$Discharge = as.integer(CombinedData$Discharge) #Set data types
CombinedData$SWSI = as.integer(CombinedData$SWSI) #Set data types

#ZIP 
summary(m1 <- zeroinfl(Discharge ~ SWSI, data = CombinedData)) #ZIP Model 

## More ZIP-NB trying: 
summary(fit1 <- zeroinfl((Discharge ~ SWSI),data = CombinedData,dist="negbin",link="logit"))

AIC(m1)
#[1] 754015.6
AIC(fit1) # lower is better, slightly better than Hurdle-NB 
#[1] 3010.959

