
####Read me#### 

#the purpose of this script is to import and clean diversion data for three largest (by annual average AF diverted) transmountain diversions in three different basins: 
#The adams tunnel/CBT (Colorado to South Platte)
#San Juan Chama Project (Colorado to Rio Grande)
# Boustead Tunnel (Colorado to Arkansas)

# These diversions were selected because of their size, and to allow analysis in three different beneficiary basins.  


#### libraries ####
library(readr)
library(tidyverse)
library(lubridate)
library(psych) 
library(car) 
library(tsibble)
library(forecast) 



#### Finding relevant WDIDS #### 
Colorado_Transbasin_Diversions <- read_csv("~/OneDrive - University of New Mexico/Spring 2023/Stakeholder Analysis/HasenbeckCoRiverDroughtSensitivity/data/raw/Colorado-Transbasin-Diversions.csv")

#Select only three relevant diversions: 
TBDs_of_Interest = Colorado_Transbasin_Diversions[Colorado_Transbasin_Diversions$TransbasinDiversionName 
                                                 %in% c("Charles H. Boustead Tunnel","San Juan-Chama Project","Colorado-Big Thompson Project"), ]
View(TBDs_of_Interest)
print(TBDs_of_Interest$Structure_WDID_CSV) # getting relevant water ids


#### Reading in data from diversions #### 
#Total diversion = amount of water entering structure
#Total release = water released from structure under dominion and control 

#CBT: Adams Tunnel: 5104634 (pipe) 5104634 exports transmountain water to 0404634 - ADAMS TUNNEL. Only has diversions. 
ADAMS_TUNNEL_Diversions <- read_csv("~/OneDrive - University of New Mexico/Spring 2023/Stakeholder Analysis/HasenbeckCoRiverDroughtSensitivity/data/raw/CBT ALVA B ADAMS TUNNEL_5104634_Diversions.csv")
#0404634 (tunnel) This has diversions and releases because it is what the pipe (5104634) dumps into. NO DATA PRE 1994. Removing from analyis and using only 5104634. 

#Fry-Ark: 3804625 Boustead Tunnel
BOUSTEAD_Diversions <- read_csv("~/OneDrive - University of New Mexico/Spring 2023/Stakeholder Analysis/HasenbeckCoRiverDroughtSensitivity/data/raw/FRY ARK PR BOUSTEAD TUNNEL_3804625_Diversions.csv")

#SJC: 7704636,Blanco diversion, #Navajo 7704635
NAVAJO_DIVERSIONS <- read_csv("~/OneDrive - University of New Mexico/Spring 2023/Stakeholder Analysis/HasenbeckCoRiverDroughtSensitivity/data/raw/USBR NAVAJO DIVERSION_7704635_Diversions_Monthly.csv")
BLANCO_DIVERSIONS <- read_csv("~/OneDrive - University of New Mexico/Spring 2023/Stakeholder Analysis/HasenbeckCoRiverDroughtSensitivity/data/raw/USBR BLANCO R DIVERSION_2904667_Diversions.csv")


#### Cleaning and clarifying data #### 
#### Simplifying data for analysis ####
ADAMS_TUNNEL_Diversions <- ADAMS_TUNNEL_Diversions %>% select(WDID,`Structure Name`,Date,Amount,`Obs Code`, `Data Status`)
BOUSTEAD_Diversions <- BOUSTEAD_Diversions%>% select(WDID,`Structure Name`,Date,Amount,`Obs Code`, `Data Status`)
NAVAJO_DIVERSIONS <- NAVAJO_DIVERSIONS%>% select(WDID,`Structure Name`,Date,Amount,`Obs Code`, `Data Status`)
BLANCO_DIVERSIONS <- BLANCO_DIVERSIONS%>% select(WDID,`Structure Name`,Date,Amount,`Obs Code`, `Data Status`)

## Joining data ### 
AllTBDiversions<- bind_rows(ADAMS_TUNNEL_Diversions, BOUSTEAD_Diversions, NAVAJO_DIVERSIONS, BLANCO_DIVERSIONS)

View(AllTBDiversions)

# Renaming observation based on metadata #
AllTBDiversions$`Obs Code`[AllTBDiversions$`Obs Code` == "*"] <- "obs"   #amt observed in person
AllTBDiversions$`Obs Code`[AllTBDiversions$`Obs Code` == "U"] <- "unk-user" # user supplied unk reliability 
AllTBDiversions$`Obs Code`[AllTBDiversions$`Obs Code` == "K"] <- "k-user" #user supplied known reliability
AllTBDiversions$`Obs Code`[AllTBDiversions$`Obs Code` == "k"] <- "k-user" #user supplied known reliability 
AllTBDiversions$`Obs Code`[AllTBDiversions$`Obs Code` == "R"] <- "obs"   #amount recorded by device
AllTBDiversions$`Obs Code`[AllTBDiversions$`Obs Code` == "S"] <- "estimate"   #amount estimated in person 
AllTBDiversions$`Obs Code`[AllTBDiversions$`Obs Code` == "E"] <- "estimate"   #date and amount estimated 
AllTBDiversions$`Obs Code`[AllTBDiversions$`Obs Code` == "C"] <- "calculated" #formula 
AllTBDiversions$`Obs Code`[AllTBDiversions$`Obs Code` == "M"] <- "modeled"   # model


#Define dates, data types: 
AllTBDiversions$WDID = as.factor(AllTBDiversions$WDID)
AllTBDiversions$`Structure Name` = as.factor(AllTBDiversions$`Structure Name`)
AllTBDiversions$`Obs Code` = as.factor(AllTBDiversions$`Obs Code`)
AllTBDiversions$`Data Status` = as.factor(AllTBDiversions$`Data Status`)
AllTBDiversions$Amount = as.numeric(AllTBDiversions$Amount)
AllTBDiversions$Date = as.Date(AllTBDiversions$Date, format="%m/%d/%Y %H:%M") #Date format is 10/31/2022 00:00

AllTBDiversions$Date = 
  mutate(yr = lubridate::year(Date)) %>%
  mutate(mo = lubridate::month(Date)) %>%
  dplyr::select(Structure, yr, mo, Amount) %>%
  group_by(Structure, yr, mo) %>%
  AllTBDiversions$Date = mutate(Date = paste(yr, mo, "1", sep="-")) 
#Renaming columns
AllTBDiversions <- 
  rename(AllTBDiversions, 
       "Observation_Code" = "Obs Code",
       "Structure" = "Structure Name", 
       "Status" = "Data Status"
       )

#Add basins to TMD
AllTBDiversions$SourceBasin <- c("Colorado")
AllTBDiversions$DestinationBasin <- c("Arkansas")
AllTBDiversions$DestinationBasin[AllTBDiversions$Structure == "CBT ALVA B ADAMS TUNNEL"] <- "South_Platte"   
AllTBDiversions$DestinationBasin[AllTBDiversions$Structure == "USBR NAVAJO DIVERSION"] <- "Rio_Grande"   
AllTBDiversions$DestinationBasin[AllTBDiversions$Structure == "USBR BLANCO R DIVERSION"] <- "Rio_Grande"   
AllTBDiversions$DestinationBasin[AllTBDiversions$Structure == "FRY ARK PR BOUSTEAD TUNNEL"] <- "Arkansas" 

View(AllTBDiversions)
Print(NA)

#write to csv 
write_csv(AllTBDiversions, "data/processed/TBdiversionsofinterest")


AllTBDiversions %>%
  ggplot(aes(x = Structure, y = Amount, color = DestinationBasin)) + 
  geom_boxplot() +
  labs(title = "Amount diverted by structure destination river basin") # for the main title, axis labels and legend titles

####Data exploration####

#### describe dataset size and structure ####

dat = AllTBDiversions
head(dat)
str(dat)


#### check timesteps, CBT #### Data is monthly, since 1975 Intervals are regular. 
# make dataset of one of the most frequently collected Structures
dat_lts = 
  dat %>% 
  group_by(Structure) %>% 
  arrange(Date)
dat_lts_alk = 
  dat %>% 
  arrange(Date)

# add year and day of year for plotting
dat_lts_alk$year = lubridate::year(dat_lts_alk$Date)
dat_lts_alk$doy = lubridate::yday(dat_lts_alk$Date)
# plot
ggplot(data=dat_lts_alk, aes(x=doy, y=Amount, color=Structure))+
  geom_point() + geom_path()+
  facet_wrap(~year, scales="free_y")+
  theme(legend.title = element_blank()) +
  theme_bw()



### How many variables are in your dataset?
str(dat)
# 65 Structures

### How many observations are in your dataset?
nrow(dat)
# 2114 total
with(dat, table(Amount, Structure))
range(with(dat, table(Amount, Structure)))
# there are a variable # of observations for each water quality Structure in each site, from 0 to 158 total

### Are the data nested in time or space?
# Yes in time - observations were collected repeatedly on an regular schedule
# Yes in space - observations were collected in three sites

#### describe data types ####
sum(is.na(dat))
str(dat)
summary(dat$Structure)
# diversions are numerical continous interval

ggplot(data=dat, aes(x=doy, y=dat$Amount, color=Structure))+
  geom_point() + geom_path()+
  facet_wrap(~year, scales= "fixed")+
  theme(legend.title = element_blank()) +
  theme_bw() +
  labs(title = "Diversions over time") + # for the main title
  xlab("Day of Year") + # for the x axis label
  ylab("Amount Diverted") # for the y axis label

#### check distributions: CBT is normal. Others are 0-inflated.  ####

dat_r = 
  dat %>% 
  group_by(Structure) %>% 
  arrange(Date)

temp = dat_r[dat_r$Structure == "USBR BLANCO R DIVERSION",]
qqPlot(temp$Amount); shapiro.test(temp$Amount) # NOT normal - right skewed
temp <- subset(temp, Amount != 0) #Run this to see curve without 0s. removing 0s does not normalize
(hist(temp$Amount, breaks = 100, main = "Blanco Diversion distribution", xlab = "AF diverted"))
View(temp)

#Hurdle model, #Use diversions as response variable. Sum diversions to add. Possibly sum the diversions by ddestination basin or end user 


temp = dat_r[dat_r$Structure == "USBR BLANCO R DIVERSION",]
qqPlot(temp$Amount); shapiro.test(temp$Amount) # NOT normal - right skewed
temp <- subset(temp, Amount != 0) #Run this to see curve without 0s. removing 0s does not normalize
(hist(temp$Amount, breaks = 100, main = "Blanco Diversion distribution", xlab = "AF diverted"))
View(temp)



temp = dat_r[dat_r$Structure == "FRY ARK PR BOUSTEAD TUNNEL",]
qqPlot(temp$Amount); shapiro.test(temp$Amount) # NOT normal - right skewed
temp <- subset(temp, Amount != 0) 
hist(temp$Amount, breaks = 100)

temp = dat_r[dat_r$Structure == "USBR NAVAJO DIVERSION",]
temp <- subset(temp, Amount != 0) 
qqPlot(temp$Amount); shapiro.test(temp$Amount) # NOT normal - right skewed 


temp = dat_r[dat_r$Structure == "CBT ALVA B ADAMS TUNNEL",]
qqPlot(temp$Amount); shapiro.test(temp$Amount) # normal
(hist(temp$Amount, breaks = 200, main = "Adams Tunnel Diversion distribution", xlab = "AF diverted"))



### Examine non-normal data closely ###
# ask:
# are outliers making it non-normal? YES 
# can I justify removing outliers based on my knowledge of the data? NO. OUTLIERS ARE IMPORTANT. 
# if data is still non-normal, what distribution is it?

temp = dat_r[dat_r$Structure == "FRY ARK PR BOUSTEAD TUNNEL",]
summary(temp$Amount)
hist(temp$Amount, breaks = 100)
plot(density(temp$Amount))


#Log normalizing - can't get code to run. 
qqPlot(temp$Date(from = 1980-01-01),
       log10(temp$Amount)); shapiro.test(log10(temp$Amount))

#### check for temporal autocorrelation ####

dat_r = 
  dat %>% 
  group_by(Structure, Date) %>% 
  arrange(Date)
summary(dat_r$Structure) 

# checking for temporal autocorrelation requires the data to be a time series object (read ?ts for details on this)
# To achieve this, I need regularly spaced data. This data is irregularly spaced, approximately monthly, but sometimes there are more than one observations per month or fewer
# I will start by averaging observations within the same month:
dat_monthly = 
  dat_r %>%
  mutate(yr = lubridate::year(Date)) %>%
  mutate(mo = lubridate::month(Date)) %>%
  dplyr::select(Structure, yr, mo, Amount) %>%
  group_by(Structure, yr, mo) %>%
  summarise(Amount.mn = mean(Amount, na.rm = T)) %>%
  mutate(date = paste(yr, mo, "15", sep="-")) %>%
  mutate(date = as.Date(date))


### subset data to be one structure:CBT #### 
temp = dat_monthly[dat_monthly$Structure == "CBT ALVA B ADAMS TUNNEL",]

### make this a time series object
## first, make doubly sure that the data is arranged by time before converting to ts object!
temp = temp %>% arrange(date) 
## second, make the spacing of dates consistent and fill in missing obs with NA. This is a handy fxn. You can also create a df of evenly spaced dates and left_join the data to this.
temp_ts =
  temp %>% 
  complete(date = seq(min(date), max(date), by = "1 month"), 
           fill = list(Amount = NA)) %>%
  as_tsibble(index = date)
## finally, convert to a ts object
# a ts object is a vector of data taken sequentially through time. Required arguments are:
# - the data vector
# - the frequency, which is the number of observations per unit of time. Lots of ways to specify this. For monthly data, you can put in 12 and it will assume that's 12 obs in a year. Google for help for other frequencies.
# - the start, which specifies when the first obs occured. Lots of ways to specify this. For monthly data, you can put in c(year, month) and it will know what you mean. 
head (temp_ts)
temp_ts = ts(temp_ts$Amount.mn, frequency=12, start=c(1975, 11)) 
# check that you specified the ts correctly
print(temp_ts, calendar = T) 
### now we're ready to check for temporal autocorrelation in this ts!
# I prefer the forecast pkg's Acf fxn over base R acf() because Acf() doesn't include 0 (which is always 1) and shows month #s by default instead of decimal years. Note the different options for dealing with NAs and how this changes the results (see ?na.fail and ?Acf for details). 
forecast::Acf(temp_ts, na.action = na.pass) 
forecast::Acf(temp_ts, na.action = na.contiguous) 
forecast::Acf(temp_ts, na.action = na.interp)

forecast::Pacf(temp_ts, na.action = na.pass)
forecast::Pacf(temp_ts, na.action = na.contiguous)
forecast::Pacf(temp_ts, na.action = na.interp)

# acf tells me that there is temporal autocorrelation. The sin-wave-like pattern is typical of a ts impacted by seasonality
# pcaf tells me that strongest source of autocorrelation is at lag 1, which indicates a random walk/AR1 process. There is possibly ac at other lags, depending on how NAs are handled. 
#also at lag 10,12 


### subset data to be one structure:Boustead ####
#FRY ARK PR BOUSTEAD TUNNEL starts 1974-05-31
temp = dat_monthly[dat_monthly$Structure == "FRY ARK PR BOUSTEAD TUNNEL",]


### make this a time series object
## first, make doubly sure that the data is arranged by time before converting to ts object!
temp = temp %>% arrange(date) 
## second, make the spacing of dates consistent and fill in missing obs with NA. This is a handy fxn. You can also create a df of evenly spaced dates and left_join the data to this.
temp_ts =
  temp %>% 
  complete(date = seq(min(date), max(date), by = "1 month"), 
           fill = list(Amount = NA)) %>%
  as_tsibble(index = date)
## finally, convert to a ts object
# a ts object is a vector of data taken sequentially through time. Required arguments are:
# - the data vector
# - the frequency, which is the number of observations per unit of time. Lots of ways to specify this. For monthly data, you can put in 12 and it will assume that's 12 obs in a year. Google for help for other frequencies.
# - the start, which specifies when the first obs occured. Lots of ways to specify this. For monthly data, you can put in c(year, month) and it will know what you mean. 
head (temp_ts)

temp_ts = ts(temp_ts$Amount.mn, frequency=12, start=c(1974, 05)) 

temp_ts = ts(temp_ts$Amount.mn, frequency=12, start=c(1975, 11)) 

# check that you specified the ts correctly
print(temp_ts, calendar = T) 
### now we're ready to check for temporal autocorrelation in this ts!
# I prefer the forecast pkg's Acf fxn over base R acf() because Acf() doesn't include 0 (which is always 1) and shows month #s by default instead of decimal years. Note the different options for dealing with NAs and how this changes the results (see ?na.fail and ?Acf for details). 
forecast::Acf(temp_ts, na.action = na.pass) 
forecast::Acf(temp_ts, na.action = na.contiguous) 
forecast::Acf(temp_ts, na.action = na.interp)

forecast::Pacf(temp_ts, na.action = na.pass)
forecast::Pacf(temp_ts, na.action = na.contiguous)
forecast::Pacf(temp_ts, na.action = na.interp)

# acf tells me that there is temporal autocorrelation. The sin-wave-like pattern is typical of a ts impacted by seasonality
# pcaf tells me that strongest source of autocorrelation is at lag 1, which indicates a random walk/AR1 process. There is possibly ac at other lags, depending on how NAs are handled. 
HEAD
#lots of autocorrelation at lag 1,2,11,12,14,23,24 





### subset data to be one structure:Blanco  ####
#USBR BLANCO R DIVERSION starts 1974-03-31

temp = dat_monthly[dat_monthly$Structure == "USBR BLANCO R DIVERSION",]
#also at lag 10,12 

### subset data to be one structure:	USBR NAVAJO DIVERSION ####
#USBR NAVAJO DIVERSION starts 1974-11-30
temp = dat_monthly[dat_monthly$Structure == "USBR NAVAJO DIVERSION",]
### make this a time series object
## first, make doubly sure that the data is arranged by time before converting to ts object!
temp = temp %>% arrange(date) 
## second, make the spacing of dates consistent and fill in missing obs with NA. This is a handy fxn. You can also create a df of evenly spaced dates and left_join the data to this.
temp_ts =
  temp %>% 
  complete(date = seq(min(date), max(date), by = "1 month"), 
           fill = list(Amount = NA)) %>%
  as_tsibble(index = date)
## finally, convert to a ts object
# a ts object is a vector of data taken sequentially through time. Required arguments are:
# - the data vector
# - the frequency, which is the number of observations per unit of time. Lots of ways to specify this. For monthly data, you can put in 12 and it will assume that's 12 obs in a year. Google for help for other frequencies.
# - the start, which specifies when the first obs occured. Lots of ways to specify this. For monthly data, you can put in c(year, month) and it will know what you mean. 
head (temp_ts)
temp_ts = ts(temp_ts$Amount.mn, frequency=12, start=c(1974, 05)) 
# check that you specified the ts correctly
print(temp_ts, calendar = T) 
### now we're ready to check for temporal autocorrelation in this ts!
# I prefer the forecast pkg's Acf fxn over base R acf() because Acf() doesn't include 0 (which is always 1) and shows month #s by default instead of decimal years. Note the different options for dealing with NAs and how this changes the results (see ?na.fail and ?Acf for details). 
forecast::Acf(temp_ts, na.action = na.pass) 
forecast::Acf(temp_ts, na.action = na.contiguous) 
forecast::Acf(temp_ts, na.action = na.interp)


forecast::Pacf(temp_ts, na.action = na.pass)
forecast::Pacf(temp_ts, na.action = na.contiguous)
forecast::Pacf(temp_ts, na.action = na.interp)

# acf tells me that there is temporal autocorrelation. The sin-wave-like pattern is typical of a ts impacted by seasonality
# pcaf tells me that strongest source of autocorrelation is at lag 1, which indicates a random walk/AR1 process. There is possibly ac at other lags, depending on how NAs are handled. 
#lots of autocorrelation at lag 1,2,5,10,11,12,13,19,20,21,23,24,25

### subset data: All Diversions ####
#USBR NAVAJO DIVERSION starts 1974-11-30
temp = dat_monthly[dat_monthly$Structure,]
### make this a time series object
## first, make doubly sure that the data is arranged by time before converting to ts object!
temp = temp %>% arrange(date) 
## second, make the spacing of dates consistent and fill in missing obs with NA. This is a handy fxn. You can also create a df of evenly spaced dates and left_join the data to this.
temp_ts =
  temp %>% 
  complete(date = seq(min(date), max(date), by = "1 month"), 
           fill = list(Amount = NA)) %>%
  as_tsibble(index = date)
## finally, convert to a ts object
# a ts object is a vector of data taken sequentially through time. Required arguments are:
# - the data vector
# - the frequency, which is the number of observations per unit of time. Lots of ways to specify this. For monthly data, you can put in 12 and it will assume that's 12 obs in a year. Google for help for other frequencies.
# - the start, which specifies when the first obs occured. Lots of ways to specify this. For monthly data, you can put in c(year, month) and it will know what you mean. 
head (temp_ts)
temp_ts = ts(temp_ts$Amount.mn, frequency=12, start=c(1974, 05)) 
# check that you specified the ts correctly
print(temp_ts, calendar = T) 
### now we're ready to check for temporal autocorrelation in this ts!
# I prefer the forecast pkg's Acf fxn over base R acf() because Acf() doesn't include 0 (which is always 1) and shows month #s by default instead of decimal years. Note the different options for dealing with NAs and how this changes the results (see ?na.fail and ?Acf for details). 
forecast::Acf(temp_ts, na.action = na.pass) 
forecast::Acf(temp_ts, na.action = na.contiguous) 
forecast::Acf(temp_ts, na.action = na.interp)


forecast::Pacf(temp_ts, na.action = na.pass)
forecast::Pacf(temp_ts, na.action = na.contiguous)
forecast::Pacf(temp_ts, na.action = na.interp)

# acf tells me that there is temporal autocorrelation. The sin-wave-like pattern is typical of a ts impacted by seasonality
# pcaf tells me that strongest source of autocorrelation is at lag 1, which indicates a random walk/AR1 process. There is possibly ac at other lags, depending on how NAs are handled. 
#lots of autocorrelation at lag 1,2,5,10,11,12,13,19,20,21,23,24,25

#### #####

