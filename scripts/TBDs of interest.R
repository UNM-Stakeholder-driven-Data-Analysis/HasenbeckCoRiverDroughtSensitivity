
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

print(TBDs_of_Interest$Structure_WDID_CSV) # getting relevant water ids


#### Reading in data from diversions #### 
#Total diversion = amount of water entering structure
#Total release = water released from structure under dominion and control 

#CBT: Adams Tunnel: 5104634 (pipe) 5104634 exports transmountain water to 0404634 - ADAMS TUNNEL. Only has diversions. 
ADAMS_TUNNEL_Diversions <- read_csv("~/OneDrive - University of New Mexico/Spring 2023/Stakeholder Analysis/HasenbeckCoRiverDroughtSensitivity/data/raw/CBT ALVA B ADAMS TUNNEL_5104634_Diversions.csv")
#0404634 (tunnel) This has diversions and releases because it is what the pipe dumps into. NO DATA PRE 1994. Removing from analyis and using only 5104634. 
ADAMS_TUNNEL_0404634_Diversions_Releases <- read_csv("~/OneDrive - University of New Mexico/Spring 2023/Stakeholder Analysis/HasenbeckCoRiverDroughtSensitivity/data/raw/ADAMS TUNNEL_0404634_Diversions_Releases.csv")


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

#Renaming columns
AllTBDiversions <- 
  rename(AllTBDiversions, 
       "Observation_Code" = "Obs Code",
       "Structure" = "Structure Name", 
       "Status" = "Data Status"
       )

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

str(dat)
summary(dat$Structure)
# diversions are numerical continous interval

#### check distributions: CBT is normal  ####

dat_r = 
  dat %>% 
  group_by(Structure) %>% 
  arrange(Date)

temp = dat_r[dat_r$Structure == "USBR BLANCO R DIVERSION",]
qqPlot(temp$Amount); shapiro.test(temp$Amount) # NOT normal - right skewed


temp = dat_r[dat_r$Structure == "FRY ARK PR BOUSTEAD TUNNEL",]
qqPlot(temp$Amount); shapiro.test(temp$Amount) # NOT normal - right skewed 


temp = dat_r[dat_r$Structure == "USBR NAVAJO DIVERSION",]
qqPlot(temp$Amount); shapiro.test(temp$Amount) # NOT normal - right skewed 

temp = dat_r[dat_r$Structure == "CBT ALVA B ADAMS TUNNEL",]
qqPlot(temp$Amount); shapiro.test(temp$Amount) # normal

# etc........ for the rest of the Structures that I think I'll use in this analysis

### Examine non-normal data closely ###
# ask:
# are outliers making it non-normal?
# can I justify removing outliers based on my knowledge of the data?
# if data is still non-normal, what distribution is it?

temp = dat_r[dat_r$Structure == "Ammonia",]
summary(temp$Amount)
hist(temp$Amount)
plot(density(temp$Amount))
# this data has 1 an extreme negative outlier. Ammonia Amounts cannot be negative, so this is an error. I will remove it in the main datasets an re-check the data's normality
dat$Amount[dat$Structure=="Ammonia" & dat$Amount<0] = NA # rplace it in main dataset
dat_r$Amount[dat_r$Structure=="Ammonia" & dat_r$Amount<0] = NA # replace it in reduced dataset
temp = dat_r[dat_r$Structure == "Ammonia",]
qqPlot(temp$Amount); shapiro.test(temp$Amount)
# there is now a high outlier to examine
temp = dat_r[dat_r$Structure == "Ammonia",]
summary(temp$Amount)
hist(temp$Amount)
plot(density(temp$Amount, na.rm = T))
# this data has 1 an extreme positive outlier. Ammonia Amounts do not get this high in natural conditions. This is coal data so maybe it isn't natural, but even still, we'd expect to see more than one point if this were not an error. I will remove it in the main datasets an re-check the data's normality
dat$Amount[dat$Structure=="Ammonia" & dat$Amount>11] = NA # rplace it in main dataset
dat_r$Amount[dat_r$Structure=="Ammonia" & dat_r$Amount>11] = NA # replace it in reduced dataset
temp = dat_r[dat_r$Structure == "Ammonia",]
qqPlot(temp$Amount); shapiro.test(temp$Amount)
hist(temp$Amount)
plot(density(temp$Amount, na.rm = T))
range(temp$Amount, na.rm = T)
# still not normal!
# this looks like a lognormal, Gamma, or Weibull distribution
# it is bounded above zero and is right-skewed
# what happens if I log-transform it?
temp = dat_r[dat_r$Structure == "Ammonia",]
qqPlot(log10(temp$Amount)); shapiro.test(log10(temp$Amount))

# a log10 transformation did the trick! That tells me that it is lognormal. I will note in my report that a log10 transformation is a possible option if my models don't meet assumptions.
# Also note the stair-steps in the data at lower Amounts. This could result from detection limits where the low Amount was replaced with a standard Amount. It shouldn't be a huge problem, but it is worth noting as a thing to investigate if the analyses don't turn out well. 


#### check for temporal autocorrelation ####

# I'm going to check these one site at a time and only of data with at least 100 obs in each site, as I am unlikely to analyze less frequently gathered data
dat_r = 
  dat %>% 
  group_by(Structure, Structure) %>% 
  filter(n() > 100) %>% 
  arrange(datetime_NM)
summary(dat_r$Structure) # note which Structures are left after filtering
summary(dat_r$Structure) # note that VR-1 no longer has any observations, so I will focus on the other two sites

# checking for temporal autocorrelation requires the data to be a time series object (read ?ts for details on this)
# To achieve this, I need regularly spaced data. This data is irregularly spaced, approximately monthly, but sometimes there are more than one observations per month or fewer
# I will start by averaging observations within the same month:
dat_monthly = 
  dat_r %>%
  mutate(yr = lubridate::year(datetime_NM)) %>%
  mutate(mo = lubridate::month(datetime_NM)) %>%
  dplyr::select(Structure, Structure, yr, mo, Amount) %>%
  group_by(Structure, Structure, yr, mo) %>%
  summarise(Amount.mn = mean(Amount, na.rm = T)) %>%
  mutate(date = paste(yr, mo, "15", sep="-")) %>%
  mutate(date = as.Date(date))


#### Alkalinity in VR-2
### subset data to be one site and one Structure
temp = dat_monthly[dat_monthly$Structure == "Alkalinity" & dat_monthly$Structure=="VR-2" ,]
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
temp_ts = ts(temp_ts$Amount.mn, frequency=12, start=c(1979, 10)) 
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


#### Alkalinity in VR-3
### subset data to be one site and one Structure
temp = dat_monthly[dat_monthly$Structure == "Alkalinity" & dat_monthly$Structure=="VR-3" ,]
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
temp_ts = ts(temp_ts$Amount.mn, frequency=12, start=c(1983, 1)) 
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


# ....... ect. for each Structure and site combination I might include in the analysis .......



#### check for spatial autocorrelation ####

# I'm interested in spatial and not temporal autocorrelation, so I am going to look at just a few observations across all sites

# reload and format data with all sites
dat_all = read.csv("Week 5/coal_WQ.csv")
dat_all$datetime_NM = as.POSIXct(dat_all$Sample_DateTime, format="%m/%d/%y %H:%M", tz="MST")
dat_all$Structure = as.factor(dat_all$Structure)
dat_all$Structure = as.factor(dat_all$Structure)
dat_all$Is_Nondetect = as.factor(dat_all$Is_Nondetect)
dat_all$Amount = as.numeric(dat_all$Amount)
# how many sites are there?
length(unique(dat_all$Structure))
# 1288

# what Structures were collected across all sites in June 1995?
dat_june1995 = dat_all[dat_all$datetime_NM >= as.POSIXct("1995-06-01") &
                         dat_all$datetime_NM < as.POSIXct("1995-07-01"),]
tb = as.data.frame( with(dat_june1995, table(Structure, Structure)) )
tb = tb[tb$Freq>0,]
tb2 = tb %>% group_by(Structure) %>% summarise(n = n()) %>% arrange(desc(n))
head(tb2, 15)
#   Structure          # of sites it was collected at in June 1995
# 1 Bicarbonate        82
# 2 Calcium            82
# 3 Chloride           82
# 4 LabpH              82
# 5 LabTDS             82
# 6 Magnesium          82
# 7 Potassium          82
# 8 Sodium             82
# 9 Sulfate            82
# ^ these are good options for testing for spatial autocorrelation

### Bicarbonate in June 1995
dat_june1995 = dat_all[dat_all$datetime_NM >= as.POSIXct("1995-06-01") &
                         dat_all$datetime_NM < as.POSIXct("1995-07-01"),]
temp = dat_june1995 %>%  filter(Structure=="Bicarbonate")
# randomly generate lat/lon for demo
set.seed(42)
temp$lat = runif(nrow(temp),35.090956,35.634117)
temp$lon = runif(nrow(temp),-107.65829,-106.65829)
## Moran.I
# generate an inverse distance matrix 
dists = as.matrix(dist(cbind(temp$lon, temp$lat)))
dists.inv = 1/dists
diag(dists.inv) = 0
# calculate Moran.I
Moran.I(temp$Amount, dists.inv)
# we can NOT reject the null hypothesis that there is zero spatial autocorrelation present. In other words, there doesn't seem to be a lot of spatial autocorrelation. 
## Mantel test
# generate spatial distance matrix
site_dists = dist(cbind(temp$lon, temp$lat))
# generate response distance matrix 
resp_dists = dist(temp$Amount)
# run Mantel test
mantel.rtest(site_dists, resp_dists, nrepet = 9999)
# 'observation' is the correlation between the distance matrices
# p Amount suggests that they are NOT correlated
# So, based on this test, there is no detectable correlation
## Map
proj = CRS("+proj=longlat +datum=WGS84")
temp_spatial  <- SpatialPointsDataFrame(coords= cbind(temp$lon, temp$lat),
                                        data = as.data.frame(cbind(temp$Structure, temp$Amount)),
                                        proj4string = proj)
plot(temp_spatial)

# ect.......... for other Structures of interest and for a few other time points, depending on how your data is structured ...........

#
#### check correlation between variables ####

# first, returning to the dataset of just 3 sites and more than 100 obs per Structure (dat_r), reformat data to make it wider, such that Structures get their own columns. 

dat_r_long = dat_r %>% 
  select(c(Structure, datetime_NM, Structure, Amount))%>%
  group_by(Structure, datetime_NM, Structure) %>%
  summarise(Amount = mean(Amount, na.rm = T)) %>%
  pivot_wider(names_from = Structure, 
              Amounts_from = Amount,
              Amounts_fill = list(Amount = NA))

# reduce data down to one site - VR-2
temp = dat_r_long %>% filter(Structure=="VR-2") 
# plot correlations (of data columns only)
pairs.panels(temp[,3:24], scale=T)
pairs.panels(temp[,3:24], scale=F)
# make table of correlations (I am rounding and replacing low Amounts with text so that it is easier to see results)
tab = round(as.data.frame(cor(cov(temp[,3:24], use="na.or.complete"))), 2)
tab[abs(tab)<0.4] = "no_corr"

# reduce data down to one site - VR-3
temp = dat_r_long %>% filter(Structure=="VR-3")
# plot correlations (of data columns only)
pairs.panels(temp[,3:24], scale=T)
pairs.panels(temp[,3:24], scale=F)
# make table of correlations (I am rounding and replacing low Amounts with text so that it is easier to see results)
tab = round(as.data.frame(cor(cov(temp[,3:24], use="na.or.complete"))), 2)
tab[abs(tab)<0.4] = "no_corr"





