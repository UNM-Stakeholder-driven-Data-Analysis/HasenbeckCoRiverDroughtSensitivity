####Read me #### 

#This script first cleans and then combines two SWSI datasets.

#libraries 
library(tidyverse)
library(dplyr)
library(lubridate)

SWSI1981to2011 <- read.csv("data/raw/SWSI1981to2011.csv")
SWSI2010tonow <- read.csv("data/raw/SWSI2010tonow.csv")


####Cleaning SWSI 2010 to now data####
  
SWSI2010tonowCLEAN <- SWSI2010tonow %>%
  select("basin","report_year","report_month","swsi") %>% #select only columns of interest
  pivot_wider(names_from = "basin", values_from = "swsi") %>% #pivot to a wider table to match format of past data
  mutate(Date_Recorded = paste(report_year,report_month,"1", sep = "-")) %>%  #combine year and month columns into one date, add 01 as day
  select(!(report_year:report_month)) #remove year and month columns

#### Joining 2010 and 1981 datasets####
SWSIjoin <- full_join(SWSI1981to2011,
           SWSI2010tonowCLEAN,
          join_by = (
            "Date" = "Date_Recorded" +
            "Gunnison" = "Gunnison" +
            "Colorado" ="Colorado" +
            "Arkansas" = "Arkansas"+
            "South.Platte" = "South Platte" +
            "Yampa..White...N..Platte" = "Yampa-White" +
            "Rio.Grande" = "Rio Grande" +
            "San.Juan..Animas..Dolores...San.Miguel" = "San Juan-Dolores"+
            keep = FALSE)
)
          
SWSI1981to2023 <- SWSIjoin  %>%
  #Merge columns
  unite(Date, "Date","Date_Recorded", na.rm = TRUE) %>%
  unite(Colorado, "Colorado", "Colorado", na.rm = TRUE) %>%
  unite(Gunnison, "Gunnison", na.rm = TRUE) %>%
  unite(Arkansas, "Arkansas", na.rm = TRUE) %>%
  unite(South_Platte, "South Platte", "South.Platte", na.rm = TRUE) %>%
  unite(Yampa_White_N_Platte, "Yampa..White...N..Platte", "Yampa-White", na.rm = TRUE) %>%
  unite(Rio_Grande, "Rio.Grande", "Rio Grande", na.rm = TRUE) %>%
  unite(San_Juan, "San.Juan..Animas..Dolores...San.Miguel", "San Juan-Dolores", na.rm = TRUE) %>% 
  #remove un-needed X column 
  select(!X)

###change data types ###
#change dates to dates 
  SWSI1981to2023$Date = as.Date(as.character(SWSI1981to2023$Date,format = "Y!-m!-d!"))
#change basin values to values 
  SWSI1981to2023$Gunnison = as.numeric(SWSI1981to2023$Gunnison)
  SWSI1981to2023$Colorado= as.numeric(SWSI1981to2023$Colorado)
  SWSI1981to2023$South_Platte = as.numeric (SWSI1981to2023$South_Platte)
  SWSI1981to2023$Yampa_White_N_Platte = as.numeric(SWSI1981to2023$Yampa_White_N_Platte) 
  SWSI1981to2023$Arkansas = as.numeric(SWSI1981to2023$Arkansas)
  SWSI1981to2023$Rio_Grande = as.numeric(SWSI1981to2023$Rio_Grande)  
  SWSI1981to2023$San_Juan = as.numeric(SWSI1981to2023$San_Juan)


str(SWSI1981to2023)


#HAVE DUPLICATE VALUES FOR PERIOD IN 2011 


####Data exploration ####

#Pivot longer to complete data exploration: 
SWSIdataexplore <- pivot_longer(SWSI1981to2023,
             cols = Gunnison:San_Juan,
            cols_vary = "fastest",
            names_to = "basin",
            values_to = "SWSI",
            values_drop_na = FALSE)

#tibble converted to df 

SWSIdataexplore <- as.data.frame(SWSIdataexplore)
  
  

#convert new column to factor 
SWSIdataexplore$basin = as.factor(SWSIdataexplore$basin)


  
View(SWSIdataexplore)
                    
#### describe dataset size and structure ####

head(SWSIdataexplore)
#Date, single SWSI observation sorted by basin. 

str(SWSIdataexplore)
#parameters were converted to date, numerics and factors. 
#Value is the measure of the SWSI
#Total observations: 3570
#Total number of variables: 3 (date, basins, SWSI value)

with(dat, table(Parameter, Site_Name))





### check timesteps by looking and time series of most frequently collected parameters
# make dataset of one of the most frequently collected parameters
dat_lts = 
  dat %>% 
  group_by(Parameter) %>% 
  filter(n() > 500) %>% 
  arrange(datetime_NM)
dat_lts_alk = 
  dat %>% 
  filter(Parameter=="Alkalinity") %>% 
  arrange(datetime_NM)
# add year and day of year for plotting
dat_lts_alk$year = lubridate::year(dat_lts_alk$datetime_NM)
dat_lts_alk$doy = lubridate::yday(dat_lts_alk$datetime_NM)
# plot
ggplot(data=dat_lts_alk, aes(x=doy, y=Value, color=Site_Name))+
  geom_point() + geom_path()+
  facet_wrap(~year, scales="free_y")+
  theme(legend.title = element_blank()) +
  theme_bw()
# can also look at single years in detail
ggplot(data=dat_lts_alk, aes(x=datetime_NM, y=Value, color=Site_Name))+
  geom_point() + geom_path()+
  xlim(c(as.POSIXct("1995-01-01"), as.POSIXct("1996-01-01")))+
  theme(legend.title = element_blank()) +
  theme_bw()
# timesteps are all over the place from year to year and site to site, what we would call "irregular"
# timesteps are not sub-daily, at most frequent are approximately monthly

# ........ etc. for each parameter I'm interested in using in analysis .........


### How many variables are in your dataset?
str(SWSIdataexplore)
# 3: date, basin, SWSI

### How many observations are in your dataset?
nrow(SWSIdataexplore)
#3570 total

### Are the data nested in time or space?
# Yes in time - observations were collected repeatedly on an irregular schedule
# Yes in space - observations were collected in three different sites, need more research/exploration to find out if sites are connected in any way

#### describe data types ####

str(dat)
summary(dat$Parameter)
# most water quality parameters are numerical continous ratios
# temp, pH are numerical continous interval
# I would need to research on how some of these other parameters were measured and what units they're in to decide whether they're ratio or interval - take time to do this for your dataset!

#### check distributions ####

# I'm only going to check the distributions of data with at least 100 obs in each site, as I am unlikely to analyze less frequently gathered data
dat_r = 
  dat %>% 
  group_by(Parameter, Site_Name) %>% 
  filter(n() > 100) %>% 
  arrange(datetime_NM)
summary(dat_r$Parameter)

temp = dat_r[dat_r$Parameter == "Alkalinity",]
qqPlot(temp$Value); shapiro.test(temp$Value) # normal
qqPlot(temp$Value[temp$Site_Name=='VR-1']); shapiro.test(temp$Value[temp$Site_Name=='VR-1']) # no data
qqPlot(temp$Value[temp$Site_Name=='VR-2']); shapiro.test(temp$Value[temp$Site_Name=='VR-2']) # normal
qqPlot(temp$Value[temp$Site_Name=='VR-3']); shapiro.test(temp$Value[temp$Site_Name=='VR-3']) # normal

temp = dat_r[dat_r$Parameter == "Ammonia",]
qqPlot(temp$Value); shapiro.test(temp$Value) # NOT normal
qqPlot(temp$Value[temp$Site_Name=='VR-1']); shapiro.test(temp$Value[temp$Site_Name=='VR-1']) # no data
qqPlot(temp$Value[temp$Site_Name=='VR-2']); shapiro.test(temp$Value[temp$Site_Name=='VR-2']) # NOT normal
qqPlot(temp$Value[temp$Site_Name=='VR-3']); shapiro.test(temp$Value[temp$Site_Name=='VR-3']) # NOT normal

temp = dat_r[dat_r$Parameter == "Bicarbonate",]
qqPlot(temp$Value); shapiro.test(temp$Value) # normal
qqPlot(temp$Value[temp$Site_Name=='VR-1']); shapiro.test(temp$Value[temp$Site_Name=='VR-1']) # no data
qqPlot(temp$Value[temp$Site_Name=='VR-2']); shapiro.test(temp$Value[temp$Site_Name=='VR-2']) # normal
qqPlot(temp$Value[temp$Site_Name=='VR-3']); shapiro.test(temp$Value[temp$Site_Name=='VR-2']) # normal

# etc........ for the rest of the parameters that I think I'll use in this analysis


    
    
