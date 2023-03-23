#### Read me ####

#This script prepares San Juan Chama data to be analyzed in a simpler linear regression, then plots it. 

####Libraries#### 
library(readr)
library(tidyverse)
library(lubridate)

library(lme4) # for creating mixed models
library(car) # for Anova(), vif()
library(MuMIn) # for AICc
library(emmeans) # for emmeans, emtrends, all the post hoc tests and plotting

####Read in data#### 

AzoteaDiversions <- read_csv(file = "data/processed/AzoteaMonthlyDischarge")
HeronReleases <- read_csv(file="data/processed/HeronMonthlyReleases") %>% 
  rename(Discharge = Release) #Renaming columns to same name as header in Tunnel data so I can recycle code more easily later
AllSWSI <- read_csv(file= "data/processed/SWSI1981to2023.csv")


#### Prepping Data for Regression #### 

SWSI_Colorado <- AllSWSI %>% #Creating dfs by basin
  select(Date,Colorado) %>% 
  rename(SWSI = Colorado) #rename column to logical name 

SWSI_RG <- AllSWSI %>% #Creating dfs by basin
  select(Date,Rio_Grande) %>%
  rename(SWSI = Rio_Grande) #rename column to logical name


Azotea_RG <- full_join(AzoteaDiversions,SWSI_RG, by = "Date") #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI
Azotea_CO <- full_join(AzoteaDiversions,SWSI_Colorado, by = "Date") #Combining SWSI by basin with diversion data, Azotea Tunnel, CO SWSI
Heron_RG <- full_join(HeronReleases,SWSI_RG, by = "Date") #Combining SWSI by basin with diversion data - Heron Reservor, RG SWSI
Heron_CO <- full_join(HeronReleases,SWSI_Colorado, by = "Date") #Combining SWSI by basin with diversion data - Heron Res, CO SWSI


#### Azotea Tunnel and Rio Grande SWSI ALEX LOOK HERE THIS IS THE AREA WHERE I NEED HELP ####

#Set dataframes 
Discharge <- AzoteaDiversions
CombinedData <- Azotea_RG
SWSI <- SWSI_RG

## plot the data ###
CombinedData %>%
  ggplot(aes(x = Discharge, y = SWSI)) + 
  geom_point()

# create the linear model
linearmodel <- lm(Discharge ~ SWSI, data = CombinedData, na.action=na.omit) ####I AM MIXED UP ON MY FORMULA HERE!! 
plot(linearmodel) 

#Code from other linear model script: linearmodel <- lm(Amount ~ SWSI * basin, data = CombinedData, na.action=na.omit)

#### Azotea Tunnel and Colorado SWSI  ####

#Set dataframes 
Discharge <- AzoteaDiversions
CombinedData <- Azotea_CO
SWSI <- SWSI_Colorado

## plot the data ###
CombinedData %>%
  ggplot(aes(x = Discharge, y = SWSI)) + 
  geom_point()

# create the linear model



#### Heron Reservoir and Rio Grande SWSI  ####

#Set dataframes 
Discharge <- HeronReleases
CombinedData <- Heron_RG
SWSI <- SWSI_RG

## plot the data ###
CombinedData %>%
  ggplot(aes(x = Discharge, y = SWSI)) + 
  geom_point()

# create the linear model
#### Heron Reservoir and Colorado SWSI  ####

#Set dataframes 
Discharge <- HeronReleases
CombinedData <- Heron_CO
SWSI <- SWSI_Colorado

## plot the data ###
CombinedData %>%
  ggplot(aes(x = Discharge, y = SWSI)) + 
  geom_point()

# create the linear model