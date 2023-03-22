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
HeronReleases <- read_csv(file="data/processed/HeronMonthlyReleases")
SWSI <- read_csv(file= "data/processed/SWSI1981to2023.csv")

#### Prepping SWSI Data #### 

SWSI_Colorado <- SWSI %>% 
  select(Date,Colorado)

SWSI_RG <- SWSI %>% 
  select(Date,Rio_Grande)



