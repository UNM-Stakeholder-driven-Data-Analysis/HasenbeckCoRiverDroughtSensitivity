####READ ME#### 
#THis model tests hurdle and Poisson ZIP models to id which is appropriate for further exploration using data from the SJC project. 


####Libraries#### 
library(readr)
library(tidyverse)
library(lubridate)

library(AER) #Poisson Model Package
library(pscl) #Hurdle Model package 
# For Hurdle Plotting: Need to install from R-Forge instead of CRAN
install.packages("countreg", repos="http://R-Forge.R-project.org")
library(countreg)

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

SWSI_Colorado <- AllSWSI %>% #Creating dfs by basin
  dplyr::select(Date,Colorado) %>% 
  rename(SWSI = Colorado) #rename column to logical name 

SWSI_RG <- AllSWSI %>% #Creating dfs by basin
  dplyr::select(Date,Rio_Grande) %>%
  rename(SWSI = Rio_Grande) #rename column to logical name

####Azotea and both SWSI Poisson ####
#resource and code: https://data.library.virginia.edu/getting-started-with-hurdle-models/

Azotea_CO_RG <- full_join(AzoteaDiversions,SWSI_CORG, by = "Date") #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI


CombinedData <- Azotea_CO_RG
CombinedData$Discharge = as.integer(CombinedData$Discharge)

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
mod.hurdle.nb <- hurdle(Discharge ~ ., data = CombinedData, dist = "negbin")
rootogram(mod.hurdle.nb) #plot

AIC(mod.hurdle)
#[1] 6519198
AIC(mod.hurdle.nb) # lower is better
#[1] 14525.27

mod.hurdle.nb2 <- hurdle(Discharge ~ . | basin + SWSI, 
                         data = CombinedData, dist = "negbin")
rootogram(mod.hurdle.nb2)

#### Azotea and RG SWSI Hurdle #### 
mod.hurdle <- hurdle(Discharge ~ SWSI, data = CombinedData)

summary(mod.hurdle)
sum(predict(mod.hurdle, type = "prob",na.rm=T)[,1])
#[1] 140

sum(CombinedData$Discharge < 1, na.rm=T)
#156

# First 5 expected counts
predict(mod.hurdle, type = "response")[1:5]

rootogram(mod.hurdle) # plot

#negative bionomial distribution: 
mod.hurdle.nb <- hurdle(Discharge ~ ., data = CombinedData, dist = "negbin")
rootogram(mod.hurdle.nb) #plot

AIC(mod.hurdle)
#[1] 6519198
AIC(mod.hurdle.nb) # lower is better
#[1] 14525.27

mod.hurdle.nb2 <- hurdle(Discharge ~ . | basin + SWSI, 
                         data = CombinedData, dist = "negbin")
rootogram(mod.hurdle.nb2)
