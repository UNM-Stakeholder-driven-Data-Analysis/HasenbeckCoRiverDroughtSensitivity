####READ ME#### 
#THis model tests hurdle and Poisson ZIP models to id which is appropriate for further exploration using data from the SJC project. 


####Libraries#### 
library(readr)
library(tidyverse)
library(lubridate)

library(AER) #Poisson Model Package
library(pscl) #Hurdle and ZIP Model package 
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

SWSI_CORG$SWSI = as.numeric(SWSI_CORG$SWSI) # Set data type


SWSI_Colorado <- AllSWSI %>% #Creating dfs by basin
  dplyr::select(Date,Colorado) %>% 
  rename(SWSI = Colorado) #rename column to logical name 

SWSI_Colorado$SWSI = as.numeric(SWSI_Colorado$SWSI) # Set data type

SWSI_RG <- AllSWSI %>% #Creating dfs by basin
  dplyr::select(Date,Rio_Grande) %>%
  rename(SWSI = Rio_Grande) #rename column to logical name
SWSI_RG$SWSI = as.numeric(SWSI_RG$SWSI) # Set data type

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

