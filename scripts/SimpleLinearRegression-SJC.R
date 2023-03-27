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

SWSI_CORG <- AllSWSI %>% #Creating dfs by basin
  select(Date,Rio_Grande,Colorado) %>% 
   pivot_longer(cols = "Rio_Grande":"Colorado", 
                names_to = "basin",
                values_to = "SWSI")
               
?pivot_longer
View(SWSI_CORG)
#### Azotea Tunnel and both SWSI####

Azotea_CO_RG <- full_join(AzoteaDiversions,SWSI_CORG, by = "Date") #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI

## plot the data ###
Discharge <- AzoteaDiversions
SWSI <- SWSI_RG

CombinedData %>%
  ggplot(aes(x = Discharge, y = SWSI)) + 
  geom_point()

# create the linear model
CombinedData <- Azotea_CO_RG
linearmodel <- lm(Discharge ~ SWSI * basin, data = CombinedData, na.action=na.omit) 
plot(linearmodel) #Azotea and RG SWSI model 

# run type 3 ANOVA
Anova(linearmodel, type = 3)

m1 <- lm(Discharge ~ SWSI * basin, data = CombinedData, na.action=na.omit) #does not fit
m2 <- lm(Discharge ~ SWSI + basin, data = CombinedData, na.action=na.omit) #does not fit
m3 <- lm(Discharge ~ basin, data = CombinedData, na.action=na.omit) #does not fit Vertical datas? 
m4 <- lm(Discharge ~ SWSI, data = CombinedData, na.action=na.omit) #does not fit 
null <- lm(Discharge ~  1, data = CombinedData, na.action=na.omit) #does not fit 

plot(m1)
plot(m2)
plot(m3)
plot(m4)
plot(null)

AICc(m1, m2, m3, m4, null)
#
#df     AICc
#df     AICc
#m1    5 18910.86
#m2    4 18909.40
#m3    3 18922.05
#m4    3 18907.39
#null  2 21694.47

anova(m1, m4)
#I'm not sure what this means? 

#post-hoc test
CombinedData %>%
  select(basin, Discharge, SWSI) %>%
  drop_na() %>%
  ggplot(aes(x = SWSI, y = Discharge)) + 
  geom_boxplot() + facet_grid(~basin)

# tukey test 
emmeans(m1, pairwise ~ SWSI | basin)




#### Azotea Tunnel and Rio Grande SWSI####

Azotea_RG <- full_join(AzoteaDiversions,SWSI_RG, by = "Date") #Combining SWSI by basin with diversion data, Azotea Tunnel, RG SWSI

#Set dataframes 
Discharge <- AzoteaDiversions
CombinedData <- Azotea_RG
SWSI <- SWSI_RG

## plot the data ###
CombinedData %>%
  ggplot(aes(x = Discharge, y = SWSI)) + 
  geom_point()

# create the linear model
linearmodel <- lm(Discharge ~ SWSI, data = CombinedData, na.action=na.omit) 
plot(linearmodel) #Azotea and RG SWSI model 

# run type 3 ANOVA
Anova(linearmodel, type = 3)

m1 <- lm(Discharge ~ SWSI * basin, data = CombinedData, na.action=na.omit) #does not fit
m2 <- lm(Discharge ~ SWSI + basin, data = CombinedData, na.action=na.omit) #does not fit
m3 <- lm(Discharge ~ basin, data = CombinedData, na.action=na.omit) #does not fit
m4 <- lm(Discharge ~ SWSI, data = CombinedData, na.action=na.omit) #does not fit 
null <- lm(Discharge ~  1, data = CombinedData, na.action=na.omit) #does not fit 

plot(m1)
plot(m2)
plot(m3)
plot(m4)
plot(null)

AICc(m1, m2, m3, m4, null)
#
#df     AICc
#m1   15 285500.2
#m2    9 285499.5
#m3    8 285612.2
#m4    3 285488.9 **
#null  2 291178.0

anova(m1, m4)
#I'm not sure what this means? 

#post-hoc test
CombinedData %>%
  select(basin, Amount, SWSI) %>%
  drop_na() %>%
  ggplot(aes(x = SWSI, y = Amount)) + 
  geom_boxplot() + facet_grid(~basin)


# tukey test comparing species for females and for males
emmeans(m1, pairwise ~ SWSI | basin)
#Doesn't calculate :( 



#### Azotea Tunnel and Colorado SWSI  ####

Azotea_CO <- full_join(AzoteaDiversions,SWSI_Colorado, by = "Date") #Combining SWSI by basin with diversion data, Azotea Tunnel, CO SWSI

#Set dataframes 
Discharge <- AzoteaDiversions
CombinedData <- Azotea_CO
SWSI <- SWSI_Colorado

## plot the data ###
CombinedData %>%
  ggplot(aes(x = Discharge, y = SWSI)) + 
  geom_point()

# create the linear model
linearmodel <- lm(Discharge ~ SWSI, data = CombinedData, na.action=na.omit) 
plot(linearmodel) #Azotea and CO SWSI model 


#### Heron Reservoir and Rio Grande SWSI  ####
Heron_RG <- full_join(HeronReleases,SWSI_RG, by = "Date") #Combining SWSI by basin with diversion data - Heron Reservor, RG SWSI

#Set dataframes 
Discharge <- HeronReleases
CombinedData <- Heron_RG
SWSI <- SWSI_RG

## plot the data ###
CombinedData %>%
  ggplot(aes(x = Discharge, y = SWSI)) + 
  geom_point()

# create the linear model
linearmodel <- lm(Discharge ~ SWSI, data = CombinedData, na.action=na.omit) 
plot(linearmodel) #Heron and RG SWSI

#### Heron Reservoir and Colorado SWSI  ####
Heron_CO <- full_join(HeronReleases,SWSI_Colorado, by = "Date") #Combining SWSI by basin with diversion data - Heron Res, CO SWSI

#Set dataframes 
Discharge <- HeronReleases
CombinedData <- Heron_CO
SWSI <- SWSI_Colorado

## plot the data ###
CombinedData %>%
  ggplot(aes(x = Discharge, y = SWSI)) + 
  geom_point()

# create the linear model
linearmodel <- lm(Discharge ~ SWSI, data = CombinedData, na.action=na.omit) 
plot(linearmodel) #Heron and COlorado SWSI

