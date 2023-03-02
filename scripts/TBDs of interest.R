
####Read me#### 

#the purpose of this script is to import and clean diversion data for three largest (by annual average AF diverted) transmountain diversions in three different basins: 
#The adams tunnel/CBT (Colorado to South Platte)
#San Juan Chama Project (Colorado to Rio Grande)
# Boustead Tunnel (Colorado to Arkansas)

# These diversions were selected because of their size, and to allow analysis in three different beneficiary basins.  


#### libraries ####
library(readr)
library(tidyverse)



#### Read data ####
Colorado_Transbasin_Diversions <- read_csv("~/OneDrive - University of New Mexico/Spring 2023/Stakeholder Analysis/HasenbeckCoRiverDroughtSensitivity/data/raw/Colorado-Transbasin-Diversions.csv")

#Select only three relevant diversions: 
TBDs_of_Interest = Colorado_Transbasin_Diversions[Colorado_Transbasin_Diversions$TransbasinDiversionName 
                                                 %in% c("Charles H. Boustead Tunnel","San Juan-Chama Project","Colorado-Big Thompson Project"), ]

print(TBDs_of_Interest$Structure_WDID_CSV) # getting relevant water ids



#Total diversion = amount of water entering structure
#Total release = water released from structure under dominion and control 

#CBT: Adams Tunnel: 5104634 (pipe), 0404634 (tunnel) 
ADAMS_TUNNEL_5104634_Diversions <- read_csv("~/OneDrive - University of New Mexico/Spring 2023/Stakeholder Analysis/HasenbeckCoRiverDroughtSensitivity/data/raw/CBT ALVA B ADAMS TUNNEL_5104634_Diversions.csv")
ADAMS_TUNNEL_0404634_Diversions_Releases <- read_csv("~/OneDrive - University of New Mexico/Spring 2023/Stakeholder Analysis/HasenbeckCoRiverDroughtSensitivity/data/raw/ADAMS TUNNEL_0404634_Diversions_Releases.csv")

#Fry-Ark: 3804625 Boustead Tunnel
BOUSTEAD_3804625_Diversions <- read_csv("~/OneDrive - University of New Mexico/Spring 2023/Stakeholder Analysis/HasenbeckCoRiverDroughtSensitivity/data/raw/FRY ARK PR BOUSTEAD TUNNEL_3804625_Diversions.csv")

#SJC: 7704636,Blanco diversion, #Navajo 7704635
NAVAJO_DIVERSION_7704635 <- read_csv("~/OneDrive - University of New Mexico/Spring 2023/Stakeholder Analysis/HasenbeckCoRiverDroughtSensitivity/data/raw/USBR NAVAJO DIVERSION_7704635_Diversions_Monthly.csv")
BLANCO_DIVERSION_2904667 <- read_csv("~/OneDrive - University of New Mexico/Spring 2023/Stakeholder Analysis/HasenbeckCoRiverDroughtSensitivity/data/raw/USBR BLANCO R DIVERSION_2904667_Diversions.csv")


